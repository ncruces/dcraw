/*
   dcraw.c -- Dave Coffin's raw photo decoder
   Copyright 1997-2004 by Dave Coffin, dcoffin a cybercom o net

   This is a portable ANSI C program to convert raw image files from
   any digital camera into PPM format.  TIFF and CIFF parsing are
   based upon public specifications, but no such documentation is
   available for the raw sensor data, so writing this program has
   been an immense effort.

   This code is freely licensed for all uses, commercial and
   otherwise.  Comments, questions, and encouragement are welcome.

   $Revision: 1.209 $
   $Date: 2004/10/07 01:11:51 $
 */

#define _GNU_SOURCE
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
/*
   By defining NO_JPEG, you lose only the ability to
   decode compressed .KDC files from the Kodak DC120.
 */
#ifndef NO_JPEG
#include <jpeglib.h>
#endif

#ifdef WIN32
#include <winsock2.h>
#pragma comment(lib, "ws2_32.lib")
#define strcasecmp stricmp
typedef __int64 INT64;
#else
#include <unistd.h>
#include <netinet/in.h>
typedef long long INT64;
#endif

#ifdef LJPEG_DECODE
#error Please compile dcraw.c by itself.
#error Do not link it with ljpeg_decode.
#endif

#ifndef LONG_BIT
#define LONG_BIT (8 * sizeof (long))
#endif

#define ushort UshORt
typedef unsigned char uchar;
typedef unsigned short ushort;

/*
   All global variables are defined here, and all functions that
   access them are prefixed with "CLASS".  Note that a thread-safe
   C++ class cannot have non-const static local variables.
 */
FILE *ifp;
short order;
char *ifname, make[64], model[64], model2[64];
time_t timestamp;
int data_offset, curve_offset, curve_length;
int tiff_data_compression, kodak_data_compression;
int raw_height, raw_width, top_margin, left_margin;
int height, width, colors, black, rgb_max;
int iheight, iwidth, shrink;
int is_canon, is_cmy, is_foveon, use_coeff, trim, flip, xmag, ymag;
int zero_after_ff;
unsigned filters;
ushort (*image)[4], white[8][8];
void (*load_raw)();
float gamma_val=0.6, bright=1.0, red_scale=1.0, blue_scale=1.0;
int four_color_rgb=0, document_mode=0, quick_interpolate=0;
int verbose=0, use_auto_wb=0, use_camera_wb=0;
float camera_red, camera_blue;
float pre_mul[4], coeff[3][4];
int histogram[0x2000];
void write_ppm(FILE *);
void (*write_fun)(FILE *) = write_ppm;
jmp_buf failure;

struct decode {
  struct decode *branch[2];
  int leaf;
} first_decode[2048], *second_decode, *free_decode;

#define CLASS

/*
   In order to inline this calculation, I make the risky
   assumption that all filter patterns can be described
   by a repeating pattern of eight rows and two columns

   Return values are either 0/1/2/3 = G/M/C/Y or 0/1/2/3 = R/G1/B/G2
 */
#define FC(row,col) \
	(filters >> ((((row) << 1 & 14) + ((col) & 1)) << 1) & 3)

#define BAYER(row,col) \
	image[((row) >> shrink)*iwidth + ((col) >> shrink)][FC(row,col)]

/*
   PowerShot 600 uses 0xe1e4e1e4:

	  0 1 2 3 4 5
	0 G M G M G M
	1 C Y C Y C Y
	2 M G M G M G
	3 C Y C Y C Y

   PowerShot A5 uses 0x1e4e1e4e:

	  0 1 2 3 4 5
	0 C Y C Y C Y
	1 G M G M G M
	2 C Y C Y C Y
	3 M G M G M G

   PowerShot A50 uses 0x1b4e4b1e:

	  0 1 2 3 4 5
	0 C Y C Y C Y
	1 M G M G M G
	2 Y C Y C Y C
	3 G M G M G M
	4 C Y C Y C Y
	5 G M G M G M
	6 Y C Y C Y C
	7 M G M G M G

   PowerShot Pro70 uses 0x1e4b4e1b:

	  0 1 2 3 4 5
	0 Y C Y C Y C
	1 M G M G M G
	2 C Y C Y C Y
	3 G M G M G M
	4 Y C Y C Y C
	5 G M G M G M
	6 C Y C Y C Y
	7 M G M G M G

   PowerShots Pro90 and G1 use 0xb4b4b4b4:

	  0 1 2 3 4 5
	0 G M G M G M
	1 Y C Y C Y C

   All RGB cameras use one of these Bayer grids:

	0x16161616:	0x61616161:	0x49494949:	0x94949494:

	  0 1 2 3 4 5	  0 1 2 3 4 5	  0 1 2 3 4 5	  0 1 2 3 4 5
	0 B G B G B G	0 G R G R G R	0 G B G B G B	0 R G R G R G
	1 G R G R G R	1 B G B G B G	1 R G R G R G	1 G B G B G B
	2 B G B G B G	2 G R G R G R	2 G B G B G B	2 R G R G R G
	3 G R G R G R	3 B G B G B G	3 R G R G R G	3 G B G B G B

 */

#ifndef __GLIBC__
char *memmem (char *haystack, size_t haystacklen,
	      char *needle, size_t needlelen)
{
  char *c;
  for (c = haystack; c <= haystack + haystacklen - needlelen; c++)
    if (!memcmp (c, needle, needlelen))
      return c;
  return NULL;
}
#endif

void CLASS merror (void *ptr, char *where)
{
  if (ptr) return;
  fprintf (stderr, "%s: Out of memory in %s\n", ifname, where);
  longjmp (failure, 1);
}

/*
   Get a 2-byte integer, making no assumptions about CPU byte order.
   Nor should we assume that the compiler evaluates left-to-right.
 */
ushort CLASS fget2 (FILE *f)
{
  uchar a, b;

  a = fgetc(f);
  b = fgetc(f);
  if (order == 0x4949)		/* "II" means little-endian */
    return a + (b << 8);
  else				/* "MM" means big-endian */
    return (a << 8) + b;
}

/*
   Same for a 4-byte integer.
 */
int CLASS fget4 (FILE *f)
{
  uchar a, b, c, d;

  a = fgetc(f);
  b = fgetc(f);
  c = fgetc(f);
  d = fgetc(f);
  if (order == 0x4949)
    return a + (b << 8) + (c << 16) + (d << 24);
  else
    return (a << 24) + (b << 16) + (c << 8) + d;
}

void CLASS canon_600_load_raw()
{
  uchar  data[1120], *dp;
  ushort pixel[896], *pix;
  int irow, orow, col;

  for (irow=orow=0; irow < height; irow++)
  {
    fread (data, 1120, 1, ifp);
    for (dp=data, pix=pixel; dp < data+1120; dp+=10, pix+=8)
    {
      pix[0] = (dp[0] << 2) + (dp[1] >> 6    );
      pix[1] = (dp[2] << 2) + (dp[1] >> 4 & 3);
      pix[2] = (dp[3] << 2) + (dp[1] >> 2 & 3);
      pix[3] = (dp[4] << 2) + (dp[1]      & 3);
      pix[4] = (dp[5] << 2) + (dp[9]      & 3);
      pix[5] = (dp[6] << 2) + (dp[9] >> 2 & 3);
      pix[6] = (dp[7] << 2) + (dp[9] >> 4 & 3);
      pix[7] = (dp[8] << 2) + (dp[9] >> 6    );
    }
    for (col=0; col < width; col++)
      BAYER(orow,col) = pixel[col] << 4;
    for (col=width; col < 896; col++)
      black += pixel[col];
    if ((orow+=2) > height)
      orow = 1;
  }
  black = ((INT64) black << 4) / ((896 - width) * height);
}

void CLASS canon_a5_load_raw()
{
  uchar  data[1940], *dp;
  ushort pixel[1552], *pix;
  int row, col;

  for (row=0; row < height; row++) {
    fread (data, raw_width * 10 / 8, 1, ifp);
    for (dp=data, pix=pixel; pix < pixel+raw_width; dp+=10, pix+=8)
    {
      pix[0] = (dp[1] << 2) + (dp[0] >> 6);
      pix[1] = (dp[0] << 4) + (dp[3] >> 4);
      pix[2] = (dp[3] << 6) + (dp[2] >> 2);
      pix[3] = (dp[2] << 8) + (dp[5]     );
      pix[4] = (dp[4] << 2) + (dp[7] >> 6);
      pix[5] = (dp[7] << 4) + (dp[6] >> 4);
      pix[6] = (dp[6] << 6) + (dp[9] >> 2);
      pix[7] = (dp[9] << 8) + (dp[8]     );
    }
    for (col=0; col < width; col++)
      BAYER(row,col) = (pixel[col] & 0x3ff) << 4;
    for (col=width; col < raw_width; col++)
      black += pixel[col] & 0x3ff;
  }
  if (raw_width > width)
    black = ((INT64) black << 4) / ((raw_width - width) * height);
}

/*
   getbits(-1) initializes the buffer
   getbits(n) where 0 <= n <= 25 returns an n-bit integer
 */
unsigned CLASS getbits (int nbits)
{
  static unsigned long bitbuf=0;
  static int vbits=0;
  unsigned c, ret;

  if (nbits == 0) return 0;
  if (nbits == -1)
    ret = bitbuf = vbits = 0;
  else {
    ret = bitbuf << (LONG_BIT - vbits) >> (LONG_BIT - nbits);
    vbits -= nbits;
  }
  while (vbits < LONG_BIT - 7) {
    c = fgetc(ifp);
    bitbuf = (bitbuf << 8) + c;
    if (c == 0xff && zero_after_ff)
      fgetc(ifp);
    vbits += 8;
  }
  return ret;
}

void CLASS init_decoder()
{
  memset (first_decode, 0, sizeof first_decode);
  free_decode = first_decode;
}

/*
   Construct a decode tree according the specification in *source.
   The first 16 bytes specify how many codes should be 1-bit, 2-bit
   3-bit, etc.  Bytes after that are the leaf values.

   For example, if the source is

    { 0,1,4,2,3,1,2,0,0,0,0,0,0,0,0,0,
      0x04,0x03,0x05,0x06,0x02,0x07,0x01,0x08,0x09,0x00,0x0a,0x0b,0xff  },

   then the code is

	00		0x04
	010		0x03
	011		0x05
	100		0x06
	101		0x02
	1100		0x07
	1101		0x01
	11100		0x08
	11101		0x09
	11110		0x00
	111110		0x0a
	1111110		0x0b
	1111111		0xff
 */
uchar * CLASS make_decoder (const uchar *source, int level)
{
  struct decode *cur;
  static int leaf;
  int i, next;

  if (level==0) leaf=0;
  cur = free_decode++;
  if (free_decode > first_decode+2048) {
    fprintf (stderr, "%s: decoder table overflow\n", ifname);
    longjmp (failure, 2);
  }
  for (i=next=0; i <= leaf && next < 16; )
    i += source[next++];
  if (i > leaf) {
    if (level < next) {
      cur->branch[0] = free_decode;
      make_decoder (source, level+1);
      cur->branch[1] = free_decode;
      make_decoder (source, level+1);
    } else
      cur->leaf = source[16 + leaf++];
  }
  return (uchar *) source + 16 + leaf;
}

void CLASS crw_init_tables (unsigned table)
{
  static const uchar first_tree[3][29] = {
    { 0,1,4,2,3,1,2,0,0,0,0,0,0,0,0,0,
      0x04,0x03,0x05,0x06,0x02,0x07,0x01,0x08,0x09,0x00,0x0a,0x0b,0xff  },

    { 0,2,2,3,1,1,1,1,2,0,0,0,0,0,0,0,
      0x03,0x02,0x04,0x01,0x05,0x00,0x06,0x07,0x09,0x08,0x0a,0x0b,0xff  },

    { 0,0,6,3,1,1,2,0,0,0,0,0,0,0,0,0,
      0x06,0x05,0x07,0x04,0x08,0x03,0x09,0x02,0x00,0x0a,0x01,0x0b,0xff  },
  };

  static const uchar second_tree[3][180] = {
    { 0,2,2,2,1,4,2,1,2,5,1,1,0,0,0,139,
      0x03,0x04,0x02,0x05,0x01,0x06,0x07,0x08,
      0x12,0x13,0x11,0x14,0x09,0x15,0x22,0x00,0x21,0x16,0x0a,0xf0,
      0x23,0x17,0x24,0x31,0x32,0x18,0x19,0x33,0x25,0x41,0x34,0x42,
      0x35,0x51,0x36,0x37,0x38,0x29,0x79,0x26,0x1a,0x39,0x56,0x57,
      0x28,0x27,0x52,0x55,0x58,0x43,0x76,0x59,0x77,0x54,0x61,0xf9,
      0x71,0x78,0x75,0x96,0x97,0x49,0xb7,0x53,0xd7,0x74,0xb6,0x98,
      0x47,0x48,0x95,0x69,0x99,0x91,0xfa,0xb8,0x68,0xb5,0xb9,0xd6,
      0xf7,0xd8,0x67,0x46,0x45,0x94,0x89,0xf8,0x81,0xd5,0xf6,0xb4,
      0x88,0xb1,0x2a,0x44,0x72,0xd9,0x87,0x66,0xd4,0xf5,0x3a,0xa7,
      0x73,0xa9,0xa8,0x86,0x62,0xc7,0x65,0xc8,0xc9,0xa1,0xf4,0xd1,
      0xe9,0x5a,0x92,0x85,0xa6,0xe7,0x93,0xe8,0xc1,0xc6,0x7a,0x64,
      0xe1,0x4a,0x6a,0xe6,0xb3,0xf1,0xd3,0xa5,0x8a,0xb2,0x9a,0xba,
      0x84,0xa4,0x63,0xe5,0xc5,0xf3,0xd2,0xc4,0x82,0xaa,0xda,0xe4,
      0xf2,0xca,0x83,0xa3,0xa2,0xc3,0xea,0xc2,0xe2,0xe3,0xff,0xff  },

    { 0,2,2,1,4,1,4,1,3,3,1,0,0,0,0,140,
      0x02,0x03,0x01,0x04,0x05,0x12,0x11,0x06,
      0x13,0x07,0x08,0x14,0x22,0x09,0x21,0x00,0x23,0x15,0x31,0x32,
      0x0a,0x16,0xf0,0x24,0x33,0x41,0x42,0x19,0x17,0x25,0x18,0x51,
      0x34,0x43,0x52,0x29,0x35,0x61,0x39,0x71,0x62,0x36,0x53,0x26,
      0x38,0x1a,0x37,0x81,0x27,0x91,0x79,0x55,0x45,0x28,0x72,0x59,
      0xa1,0xb1,0x44,0x69,0x54,0x58,0xd1,0xfa,0x57,0xe1,0xf1,0xb9,
      0x49,0x47,0x63,0x6a,0xf9,0x56,0x46,0xa8,0x2a,0x4a,0x78,0x99,
      0x3a,0x75,0x74,0x86,0x65,0xc1,0x76,0xb6,0x96,0xd6,0x89,0x85,
      0xc9,0xf5,0x95,0xb4,0xc7,0xf7,0x8a,0x97,0xb8,0x73,0xb7,0xd8,
      0xd9,0x87,0xa7,0x7a,0x48,0x82,0x84,0xea,0xf4,0xa6,0xc5,0x5a,
      0x94,0xa4,0xc6,0x92,0xc3,0x68,0xb5,0xc8,0xe4,0xe5,0xe6,0xe9,
      0xa2,0xa3,0xe3,0xc2,0x66,0x67,0x93,0xaa,0xd4,0xd5,0xe7,0xf8,
      0x88,0x9a,0xd7,0x77,0xc4,0x64,0xe2,0x98,0xa5,0xca,0xda,0xe8,
      0xf3,0xf6,0xa9,0xb2,0xb3,0xf2,0xd2,0x83,0xba,0xd3,0xff,0xff  },

    { 0,0,6,2,1,3,3,2,5,1,2,2,8,10,0,117,
      0x04,0x05,0x03,0x06,0x02,0x07,0x01,0x08,
      0x09,0x12,0x13,0x14,0x11,0x15,0x0a,0x16,0x17,0xf0,0x00,0x22,
      0x21,0x18,0x23,0x19,0x24,0x32,0x31,0x25,0x33,0x38,0x37,0x34,
      0x35,0x36,0x39,0x79,0x57,0x58,0x59,0x28,0x56,0x78,0x27,0x41,
      0x29,0x77,0x26,0x42,0x76,0x99,0x1a,0x55,0x98,0x97,0xf9,0x48,
      0x54,0x96,0x89,0x47,0xb7,0x49,0xfa,0x75,0x68,0xb6,0x67,0x69,
      0xb9,0xb8,0xd8,0x52,0xd7,0x88,0xb5,0x74,0x51,0x46,0xd9,0xf8,
      0x3a,0xd6,0x87,0x45,0x7a,0x95,0xd5,0xf6,0x86,0xb4,0xa9,0x94,
      0x53,0x2a,0xa8,0x43,0xf5,0xf7,0xd4,0x66,0xa7,0x5a,0x44,0x8a,
      0xc9,0xe8,0xc8,0xe7,0x9a,0x6a,0x73,0x4a,0x61,0xc7,0xf4,0xc6,
      0x65,0xe9,0x72,0xe6,0x71,0x91,0x93,0xa6,0xda,0x92,0x85,0x62,
      0xf3,0xc5,0xb2,0xa4,0x84,0xba,0x64,0xa5,0xb3,0xd2,0x81,0xe5,
      0xd3,0xaa,0xc4,0xca,0xf2,0xb1,0xe4,0xd1,0x83,0x63,0xea,0xc3,
      0xe2,0x82,0xf1,0xa3,0xc2,0xa1,0xc1,0xe3,0xa2,0xe1,0xff,0xff  }
  };

  if (table > 2) table = 2;
  init_decoder();
  make_decoder ( first_tree[table], 0);
  second_decode = free_decode;
  make_decoder (second_tree[table], 0);
}

/*
   Return 0 if the image starts with compressed data,
   1 if it starts with uncompressed low-order bits.

   In Canon compressed data, 0xff is always followed by 0x00.
 */
int CLASS canon_has_lowbits()
{
  uchar test[0x4000];
  int ret=1, i;

  fseek (ifp, 0, SEEK_SET);
  fread (test, 1, sizeof test, ifp);
  for (i=540; i < sizeof test - 1; i++)
    if (test[i] == 0xff) {
      if (test[i+1]) return 1;
      ret=0;
    }
  return ret;
}

void CLASS canon_compressed_load_raw()
{
  ushort *pixel, *prow;
  int lowbits, shift, i, row, r, col, save, val;
  unsigned irow, icol;
  struct decode *decode, *dindex;
  int block, diffbuf[64], leaf, len, diff, carry=0, pnum=0, base[2];
  uchar c;
  INT64 bblack=0;

  pixel = calloc (raw_width*8, sizeof *pixel);
  merror (pixel, "canon_compressed_load_raw()");
  lowbits = canon_has_lowbits();
  shift = 4 - lowbits*2;
  fseek (ifp, 540 + lowbits*raw_height*raw_width/4, SEEK_SET);
  zero_after_ff = 1;
  getbits(-1);
  for (row=0; row < raw_height; row+=8) {
    for (block=0; block < raw_width >> 3; block++) {
      memset (diffbuf, 0, sizeof diffbuf);
      decode = first_decode;
      for (i=0; i < 64; i++ ) {
	for (dindex=decode; dindex->branch[0]; )
	  dindex = dindex->branch[getbits(1)];
	leaf = dindex->leaf;
	decode = second_decode;
	if (leaf == 0 && i) break;
	if (leaf == 0xff) continue;
	i  += leaf >> 4;
	len = leaf & 15;
	if (len == 0) continue;
	diff = getbits(len);
	if ((diff & (1 << (len-1))) == 0)
	  diff -= (1 << len) - 1;
	if (i < 64) diffbuf[i] = diff;
      }
      diffbuf[0] += carry;
      carry = diffbuf[0];
      for (i=0; i < 64; i++ ) {
	if (pnum++ % raw_width == 0)
	  base[0] = base[1] = 512;
	pixel[(block << 6) + i] = ( base[i & 1] += diffbuf[i] );
      }
    }
    if (lowbits) {
      save = ftell(ifp);			/* Don't lose our place */
      fseek (ifp, 26 + row*raw_width/4, SEEK_SET);
      for (prow=pixel, i=0; i < raw_width*2; i++) {
	c = fgetc(ifp);
	for (r=0; r < 8; r+=2, prow++) {
	  val = (*prow << 2) + ((c >> r) & 3);
	  if (raw_width == 2672 && val < 512) val += 2;
	  *prow = val;
	}
      }
      fseek (ifp, save, SEEK_SET);
    }
    for (r=0; r < 8; r++) {
      irow = row - top_margin + r;
      if (irow >= height) continue;
      for (col=0; col < raw_width; col++) {
	icol = col - left_margin;
	if (icol < width)
	  BAYER(irow,icol) = pixel[r*raw_width+col] << shift;
	else
	  bblack += pixel[r*raw_width+col];
      }
    }
  }
  free (pixel);
  if (raw_width > width)
    black = (bblack << shift) / ((raw_width - width) * height);
}

void CLASS kodak_curve (ushort *curve)
{
  int i, entries, tag, type, len, val;

  for (i=0; i < 0x1000; i++)
    curve[i] = i;
  if (strcasecmp(make,"KODAK")) return;
  if (!curve_offset) {
    fseek (ifp, 12, SEEK_SET);
    entries = fget2(ifp);
    while (entries--) {
      tag  = fget2(ifp);
      type = fget2(ifp);
      len  = fget4(ifp);
      val  = fget4(ifp);
      if (tag == 0x90d) {
	curve_offset = val;
	curve_length = len;
      }
    }
  }
  if (curve_offset) {
    fseek (ifp, curve_offset, SEEK_SET);
    for (i=0; i < curve_length; i++)
      curve[i] = fget2(ifp);
    for ( ; i < 0x1000; i++)
      curve[i] = curve[i-1];
    rgb_max = curve[i-1] << 2;
  }
  fseek (ifp, data_offset, SEEK_SET);
}

/*
   Not a full implementation of Lossless JPEG,
   just enough to decode Canon and Kodak images.
 */
void CLASS lossless_jpeg_load_raw()
{
  int tag, len, jhigh=0, jwide=0, trick, row, col, diff;
  uchar data[256], *dp;
  int vpred[2] = { 0x800, 0x800 }, hpred[2];
  struct decode *dstart[2], *dindex;
  ushort curve[0x10000];
  INT64 bblack=0;
  int min=INT_MAX;

  kodak_curve (curve);
  order = 0x4d4d;
  if (fget2(ifp) != 0xffd8) return;
  do {
    tag = fget2(ifp);
    len = fget2(ifp) - 2;
    if (tag <= 0xff00 || len > 255) return;
    fread (data, 1, len, ifp);
    switch (tag) {
      case 0xffc3:
	jhigh = (data[1] << 8) + data[2];
	jwide = (data[3] << 8) + data[4];
	break;
      case 0xffc4:
	init_decoder();
	dstart[0] = dstart[1] = free_decode;
	for (dp = data; dp < data+len && *dp < 2; ) {
	  dstart[*dp] = free_decode;
	  dp = make_decoder (++dp, 0);
	}
    }
  } while (tag != 0xffda);

  trick = 2 * jwide / width;
  zero_after_ff = 1;
  getbits(-1);
  for (row=0; row < raw_height; row++)
    for (col=0; col < raw_width; col++)
    {
      for (dindex = dstart[col & 1]; dindex->branch[0]; )
	dindex = dindex->branch[getbits(1)];
      len = dindex->leaf;
      diff = getbits(len);
      if ((diff & (1 << (len-1))) == 0)
	diff -= (1 << len) - 1;
      if (col < 2 && (row % trick == 0)) {
	vpred[col] += diff;
	hpred[col] = vpred[col];
      } else
	hpred[col & 1] += diff;
      diff = hpred[col & 1];
      if (diff < 0) diff = 0;
      if ((unsigned) (row-top_margin) >= height)
	continue;
      if ((unsigned) (col-left_margin) < width) {
	BAYER(row-top_margin,col-left_margin) = curve[diff] << 2;
	if (min > curve[diff])
	    min = curve[diff];
      } else
	bblack += curve[diff];
    }
  if (raw_width > width)
    black = (bblack << 2) / ((raw_width - width) * height);
  if (!strcasecmp(make,"KODAK"))
    black = min << 2;
}

void CLASS nikon_compressed_load_raw()
{
  static const uchar nikon_tree[] = {
    0,1,5,1,1,1,1,1,1,2,0,0,0,0,0,0,
    5,4,3,6,2,7,1,0,8,9,11,10,12
  };
  int vpred[4], hpred[2], csize, row, col, i, len, diff;
  ushort *curve;
  struct decode *dindex;

  init_decoder();
  make_decoder (nikon_tree, 0);

  fseek (ifp, curve_offset, SEEK_SET);
  for (i=0; i < 4; i++)
    vpred[i] = fget2(ifp);
  csize = fget2(ifp);
  curve = calloc (csize, sizeof *curve);
  merror (curve, "nikon_compressed_load_raw()");
  for (i=0; i < csize; i++)
    curve[i] = fget2(ifp);

  fseek (ifp, data_offset, SEEK_SET);
  getbits(-1);

  for (row=0; row < height; row++)
    for (col=0; col < raw_width; col++)
    {
      for (dindex=first_decode; dindex->branch[0]; )
	dindex = dindex->branch[getbits(1)];
      len = dindex->leaf;
      diff = getbits(len);
      if ((diff & (1 << (len-1))) == 0)
	diff -= (1 << len) - 1;
      if (col < 2) {
	i = 2*(row & 1) + (col & 1);
	vpred[i] += diff;
	hpred[col] = vpred[i];
      } else
	hpred[col & 1] += diff;
      if ((unsigned) (col-left_margin) >= width) continue;
      diff = hpred[col & 1];
      if (diff < 0) diff = 0;
      if (diff >= csize) diff = csize-1;
      BAYER(row,col-left_margin) = curve[diff] << 2;
    }
  free (curve);
}

void CLASS nikon_load_raw()
{
  int irow, row, col, i;

  getbits(-1);
  for (irow=0; irow < height; irow++) {
    row = irow;
    if (model[0] == 'E') {
      row = irow * 2 % height + irow / (height/2);
      if (row == 1 && atoi(model+1) < 5000) {
	fseek (ifp, 0, SEEK_END);
	fseek (ifp, ftell(ifp)/2, SEEK_SET);
	getbits(-1);
      }
    }
    for (col=0; col < raw_width; col++) {
      i = getbits(12);
      if ((unsigned) (col-left_margin) < width)
	BAYER(row,col-left_margin) = i << 2;
      if (tiff_data_compression == 34713 && (col % 10) == 9)
	getbits(8);
    }
  }
}

/*
   Figure out if a NEF file is compressed.  These fancy heuristics
   are only needed for the D100, thanks to a bug in some cameras
   that tags all images as "compressed".
 */
int CLASS nikon_is_compressed()
{
  uchar test[256];
  int i;

  if (tiff_data_compression != 34713)
    return 0;
  if (strcmp(model,"D100"))
    return 1;
  fseek (ifp, data_offset, SEEK_SET);
  fread (test, 1, 256, ifp);
  for (i=15; i < 256; i+=16)
    if (test[i]) return 1;
  return 0;
}

/*
   Returns 1 for a Coolpix 990, 0 for a Coolpix 995.
 */
int CLASS nikon_e990()
{
  int i, histo[256];
  const uchar often[] = { 0x00, 0x55, 0xaa, 0xff };

  memset (histo, 0, sizeof histo);
  fseek (ifp, 2064*1540*3/4, SEEK_SET);
  for (i=0; i < 2000; i++)
    histo[fgetc(ifp)]++;
  for (i=0; i < 4; i++)
    if (histo[often[i]] > 400)
      return 1;
  return 0;
}

/*
   Returns 1 for a Coolpix 2100, 0 for anything else.
 */
int CLASS nikon_e2100()
{
  uchar t[12];
  int i;

  fseek (ifp, 0, SEEK_SET);
  for (i=0; i < 1024; i++) {
    fread (t, 1, 12, ifp);
    if (((t[2] & t[4] & t[7] & t[9]) >> 4
	& t[1] & t[6] & t[8] & t[11] & 3) != 3)
      return 0;
  }
  return 1;
}

void CLASS nikon_e2100_load_raw()
{
  uchar   data[2424], *dp;
  ushort pixel[1616], *pix;
  int row, col;

  for (row=0; row <= height; row+=2) {
    if (row == height) {
      fseek (ifp, 8792, SEEK_CUR);
      row = 1;
    }
    fread (data, 2424, 1, ifp);
    for (dp=data, pix=pixel; dp < data+2424; dp+=12, pix+=8)
    {
      pix[0] = (dp[ 3] << 2) + (dp[2] >> 6);
      pix[1] = (dp[ 1] >> 2) + (dp[2] << 6);
      pix[2] = (dp[ 0] << 2) + (dp[7] >> 6);
      pix[3] = (dp[ 6] >> 2) + (dp[7] << 6);
      pix[4] = (dp[ 5] << 2) + (dp[4] >> 6);
      pix[5] = (dp[11] >> 2) + (dp[4] << 6);
      pix[6] = (dp[10] << 2) + (dp[9] >> 6);
      pix[7] = (dp[ 8] >> 2) + (dp[9] << 6);
    }
    for (col=0; col < width; col++)
      BAYER(row,col) = (pixel[col] & 0x3ff) << 4;
  }
}

void CLASS nikon_e950_load_raw()
{
  int irow, row, col;

  getbits(-1);
  for (irow=0; irow < height; irow++) {
    row = irow * 2 % height;
    for (col=0; col < width; col++)
      BAYER(row,col) = getbits(10) << 4;
    for (col=28; col--; )
      getbits(8);
  }
}

/*
   The Fuji Super CCD is just a Bayer grid rotated 45 degrees.
 */
void CLASS fuji_s2_load_raw()
{
  ushort pixel[2944];
  int row, col, r, c;

  fseek (ifp, (2944*24+32)*2, SEEK_CUR);
  for (row=0; row < 2144; row++) {
    fread (pixel, 2, 2944, ifp);
    for (col=0; col < 2880; col++) {
      r = row + ((col+1) >> 1);
      c = 2143 - row + (col >> 1);
      BAYER(r,c) = ntohs(pixel[col]) << 2;
    }
  }
}

void CLASS fuji_common_load_raw (int ncol, int icol, int nrow)
{
  ushort pixel[2048];
  int row, col, r, c;

  for (row=0; row < nrow; row++) {
    fread (pixel, 2, ncol, ifp);
    if (ntohs(0xaa55) == 0xaa55)	/* data is little-endian */
      swab (pixel, pixel, ncol*2);
    for (col=0; col <= icol; col++) {
      r = icol - col + (row >> 1);
      c = col + ((row+1) >> 1);
      BAYER(r,c) = pixel[col] << 2;
    }
  }
}

void CLASS fuji_s5000_load_raw()
{
  fseek (ifp, (1472*4+24)*2, SEEK_CUR);
  fuji_common_load_raw (1472, 1423, 2152);
}

void CLASS fuji_s7000_load_raw()
{
  fuji_common_load_raw (2048, 2047, 3080);
}

/*
   The Fuji Super CCD SR has two photodiodes for each pixel.
   The secondary has about 1/16 the sensitivity of the primary,
   but this ratio may vary.
 */
void CLASS fuji_f700_load_raw()
{
  ushort pixel[2944];
  int row, col, r, c, val;

  for (row=0; row < 2168; row++) {
    fread (pixel, 2, 2944, ifp);
    if (ntohs(0xaa55) == 0xaa55)	/* data is little-endian */
      swab (pixel, pixel, 2944*2);
    for (col=0; col < 1440; col++) {
      r = 1439 - col + (row >> 1);
      c = col + ((row+1) >> 1);
      val = pixel[col+16];
      if (val == 0x3fff) {		/* If the primary is maxed, */
	val = pixel[col+1488] << 4;	/* use the secondary.       */
	rgb_max = 0xffff;
      }
      if (val > 0xffff)
	val = 0xffff;
      BAYER(r,c) = val;
    }
  }
}

void CLASS rollei_load_raw()
{
  uchar pixel[10];
  unsigned iten=0, isix, i, buffer=0, row, col, todo[16];

  isix = raw_width * raw_height * 5 / 8;
  while (fread (pixel, 1, 10, ifp) == 10) {
    for (i=0; i < 10; i+=2) {
      todo[i]   = iten++;
      todo[i+1] = pixel[i] << 8 | pixel[i+1];
      buffer    = pixel[i] >> 2 | buffer << 6;
    }
    for (   ; i < 16; i+=2) {
      todo[i]   = isix++;
      todo[i+1] = buffer >> (14-i)*5;
    }
    for (i=0; i < 16; i+=2) {
      row = todo[i] / raw_width - top_margin;
      col = todo[i] % raw_width - left_margin;
      if (row < height && col < width)
	BAYER(row,col) = (todo[i+1] & 0x3ff) << 4;
    }
  }
}

void CLASS phase_one_load_raw()
{
  int row, col, a, b;
  ushort pixel[4134], akey, bkey;

  fseek (ifp, 8, SEEK_CUR);
  fseek (ifp, fget4(ifp) + 296, SEEK_CUR);
  akey = fget2(ifp);
  bkey = fget2(ifp);
  fseek (ifp, data_offset + 12 + top_margin*raw_width*2, SEEK_SET);
  for (row=0; row < height; row++) {
    fread (pixel, 2, raw_width, ifp);
    for (col=0; col < raw_width; col+=2) {
      a = ntohs(pixel[col+0]) ^ akey;
      b = ntohs(pixel[col+1]) ^ bkey;
      pixel[col+0] = (b & 0xaaaa) | (a & 0x5555);
      pixel[col+1] = (a & 0xaaaa) | (b & 0x5555);
    }
    for (col=0; col < width; col++)
      BAYER(row,col) = pixel[col+left_margin];
  }
}

void CLASS ixpress_load_raw()
{
  ushort pixel[4090];
  int row, col;

  fseek (ifp, 304 + 6*2*4090, SEEK_SET);
  for (row=height; --row >= 0; ) {
    fread (pixel, 2, 4090, ifp);
    if (ntohs(0xaa55) == 0xaa55)	/* data is little-endian */
      swab (pixel, pixel, 4090*2);
    for (col=0; col < width; col++)
      BAYER(row,col) = pixel[width-1-col];
  }
}

/* For this function only, raw_width is in bytes, not pixels! */
void CLASS packed_12_load_raw()
{
  int row, col;

  getbits(-1);
  for (row=0; row < height; row++) {
    for (col=0; col < width; col++)
      BAYER(row,col) = getbits(12) << 2;
    for (col = width*3/2; col < raw_width; col++)
      getbits(8);
  }
}

void CLASS unpacked_load_raw (int order, int rsh)
{
  ushort *pixel;
  int row, col;

  pixel = calloc (raw_width, sizeof *pixel);
  merror (pixel, "unpacked_load_raw()");
  for (row=0; row < height; row++) {
    fread (pixel, 2, raw_width, ifp);
    if (order != ntohs(0x55aa))
      swab (pixel, pixel, width*2);
    for (col=0; col < width; col++)
      BAYER(row,col) = pixel[col] << 8 >> (8+rsh);
  }
  free (pixel);
}

void CLASS be_16_load_raw()		/* "be" = "big-endian" */
{
  unpacked_load_raw (0x55aa, 0);
}

void CLASS be_high_12_load_raw()
{
  unpacked_load_raw (0x55aa, 2);
}

void CLASS be_low_12_load_raw()
{
  unpacked_load_raw (0x55aa,-2);
}

void CLASS be_low_10_load_raw()
{
  unpacked_load_raw (0x55aa,-4);
}

void CLASS le_high_12_load_raw()	/* "le" = "little-endian" */
{
  unpacked_load_raw (0xaa55, 2);
}

void CLASS olympus_cseries_load_raw()
{
  int irow, row, col;

  for (irow=0; irow < height; irow++) {
    row = irow * 2 % height + irow / (height/2);
    if (row < 2) {
      fseek (ifp, data_offset - row*(-width*height*3/4 & -2048), SEEK_SET);
      getbits(-1);
    }
    for (col=0; col < width; col++)
      BAYER(row,col) = getbits(12) << 2;
  }
}

void CLASS eight_bit_load_raw()
{
  uchar *pixel;
  int row, col;

  pixel = calloc (raw_width, sizeof *pixel);
  merror (pixel, "eight_bit_load_raw()");
  for (row=0; row < height; row++) {
    fread (pixel, 1, raw_width, ifp);
    for (col=0; col < width; col++)
      BAYER(row,col) = pixel[col] << 6;
  }
  free (pixel);
}

void CLASS casio_qv5700_load_raw()
{
  uchar  data[3232],  *dp;
  ushort pixel[2576], *pix;
  int row, col;

  for (row=0; row < height; row++) {
    fread (data, 1, 3232, ifp);
    for (dp=data, pix=pixel; dp < data+3220; dp+=5, pix+=4) {
      pix[0] = (dp[0] << 2) + (dp[1] >> 6);
      pix[1] = (dp[1] << 4) + (dp[2] >> 4);
      pix[2] = (dp[2] << 6) + (dp[3] >> 2);
      pix[3] = (dp[3] << 8) + (dp[4]     );
    }
    for (col=0; col < width; col++)
      BAYER(row,col) = (pixel[col] & 0x3ff) << 4;
  }
}

void CLASS nucore_load_raw()
{
  uchar *data, *dp;
  int irow, row, col;

  data = calloc (width, 2);
  merror (data, "nucore_load_raw()");
  for (irow=0; irow < height; irow++) {
    fread (data, 2, width, ifp);
    if (model[0] == 'B' && width == 2598)
      row = height - 1 - irow/2 - height/2 * (irow & 1);
    else
      row = irow;
    for (dp=data, col=0; col < width; col++, dp+=2)
      BAYER(row,col) = (dp[0] << 2) + (dp[1] << 10);
  }
  free (data);
}

const int * CLASS make_decoder_int (const int *source, int level)
{
  struct decode *cur;

  cur = free_decode++;
  if (level < source[0]) {
    cur->branch[0] = free_decode;
    source = make_decoder_int (source, level+1);
    cur->branch[1] = free_decode;
    source = make_decoder_int (source, level+1);
  } else {
    cur->leaf = source[1];
    source += 2;
  }
  return source;
}

int CLASS radc_token (int tree)
{
  int t;
  static struct decode *dstart[18], *dindex;
  static const int *s, source[] = {
    1,1, 2,3, 3,4, 4,2, 5,7, 6,5, 7,6, 7,8,
    1,0, 2,1, 3,3, 4,4, 5,2, 6,7, 7,6, 8,5, 8,8,
    2,1, 2,3, 3,0, 3,2, 3,4, 4,6, 5,5, 6,7, 6,8,
    2,0, 2,1, 2,3, 3,2, 4,4, 5,6, 6,7, 7,5, 7,8,
    2,1, 2,4, 3,0, 3,2, 3,3, 4,7, 5,5, 6,6, 6,8,
    2,3, 3,1, 3,2, 3,4, 3,5, 3,6, 4,7, 5,0, 5,8,
    2,3, 2,6, 3,0, 3,1, 4,4, 4,5, 4,7, 5,2, 5,8,
    2,4, 2,7, 3,3, 3,6, 4,1, 4,2, 4,5, 5,0, 5,8,
    2,6, 3,1, 3,3, 3,5, 3,7, 3,8, 4,0, 5,2, 5,4,
    2,0, 2,1, 3,2, 3,3, 4,4, 4,5, 5,6, 5,7, 4,8,
    1,0, 2,2, 2,-2,
    1,-3, 1,3,
    2,-17, 2,-5, 2,5, 2,17,
    2,-7, 2,2, 2,9, 2,18,
    2,-18, 2,-9, 2,-2, 2,7,
    2,-28, 2,28, 3,-49, 3,-9, 3,9, 4,49, 5,-79, 5,79,
    2,-1, 2,13, 2,26, 3,39, 4,-16, 5,55, 6,-37, 6,76,
    2,-26, 2,-13, 2,1, 3,-39, 4,16, 5,-55, 6,-76, 6,37
  };

  if (free_decode == first_decode)
    for (s=source, t=0; t < 18; t++) {
      dstart[t] = free_decode;
      s = make_decoder_int (s, 0);
    }
  if (tree == 18) {
    if (model[2] == '4')
      return (getbits(5) << 3) + 4;	/* DC40 */
    else
      return (getbits(6) << 2) + 2;	/* DC50 */
  }
  for (dindex = dstart[tree]; dindex->branch[0]; )
    dindex = dindex->branch[getbits(1)];
  return dindex->leaf;
}

#define FORYX for (y=1; y < 3; y++) for (x=col+1; x >= col; x--)

#define PREDICTOR (c ? (buf[c][y-1][x] + buf[c][y][x+1]) / 2 \
: (buf[c][y-1][x+1] + 2*buf[c][y-1][x] + buf[c][y][x+1]) / 4)

void CLASS kodak_radc_load_raw()
{
  int row, col, tree, nreps, rep, step, i, c, s, r, x, y, val;
  short last[3] = { 16,16,16 }, mul[3], buf[3][3][386];

  init_decoder();
  getbits(-1);
  for (i=0; i < sizeof(buf)/sizeof(short); i++)
    buf[0][0][i] = 2048;
  for (row=0; row < height; row+=4) {
    for (i=0; i < 3; i++)
      mul[i] = getbits(6);
    for (c=0; c < 3; c++) {
      val = ((0x1000000/last[c] + 0x7ff) >> 12) * mul[c];
      s = val > 65564 ? 10:12;
      x = ~(-1 << (s-1));
      val <<= 12-s;
      for (i=0; i < sizeof(buf[0])/sizeof(short); i++)
	buf[c][0][i] = (buf[c][0][i] * val + x) >> s;
      last[c] = mul[c];
      for (r=0; r <= !c; r++) {
	buf[c][1][width/2] = buf[c][2][width/2] = mul[c] << 7;
	for (tree=1, col=width/2; col > 0; ) {
	  if ((tree = radc_token(tree))) {
	    col -= 2;
	    if (tree == 8)
	      FORYX buf[c][y][x] = radc_token(tree+10) * mul[c];
	    else
	      FORYX buf[c][y][x] = radc_token(tree+10) * 16 + PREDICTOR;
	  } else
	    do {
	      nreps = (col > 2) ? radc_token(9) + 1 : 1;
	      for (rep=0; rep < 8 && rep < nreps && col > 0; rep++) {
		col -= 2;
		FORYX buf[c][y][x] = PREDICTOR;
		if (rep & 1) {
		  step = radc_token(10) << 4;
		  FORYX buf[c][y][x] += step;
		}
	      }
	    } while (nreps == 9);
	}
	for (y=0; y < 2; y++)
	  for (x=0; x < width/2; x++) {
	    val = (buf[c][y+1][x] << 4) / mul[c];
	    if (val < 0) val = 0;
	    if (c)
	      BAYER(row+y*2+c-1,x*2+2-c) = val;
	    else
	      BAYER(row+r*2+y,x*2+y) = val;
	  }
	memcpy (buf[c][0]+!c, buf[c][2], sizeof buf[c][0]-2*!c);
      }
    }
    for (y=row; y < row+4; y++)
      for (x=0; x < width; x++)
	if ((x+y) & 1) {
	  val = (BAYER(y,x)-2048)*2 + (BAYER(y,x-1)+BAYER(y,x+1))/2;
	  if (val < 0) val = 0;
	  BAYER(y,x) = val;
	}
  }
}

#undef FORYX
#undef PREDICTOR

#ifdef NO_JPEG
void CLASS kodak_jpeg_load_raw() {}
#else

METHODDEF(boolean)
fill_input_buffer (j_decompress_ptr cinfo)
{
  static char jpeg_buffer[4096];
  size_t nbytes;

  nbytes = fread (jpeg_buffer, 1, 4096, ifp);
  swab (jpeg_buffer, jpeg_buffer, nbytes);
  cinfo->src->next_input_byte = jpeg_buffer;
  cinfo->src->bytes_in_buffer = nbytes;
  return TRUE;
}

void CLASS kodak_jpeg_load_raw()
{
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr jerr;
  JSAMPARRAY buf;
  JSAMPLE (*pixel)[3];
  int row, col;

  cinfo.err = jpeg_std_error (&jerr);
  jpeg_create_decompress (&cinfo);
  jpeg_stdio_src (&cinfo, ifp);
  cinfo.src->fill_input_buffer = fill_input_buffer;
  jpeg_read_header (&cinfo, TRUE);
  jpeg_start_decompress (&cinfo);
  if ((cinfo.output_width      != width  ) ||
      (cinfo.output_height*2   != height ) ||
      (cinfo.output_components != 3      )) {
    fprintf (stderr, "%s: incorrect JPEG dimensions\n", ifname);
    jpeg_destroy_decompress (&cinfo);
    longjmp (failure, 3);
  }
  buf = (*cinfo.mem->alloc_sarray)
		((j_common_ptr) &cinfo, JPOOL_IMAGE, width*3, 1);

  while (cinfo.output_scanline < cinfo.output_height) {
    row = cinfo.output_scanline * 2;
    jpeg_read_scanlines (&cinfo, buf, 1);
    pixel = (void *) buf[0];
    for (col=0; col < width; col+=2) {
      BAYER(row+0,col+0) = pixel[col+0][1] << 6;
      BAYER(row+1,col+1) = pixel[col+1][1] << 6;
      BAYER(row+0,col+1) = (pixel[col][0] + pixel[col+1][0]) << 5;
      BAYER(row+1,col+0) = (pixel[col][2] + pixel[col+1][2]) << 5;
    }
  }
  jpeg_finish_decompress (&cinfo);
  jpeg_destroy_decompress (&cinfo);
}

#endif

void CLASS kodak_dc120_load_raw()
{
  static const int mul[4] = { 162, 192, 187,  92 };
  static const int add[4] = {   0, 636, 424, 212 };
  uchar pixel[848];
  int row, shift, col;

  for (row=0; row < height; row++)
  {
    fread (pixel, 848, 1, ifp);
    shift = row * mul[row & 3] + add[row & 3];
    for (col=0; col < width; col++)
      BAYER(row,col) = (ushort) pixel[(col + shift) % 848] << 6;
  }
}

void CLASS kodak_dc20_coeff (float juice)
{
  static const float my_coeff[3][4] =
  { {  2.25,  0.75, -1.75, -0.25 },
    { -0.25,  0.75,  0.75, -0.25 },
    { -0.25, -1.75,  0.75,  2.25 } };
  static const float flat[3][4] =
  { {  1, 0,   0,   0 },
    {  0, 0.5, 0.5, 0 },
    {  0, 0,   0,   1 } };
  int r, g;

  for (r=0; r < 3; r++)
    for (g=0; g < 4; g++)
      coeff[r][g] = my_coeff[r][g] * juice + flat[r][g] * (1-juice);
  use_coeff = 1;
}

void CLASS kodak_easy_load_raw()
{
  uchar *pixel;
  ushort curve[0x1000];
  unsigned row, col, icol;

  kodak_curve (curve);
  if (raw_width > width)
    black = 0;
  pixel = calloc (raw_width, sizeof *pixel);
  merror (pixel, "kodak_easy_load_raw()");
  for (row=0; row < height; row++) {
    fread (pixel, 1, raw_width, ifp);
    for (col=0; col < raw_width; col++) {
      icol = col - left_margin;
      if (icol < width)
	BAYER(row,icol) = (ushort) curve[pixel[col]] << 2;
      else
	black += curve[pixel[col]];
    }
  }
  if (raw_width > width)
    black = ((INT64) black << 2) / ((raw_width - width) * height);
  if (!strncmp(model,"DC2",3))
    black = 0;
  free (pixel);
}

void CLASS kodak_compressed_load_raw()
{
  uchar c, blen[256];
  ushort raw[6], curve[0x1000];
  unsigned row, col, len, save, i, israw=0, bits=0, pred[2];
  INT64 bitbuf=0;
  int diff;

  kodak_curve (curve);
  for (row=0; row < height; row++)
    for (col=0; col < width; col++)
    {
      if ((col & 255) == 0) {		/* Get the bit-lengths of the */
	len = width - col;		/* next 256 pixel values      */
	if (len > 256) len = 256;
	save = ftell(ifp);
	for (israw=i=0; i < len; i+=2) {
	  c = fgetc(ifp);
	  if ((blen[i+0] = c & 15) > 12 ||
	      (blen[i+1] = c >> 4) > 12 )
	    israw = 1;
	}
	bitbuf = bits = pred[0] = pred[1] = 0;
	if (len % 8 == 4) {
	  bitbuf  = fgetc(ifp) << 8;
	  bitbuf += fgetc(ifp);
	  bits = 16;
	}
	if (israw)
	  fseek (ifp, save, SEEK_SET);
      }
      if (israw) {			/* If the data is not compressed */
	switch (col & 7) {
	  case 0:
	    fread (raw, 2, 6, ifp);
	    for (i=0; i < 6; i++)
	      raw[i] = ntohs(raw[i]);
	    diff = raw[0] >> 12 << 8 | raw[2] >> 12 << 4 | raw[4] >> 12;
	    break;
	  case 1:
	    diff = raw[1] >> 12 << 8 | raw[3] >> 12 << 4 | raw[5] >> 12;
	    break;
	  default:
	    diff = raw[(col & 7) - 2] & 0xfff;
	}
      } else {				/* If the data is compressed */
	len = blen[col & 255];		/* Number of bits for this pixel */
	if (bits < len) {		/* Got enough bits in the buffer? */
	  for (i=0; i < 32; i+=8)
	    bitbuf += (INT64) fgetc(ifp) << (bits+(i^8));
	  bits += 32;
	}
	diff = bitbuf & (0xffff >> (16-len));  /* Pull bits from buffer */
	bitbuf >>= len;
	bits -= len;
	if ((diff & (1 << (len-1))) == 0)
	  diff -= (1 << len) - 1;
	pred[col & 1] += diff;
	diff = pred[col & 1];
      }
      BAYER(row,col) = curve[diff] << 2;
    }
}

void CLASS kodak_yuv_load_raw()
{
  uchar c, blen[384];
  unsigned row, col, len, bits=0;
  INT64 bitbuf=0;
  int i, li=0, si, diff, six[6], y[4], cb=0, cr=0, rgb[3];
  ushort *ip, curve[0x1000];

  kodak_curve (curve);
  for (row=0; row < height; row+=2)
    for (col=0; col < width; col+=2) {
      if ((col & 127) == 0) {
	len = (width - col + 1) * 3 & -4;
	if (len > 384) len = 384;
	for (i=0; i < len; ) {
	  c = fgetc(ifp);
	  blen[i++] = c & 15;
	  blen[i++] = c >> 4;
	}
	li = bitbuf = bits = y[1] = y[3] = cb = cr = 0;
	if (len % 8 == 4) {
	  bitbuf  = fgetc(ifp) << 8;
	  bitbuf += fgetc(ifp);
	  bits = 16;
	}
      }
      for (si=0; si < 6; si++) {
	len = blen[li++];
	if (bits < len) {
	  for (i=0; i < 32; i+=8)
	    bitbuf += (INT64) fgetc(ifp) << (bits+(i^8));
	  bits += 32;
	}
	diff = bitbuf & (0xffff >> (16-len));
	bitbuf >>= len;
	bits -= len;
	if ((diff & (1 << (len-1))) == 0)
	  diff -= (1 << len) - 1;
	six[si] = diff;
      }
      y[0] = six[0] + y[1];
      y[1] = six[1] + y[0];
      y[2] = six[2] + y[3];
      y[3] = six[3] + y[2];
      cb  += six[4];
      cr  += six[5];
      for (i=0; i < 4; i++) {
	ip = image[(row+(i >> 1))*width + col+(i & 1)];
	rgb[0] = y[i] + cr;
	rgb[1] = y[i];
	rgb[2] = y[i] + cb;
	for (c=0; c < 3; c++)
	  if (rgb[c] > 0) ip[c] = curve[rgb[c]] << 2;
      }
    }
}

void CLASS sony_decrypt (unsigned *data, int len, int start, int key)
{
  static unsigned pad[128], p;

  if (start) {
    for (p=0; p < 4; p++)
      pad[p] = key = key * 48828125 + 1;
    pad[3] = pad[3] << 1 | (pad[0]^pad[2]) >> 31;
    for (p=4; p < 127; p++)
      pad[p] = (pad[p-4]^pad[p-2]) << 1 | (pad[p-3]^pad[p-1]) >> 31;
    for (p=0; p < 127; p++)
      pad[p] = htonl(pad[p]);
  }
  while (len--)
    *data++ ^= pad[p++ & 127] = pad[(p+1) & 127] ^ pad[(p+65) & 127];
}

void CLASS sony_load_raw()
{
  uchar head[40];
  ushort pixel[3360];
  unsigned i, key, row, col, icol;
  INT64 bblack=0;

  fseek (ifp, 200896, SEEK_SET);
  fseek (ifp, (unsigned) fgetc(ifp)*4 - 1, SEEK_CUR);
  order = 0x4d4d;
  key = fget4(ifp);
  fseek (ifp, 164600, SEEK_SET);
  fread (head, 1, 40, ifp);
  sony_decrypt ((void *) head, 10, 1, key);
  for (i=26; i-- > 22; )
    key = key << 8 | head[i];
  fseek (ifp, 862144, SEEK_SET);
  for (row=0; row < height; row++) {
    fread (pixel, 2, raw_width, ifp);
    sony_decrypt ((void *) pixel, raw_width/2, !row, key);
    for (col=0; col < 3343; col++)
      if ((icol = col-left_margin) < width)
	BAYER(row,icol) = ntohs(pixel[col]);
      else
	bblack += ntohs(pixel[col]);
  }
  black = bblack / ((3343 - width) * height);
}

void CLASS sony_rgbe_coeff()
{
  int r, g;
  static const float my_coeff[3][4] =
  { {  1.321918,  0.000000,  0.149829, -0.471747 },
    { -0.288764,  1.129213, -0.486517,  0.646067 },
    {  0.061336, -0.199343,  1.138007,  0.000000 } };

  for (r=0; r < 3; r++)
    for (g=0; g < 4; g++)
      coeff[r][g] = my_coeff[r][g];
  use_coeff = 1;
}

void CLASS foveon_decoder (unsigned huff[1024], unsigned code)
{
  struct decode *cur;
  int i, len;

  cur = free_decode++;
  if (free_decode > first_decode+2048) {
    fprintf (stderr, "%s: decoder table overflow\n", ifname);
    longjmp (failure, 2);
  }
  if (code) {
    for (i=0; i < 1024; i++)
      if (huff[i] == code) {
	cur->leaf = i;
	return;
      }
  }
  if ((len = code >> 27) > 26) return;
  code = (len+1) << 27 | (code & 0x3ffffff) << 1;

  cur->branch[0] = free_decode;
  foveon_decoder (huff, code);
  cur->branch[1] = free_decode;
  foveon_decoder (huff, code+1);
}

void CLASS foveon_load_raw()
{
  struct decode *dindex;
  short diff[1024], pred[3];
  unsigned huff[1024], bitbuf=0;
  int row, col, bit=-1, c, i;

  fseek (ifp, 260, SEEK_SET);
  for (i=0; i < 1024; i++)
    diff[i] = fget2(ifp);
  for (i=0; i < 1024; i++)
    huff[i] = fget4(ifp);

  init_decoder();
  foveon_decoder (huff, 0);

  for (row=0; row < raw_height; row++) {
    memset (pred, 0, sizeof pred);
    if (!bit) fget4(ifp);
    for (col=bit=0; col < raw_width; col++) {
      for (c=0; c < 3; c++) {
	for (dindex=first_decode; dindex->branch[0]; ) {
	  if ((bit = (bit-1) & 31) == 31)
	    for (i=0; i < 4; i++)
	      bitbuf = (bitbuf << 8) + fgetc(ifp);
	  dindex = dindex->branch[bitbuf >> bit & 1];
	}
	pred[c] += diff[dindex->leaf];
      }
      if ((unsigned) (row-top_margin)  >= height ||
	  (unsigned) (col-left_margin) >= width ) continue;
      for (c=0; c < 3; c++)
	if (pred[c] > 0)
	  image[(row-top_margin)*width+(col-left_margin)][c] = pred[c];
    }
  }
}

int CLASS apply_curve (int i, const int *curve)
{
  if (i <= -curve[0])
    return -curve[curve[0]]-1;
  else if (i < 0)
    return -curve[1-i];
  else if (i < curve[0])
    return  curve[1+i];
  else
    return  curve[curve[0]]+1;
}

void CLASS foveon_interpolate()
{
  float mul[3] =
  { 1.0321, 1.0, 1.1124 };
  static const int weight[3][3][3] =
  { { {   4141,  37726,  11265  },
      { -30437,  16066, -41102  },
      {    326,   -413,    362  } },
    { {   1770,  -1316,   3480  },
      {  -2139,    213,  -4998  },
      {  -2381,   3496,  -2008  } },
    { {  -3838, -24025, -12968  },
      {  20144, -12195,  30272  },
      {   -631,  -2025,    822  } } },
  curve1[73] = { 72,
     0,1,2,2,3,4,5,6,6,7,8,9,9,10,11,11,12,13,13,14,14,
    15,16,16,17,17,18,18,18,19,19,20,20,20,21,21,21,22,
    22,22,23,23,23,23,23,24,24,24,24,24,25,25,25,25,25,
    25,25,25,26,26,26,26,26,26,26,26,26,26,26,26,26,26 },
  curve2[21] = { 20,
    0,1,1,2,3,3,4,4,5,5,6,6,6,7,7,7,7,7,7,7 },
  curve3[73] = { 72,
     0,1,1,2,2,3,4,4,5,5,6,6,7,7,8,8,8,9,9,10,10,10,10,
    11,11,11,12,12,12,12,12,12,13,13,13,13,13,13,13,13,
    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
    14,14,14,14,14,14,14,14,14,14,14,14,14,14,14 },
  curve4[37] = { 36,
    0,1,1,2,3,3,4,4,5,6,6,7,7,7,8,8,9,9,9,10,10,10,
    11,11,11,11,11,12,12,12,12,12,12,12,12,12 },
  curve5[111] = { 110,
    0,1,1,2,3,3,4,5,6,6,7,7,8,9,9,10,11,11,12,12,13,13,
    14,14,15,15,16,16,17,17,18,18,18,19,19,19,20,20,20,
    21,21,21,21,22,22,22,22,22,23,23,23,23,23,24,24,24,24,
    24,24,24,24,25,25,25,25,25,25,25,25,25,25,25,25,26,26,
    26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,
    26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26 },
  *curves[3] = { curve3, curve4, curve5 },
  trans[3][3] =
  { {   7576,  -2933,   1279  },
    { -11594,  29911, -12394  },
    {   4000, -18850,  20772  } };
  ushort *pix, prev[3], (*shrink)[3];
  int row, col, c, i, j, diff, sum, ipix[3], work[3][3], total[4];
  int (*smrow[7])[3], smlast, smred, smred_p=0, hood[7], min, max;

  /* Sharpen all colors */
  for (row=0; row < height; row++) {
    pix = image[row*width];
    memcpy (prev, pix, sizeof prev);
    for (col=0; col < width; col++) {
      for (c=0; c < 3; c++) {
	diff = pix[c] - prev[c];
	prev[c] = pix[c];
	ipix[c] = pix[c] + ((diff + (diff*diff >> 14)) * 0x3333 >> 14);
      }
      for (c=0; c < 3; c++) {
	work[0][c] = ipix[c]*ipix[c] >> 14;
	work[2][c] = ipix[c]*work[0][c] >> 14;
	work[1][2-c] = ipix[(c+1) % 3] * ipix[(c+2) % 3] >> 14;
      }
      for (c=0; c < 3; c++) {
	for (sum=i=0; i < 3; i++)
	  for (  j=0; j < 3; j++)
	    sum += weight[c][i][j] * work[i][j];
	ipix[c] = (ipix[c] + (sum >> 14)) * mul[c];
	if (ipix[c] < 0)     ipix[c] = 0;
	if (ipix[c] > 32000) ipix[c] = 32000;
	pix[c] = ipix[c];
      }
      pix += 4;
    }
  }
  /* Array for 5x5 Gaussian averaging of red values */
  smrow[6] = calloc (width*5, sizeof **smrow);
  merror (smrow[6], "foveon_interpolate()");
  for (i=0; i < 5; i++)
    smrow[i] = smrow[6] + i*width;

  /* Sharpen the reds against these Gaussian averages */
  for (smlast=-1, row=2; row < height-2; row++) {
    while (smlast < row+2) {
      for (i=0; i < 6; i++)
	smrow[(i+5) % 6] = smrow[i];
      pix = image[++smlast*width+2];
      for (col=2; col < width-2; col++) {
	smrow[4][col][0] =
	  (pix[0]*6 + (pix[-4]+pix[4])*4 + pix[-8]+pix[8] + 8) >> 4;
	pix += 4;
      }
    }
    pix = image[row*width+2];
    for (col=2; col < width-2; col++) {
      smred = (smrow[2][col][0]*6 + (smrow[1][col][0]+smrow[3][col][0])*4
		+ smrow[0][col][0]+smrow[4][col][0] + 8) >> 4;
      if (col == 2)
	smred_p = smred;
      i = pix[0] + ((pix[0] - ((smred*7 + smred_p) >> 3)) >> 2);
      if (i < 0)     i = 0;
      if (i > 10000) i = 10000;
      pix[0] = i;
      smred_p = smred;
      pix += 4;
    }
  }
  /* Limit each color value to the range of its neighbors */
  hood[0] = 4;
  for (i=0; i < 3; i++) {
    hood[i+1] = (i-width-1)*4;
    hood[i+4] = (i+width-1)*4;
  }
  for (row=1; row < height-1; row++) {
    pix = image[row*width+1];
    memcpy (prev, pix-4, sizeof prev);
    for (col=1; col < width-1; col++) {
      for (c=0; c < 3; c++) {
	for (min=max=prev[c], i=0; i < 7; i++) {
	  j = pix[hood[i]];
	  if (min > j) min = j;
	  if (max < j) max = j;
	}
	prev[c] = *pix;
	if (*pix < min) *pix = min;
	if (*pix > max) *pix = max;
	pix++;
      }
      pix++;
    }
  }
/*
   Because photons that miss one detector often hit another,
   the sum R+G+B is much less noisy than the individual colors.
   So smooth the hues without smoothing the total.
 */
  for (smlast=-1, row=2; row < height-2; row++) {
    while (smlast < row+2) {
      for (i=0; i < 6; i++)
	smrow[(i+5) % 6] = smrow[i];
      pix = image[++smlast*width+2];
      for (col=2; col < width-2; col++) {
	for (c=0; c < 3; c++)
	  smrow[4][col][c] = pix[c-8]+pix[c-4]+pix[c]+pix[c+4]+pix[c+8];
	pix += 4;
      }
    }
    pix = image[row*width+2];
    for (col=2; col < width-2; col++) {
      for (total[3]=1500, sum=60, c=0; c < 3; c++) {
	for (total[c]=i=0; i < 5; i++)
	  total[c] += smrow[i][col][c];
	total[3] += total[c];
	sum += pix[c];
      }
      j = (sum << 16) / total[3];
      for (c=0; c < 3; c++) {
	i = apply_curve ((total[c] * j >> 16) - pix[c], curve1);
	i += pix[c] - 13 - (c==1);
	ipix[c] = i - apply_curve (i, curve2);
      }
      sum = (ipix[0]+ipix[1]+ipix[1]+ipix[2]) >> 2;
      for (c=0; c < 3; c++) {
	i = ipix[c] - apply_curve (ipix[c] - sum, curve2);
	if (i < 0) i = 0;
	pix[c] = i;
      }
      pix += 4;
    }
  }
  /* Translate the image to a different colorspace */
  for (pix=image[0]; pix < image[height*width]; pix+=4) {
    for (c=0; c < 3; c++) {
      for (i=j=0; j < 3; j++)
	i += trans[c][j] * pix[j];
      i = (i+0x1000) >> 13;
      if (i < 0)     i = 0;
      if (i > 24000) i = 24000;
      ipix[c] = i;
    }
    for (c=0; c < 3; c++)
      pix[c] = ipix[c];
  }
  /* Smooth the image bottom-to-top and save at 1/4 scale */
  shrink = calloc ((width/4) * (height/4), sizeof *shrink);
  merror (shrink, "foveon_interpolate()");
  for (row = height/4; row--; )
    for (col=0; col < width/4; col++) {
      ipix[0] = ipix[1] = ipix[2] = 0;
      for (i=0; i < 4; i++)
	for (j=0; j < 4; j++)
	  for (c=0; c < 3; c++)
	    ipix[c] += image[(row*4+i)*width+col*4+j][c];
      for (c=0; c < 3; c++)
	if (row+2 > height/4)
	  shrink[row*(width/4)+col][c] = ipix[c] >> 4;
	else
	  shrink[row*(width/4)+col][c] =
	    (shrink[(row+1)*(width/4)+col][c]*1840 + ipix[c]*141) >> 12;
    }

  /* From the 1/4-scale image, smooth right-to-left */
  for (row=0; row < (height & ~3); row++) {
    ipix[0] = ipix[1] = ipix[2] = 0;
    if ((row & 3) == 0)
      for (col = width & ~3 ; col--; )
	for (c=0; c < 3; c++)
	  smrow[0][col][c] = ipix[c] =
	    (shrink[(row/4)*(width/4)+col/4][c]*1485 + ipix[c]*6707) >> 13;

  /* Then smooth left-to-right */
    ipix[0] = ipix[1] = ipix[2] = 0;
    for (col=0; col < (width & ~3); col++)
      for (c=0; c < 3; c++)
	smrow[1][col][c] = ipix[c] =
	  (smrow[0][col][c]*1485 + ipix[c]*6707) >> 13;

  /* Smooth top-to-bottom */
    if (row == 0)
      memcpy (smrow[2], smrow[1], sizeof **smrow * width);
    else
      for (col=0; col < (width & ~3); col++)
	for (c=0; c < 3; c++)
	  smrow[2][col][c] =
	    (smrow[2][col][c]*6707 + smrow[1][col][c]*1485) >> 13;

  /* Adjust the chroma toward the smooth values */
    for (col=0; col < (width & ~3); col++) {
      for (i=j=60, c=0; c < 3; c++) {
	i += smrow[2][col][c];
	j += image[row*width+col][c];
      }
      j = (j << 16) / i;
      for (sum=c=0; c < 3; c++) {
	i = (smrow[2][col][c] * j >> 16) - image[row*width+col][c];
	ipix[c] = apply_curve (i, curves[c]);
	sum += ipix[c];
      }
      sum >>= 3;
      for (c=0; c < 3; c++) {
	i = image[row*width+col][c] + ipix[c] - sum;
	if (i < 0) i = 0;
	image[row*width+col][c] = i;
      }
    }
  }
  free (shrink);
  free (smrow[6]);
}

/*
   Seach from the current directory up to the root looking for
   a ".badpixels" file, and fix those pixels now.
 */
void CLASS bad_pixels()
{
  FILE *fp=NULL;
  char *fname, *cp, line[128];
  int len, time, row, col, r, c, rad, tot, n, fixed=0;

  if (!filters) return;
  for (len=16 ; ; len *= 2) {
    fname = malloc (len);
    if (!fname) return;
    if (getcwd (fname, len-12)) break;
    free (fname);
    if (errno != ERANGE) return;
  }
#ifdef WIN32
  if (fname[1] == ':')
    memmove (fname, fname+2, len-2);
  for (cp=fname; *cp; cp++)
    if (*cp == '\\') *cp = '/';
#endif
  cp = fname + strlen(fname);
  if (cp[-1] == '/') cp--;
  while (*fname == '/') {
    strcpy (cp, "/.badpixels");
    if ((fp = fopen (fname, "r"))) break;
    if (cp == fname) break;
    while (*--cp != '/');
  }
  free (fname);
  if (!fp) return;
  while (fgets (line, 128, fp)) {
    cp = strchr (line, '#');
    if (cp) *cp = 0;
    if (sscanf (line, "%d %d %d", &col, &row, &time) != 3) continue;
    if ((unsigned) col >= width || (unsigned) row >= height) continue;
    if (time > timestamp) continue;
    for (tot=n=0, rad=1; rad < 3 && n==0; rad++)
      for (r = row-rad; r <= row+rad; r++)
	for (c = col-rad; c <= col+rad; c++)
	  if ((unsigned) r < height && (unsigned) c < width &&
		(r != row || c != col) && FC(r,c) == FC(row,col)) {
	    tot += BAYER(r,c);
	    n++;
	  }
    BAYER(row,col) = tot/n;
    if (verbose) {
      if (!fixed++)
	fprintf (stderr, "Fixed bad pixels at:");
      fprintf (stderr, " %d,%d", col, row);
    }
  }
  if (fixed) fputc ('\n', stderr);
  fclose (fp);
}

void CLASS scale_colors()
{
  int row, col, c, val;
  int min[4], max[4], count[4];
  double sum[4], dmin, dmax;

  rgb_max -= black;
  if (use_auto_wb || (use_camera_wb && camera_red == -1)) {
    for (c=0; c < 4; c++) {
      min[c] = INT_MAX;
      max[c] = count[c] = sum[c] = 0;
    }
    for (row=0; row < height; row++)
      for (col=0; col < width; col++)
	for (c=0; c < colors; c++) {
	  val = image[row*width+col][c];
	  if (!val) continue;
	  if (min[c] > val) min[c] = val;
	  if (max[c] < val) max[c] = val;
	  val -= black;
	  if (val > rgb_max-100) continue;
	  if (val < 0) val = 0;
	  sum[c] += val;
	  count[c]++;
	}
    for (dmax=c=0; c < colors; c++) {
      sum[c] /= count[c];
      if (dmax < sum[c]) dmax = sum[c];
    }
    for (c=0; c < colors; c++)
      pre_mul[c] = dmax / sum[c];
  }
  if (use_camera_wb && camera_red != -1) {
    for (c=0; c < 4; c++)
      count[c] = sum[c] = 0;
    for (row=0; row < 8; row++)
      for (col=0; col < 8; col++) {
	c = FC(row,col);
	if ((val = white[row][col] - black) > 0)
	  sum[c] += val;
	count[c]++;
      }
    for (dmin=DBL_MAX, dmax=c=0; c < colors; c++) {
      sum[c] /= count[c];
      if (dmin > sum[c]) dmin = sum[c];
      if (dmax < sum[c]) dmax = sum[c];
    }
    if (dmin > 0)
      for (c=0; c < colors; c++)
	pre_mul[c] = dmax / sum[c];
    else if (camera_red && camera_blue) {
      pre_mul[0] = camera_red;
      pre_mul[2] = camera_blue;
      pre_mul[1] = pre_mul[3] = 1.0;
    } else
      fprintf (stderr, "%s: Cannot use camera white balance.\n", ifname);
  }
  if (!use_coeff) {
    pre_mul[0] *= red_scale;
    pre_mul[2] *= blue_scale;
  }
  if (verbose) {
    fprintf (stderr, "Scaling with black=%d, pre_mul[] =", black);
    for (c=0; c < colors; c++)
      fprintf (stderr, " %f", pre_mul[c]);
    fputc ('\n', stderr);
  }
  for (row=0; row < height; row++)
    for (col=0; col < width; col++)
      for (c=0; c < colors; c++) {
	val = image[row*width+col][c];
	if (!val) continue;
	val -= black;
	val *= pre_mul[c];
	if (val < 0) val = 0;
	if (val > rgb_max) val = rgb_max;
	image[row*width+col][c] = val;
      }
}

/*
   This algorithm is officially called:

   "Interpolation using a Threshold-based variable number of gradients"

   described in http://www-ise.stanford.edu/~tingchen/algodep/vargra.html

   I've extended the basic idea to work with non-Bayer filter arrays.
   Gradients are numbered clockwise from NW=0 to W=7.
 */
void CLASS vng_interpolate()
{
  static const signed char *cp, terms[] = {
    -2,-2,+0,-1,0,0x01, -2,-2,+0,+0,1,0x01, -2,-1,-1,+0,0,0x01,
    -2,-1,+0,-1,0,0x02, -2,-1,+0,+0,0,0x03, -2,-1,+0,+1,1,0x01,
    -2,+0,+0,-1,0,0x06, -2,+0,+0,+0,1,0x02, -2,+0,+0,+1,0,0x03,
    -2,+1,-1,+0,0,0x04, -2,+1,+0,-1,1,0x04, -2,+1,+0,+0,0,0x06,
    -2,+1,+0,+1,0,0x02, -2,+2,+0,+0,1,0x04, -2,+2,+0,+1,0,0x04,
    -1,-2,-1,+0,0,0x80, -1,-2,+0,-1,0,0x01, -1,-2,+1,-1,0,0x01,
    -1,-2,+1,+0,1,0x01, -1,-1,-1,+1,0,0x88, -1,-1,+1,-2,0,0x40,
    -1,-1,+1,-1,0,0x22, -1,-1,+1,+0,0,0x33, -1,-1,+1,+1,1,0x11,
    -1,+0,-1,+2,0,0x08, -1,+0,+0,-1,0,0x44, -1,+0,+0,+1,0,0x11,
    -1,+0,+1,-2,1,0x40, -1,+0,+1,-1,0,0x66, -1,+0,+1,+0,1,0x22,
    -1,+0,+1,+1,0,0x33, -1,+0,+1,+2,1,0x10, -1,+1,+1,-1,1,0x44,
    -1,+1,+1,+0,0,0x66, -1,+1,+1,+1,0,0x22, -1,+1,+1,+2,0,0x10,
    -1,+2,+0,+1,0,0x04, -1,+2,+1,+0,1,0x04, -1,+2,+1,+1,0,0x04,
    +0,-2,+0,+0,1,0x80, +0,-1,+0,+1,1,0x88, +0,-1,+1,-2,0,0x40,
    +0,-1,+1,+0,0,0x11, +0,-1,+2,-2,0,0x40, +0,-1,+2,-1,0,0x20,
    +0,-1,+2,+0,0,0x30, +0,-1,+2,+1,1,0x10, +0,+0,+0,+2,1,0x08,
    +0,+0,+2,-2,1,0x40, +0,+0,+2,-1,0,0x60, +0,+0,+2,+0,1,0x20,
    +0,+0,+2,+1,0,0x30, +0,+0,+2,+2,1,0x10, +0,+1,+1,+0,0,0x44,
    +0,+1,+1,+2,0,0x10, +0,+1,+2,-1,1,0x40, +0,+1,+2,+0,0,0x60,
    +0,+1,+2,+1,0,0x20, +0,+1,+2,+2,0,0x10, +1,-2,+1,+0,0,0x80,
    +1,-1,+1,+1,0,0x88, +1,+0,+1,+2,0,0x08, +1,+0,+2,-1,0,0x40,
    +1,+0,+2,+1,0,0x10
  }, chood[] = { -1,-1, -1,0, -1,+1, 0,+1, +1,+1, +1,0, +1,-1, 0,-1 };
  ushort (*brow[5])[4], *pix;
  int code[8][2][320], *ip, gval[8], gmin, gmax, sum[4];
  int row, col, shift, x, y, x1, x2, y1, y2, t, weight, grads, color, diag;
  int g, diff, thold, num, c;

  for (row=0; row < 8; row++) {		/* Precalculate for bilinear */
    for (col=1; col < 3; col++) {
      ip = code[row][col & 1];
      memset (sum, 0, sizeof sum);
      for (y=-1; y <= 1; y++)
	for (x=-1; x <= 1; x++) {
	  shift = (y==0) + (x==0);
	  if (shift == 2) continue;
	  color = FC(row+y,col+x);
	  *ip++ = (width*y + x)*4 + color;
	  *ip++ = shift;
	  *ip++ = color;
	  sum[color] += 1 << shift;
	}
      for (c=0; c < colors; c++)
	if (c != FC(row,col)) {
	  *ip++ = c;
	  *ip++ = sum[c];
	}
    }
  }
  for (row=1; row < height-1; row++) {	/* Do bilinear interpolation */
    for (col=1; col < width-1; col++) {
      pix = image[row*width+col];
      ip = code[row & 7][col & 1];
      memset (sum, 0, sizeof sum);
      for (g=8; g--; ) {
	diff = pix[*ip++];
	diff <<= *ip++;
	sum[*ip++] += diff;
      }
      for (g=colors; --g; ) {
	c = *ip++;
	pix[c] = sum[c] / *ip++;
      }
    }
  }
  if (quick_interpolate)
    return;

  for (row=0; row < 8; row++) {		/* Precalculate for VNG */
    for (col=0; col < 2; col++) {
      ip = code[row][col];
      for (cp=terms, t=0; t < 64; t++) {
	y1 = *cp++;  x1 = *cp++;
	y2 = *cp++;  x2 = *cp++;
	weight = *cp++;
	grads = *cp++;
	color = FC(row+y1,col+x1);
	if (FC(row+y2,col+x2) != color) continue;
	diag = (FC(row,col+1) == color && FC(row+1,col) == color) ? 2:1;
	if (abs(y1-y2) == diag && abs(x1-x2) == diag) continue;
	*ip++ = (y1*width + x1)*4 + color;
	*ip++ = (y2*width + x2)*4 + color;
	*ip++ = weight;
	for (g=0; g < 8; g++)
	  if (grads & 1<<g) *ip++ = g;
	*ip++ = -1;
      }
      *ip++ = INT_MAX;
      for (cp=chood, g=0; g < 8; g++) {
	y = *cp++;  x = *cp++;
	*ip++ = (y*width + x) * 4;
	color = FC(row,col);
	if (FC(row+y,col+x) != color && FC(row+y*2,col+x*2) == color)
	  *ip++ = (y*width + x) * 8 + color;
	else
	  *ip++ = 0;
      }
    }
  }
  brow[4] = calloc (width*3, sizeof **brow);
  merror (brow[4], "vng_interpolate()");
  for (row=0; row < 3; row++)
    brow[row] = brow[4] + row*width;
  for (row=2; row < height-2; row++) {		/* Do VNG interpolation */
    for (col=2; col < width-2; col++) {
      pix = image[row*width+col];
      ip = code[row & 7][col & 1];
      memset (gval, 0, sizeof gval);
      while ((g = ip[0]) != INT_MAX) {		/* Calculate gradients */
	num = (diff = pix[g] - pix[ip[1]]) >> 31;
	gval[ip[3]] += (diff = ((diff ^ num) - num) << ip[2]);
	ip += 5;
	if ((g = ip[-1]) == -1) continue;
	gval[g] += diff;
	while ((g = *ip++) != -1)
	  gval[g] += diff;
      }
      ip++;
      gmin = gmax = gval[0];			/* Choose a threshold */
      for (g=1; g < 8; g++) {
	if (gmin > gval[g]) gmin = gval[g];
	if (gmax < gval[g]) gmax = gval[g];
      }
      if (gmax == 0) {
	memcpy (brow[2][col], pix, sizeof *image);
	continue;
      }
      thold = gmin + (gmax >> 1);
      memset (sum, 0, sizeof sum);
      color = FC(row,col);
      for (num=g=0; g < 8; g++,ip+=2) {		/* Average the neighbors */
	if (gval[g] <= thold) {
	  for (c=0; c < colors; c++)
	    if (c == color && ip[1])
	      sum[c] += (pix[c] + pix[ip[1]]) >> 1;
	    else
	      sum[c] += pix[ip[0] + c];
	  num++;
	}
      }
      for (c=0; c < colors; c++) {		/* Save to buffer */
	t = pix[color];
	if (c != color) {
	  t += (sum[c] - sum[color])/num;
	  if (t < 0) t = 0;
	  if (t > rgb_max) t = rgb_max;
	}
	brow[2][col][c] = t;
      }
    }
    if (row > 3)				/* Write buffer to image */
      memcpy (image[(row-2)*width+2], brow[0]+2, (width-4)*sizeof *image);
    for (g=0; g < 4; g++)
      brow[(g-1) & 3] = brow[g];
  }
  memcpy (image[(row-2)*width+2], brow[0]+2, (width-4)*sizeof *image);
  memcpy (image[(row-1)*width+2], brow[1]+2, (width-4)*sizeof *image);
  free (brow[4]);
}

void CLASS tiff_parse_subifd (int base)
{
  int entries, tag, type, len, val, save;

  entries = fget2(ifp);
  while (entries--) {
    tag  = fget2(ifp);
    type = fget2(ifp);
    len  = fget4(ifp);
    if (type == 3 && len < 3) {
      val = fget2(ifp);  fget2(ifp);
    } else
      val = fget4(ifp);
    switch (tag) {
      case 0x100:		/* ImageWidth */
	raw_width = val;
	break;
      case 0x101:		/* ImageHeight */
	raw_height = val;
	break;
      case 0x102:		/* Bits per sample */
	break;
      case 0x103:		/* Compression */
	tiff_data_compression = val;
	break;
      case 0x106:		/* Kodak color format */
	kodak_data_compression = val;
	break;
      case 0x111:		/* StripOffset */
	if (len == 1)
	  data_offset = val;
	else {
	  save = ftell(ifp);
	  fseek (ifp, val+base, SEEK_SET);
	  data_offset = fget4(ifp);
	  fseek (ifp, save, SEEK_SET);
	}
	break;
      case 0x115:		/* SamplesPerRow */
	break;
      case 0x116:		/* RowsPerStrip */
	break;
      case 0x117:		/* StripByteCounts */
	break;
      case 0x123:
	curve_offset = val;
	curve_length = len;
    }
  }
}

void CLASS nef_parse_makernote()
{
  int base=0, offset=0, entries, tag, type, len, val, save;
  short sorder;
  char buf[10];

/*
   The MakerNote might have its own TIFF header (possibly with
   its own byte-order!), or it might just be a table.
 */
  sorder = order;
  fread (buf, 1, 10, ifp);
  if (!strcmp (buf,"Nikon")) {	/* starts with "Nikon\0\2\0\0\0" ? */
    base = ftell(ifp);
    order = fget2(ifp);		/* might differ from file-wide byteorder */
    val = fget2(ifp);		/* should be 42 decimal */
    offset = fget4(ifp);
    fseek (ifp, offset-8, SEEK_CUR);
  } else if (!strcmp (buf,"OLYMP"))
    fseek (ifp, -2, SEEK_CUR);
  else
    fseek (ifp, -10, SEEK_CUR);

  entries = fget2(ifp);
  while (entries--) {
    tag  = fget2(ifp);
    type = fget2(ifp);
    len  = fget4(ifp);
    if (type == 3) {            /* short int */
      val = fget2(ifp);  fget2(ifp);
    } else
      val = fget4(ifp);
    save = ftell(ifp);
    if (tag == 0xc) {
      fseek (ifp, base+val, SEEK_SET);
      camera_red  = fget4(ifp);
      camera_red /= fget4(ifp);
      camera_blue = fget4(ifp);
      camera_blue/= fget4(ifp);
    }
    if (tag == 0x8c)
      curve_offset = base+val + 2112;
    if (tag == 0x96)
      curve_offset = base+val + 2;
    if (tag == 0x97) {
      if (!strcmp(model,"NIKON D100 ")) {
	fseek (ifp, base+val + 72, SEEK_SET);
	camera_red  = fget2(ifp) / 256.0;
	camera_blue = fget2(ifp) / 256.0;
      } else if (!strcmp(model,"NIKON D2H")) {
	fseek (ifp, base+val + 10, SEEK_SET);
	camera_red  = fget2(ifp);
	camera_red /= fget2(ifp);
	camera_blue = fget2(ifp);
	camera_blue = fget2(ifp) / camera_blue;
      } else if (!strcmp(model,"NIKON D70")) {
	fseek (ifp, base+val + 20, SEEK_SET);
	camera_red  = fget2(ifp);
	camera_red /= fget2(ifp);
	camera_blue = fget2(ifp);
	camera_blue/= fget2(ifp);
      }
    }
    if (tag == 0x1017)		/* Olympus */
      camera_red  = val / 256.0;
    if (tag == 0x1018)
      camera_blue = val / 256.0;
    fseek (ifp, save, SEEK_SET);
  }
  order = sorder;
}

/*
   Since the TIFF DateTime string has no timezone information,
   assume that the camera's clock was set to Universal Time.
 */
void CLASS get_timestamp()
{
  struct tm t;
  time_t ts;

  if (fscanf (ifp, "%d:%d:%d %d:%d:%d", &t.tm_year, &t.tm_mon,
	&t.tm_mday, &t.tm_hour, &t.tm_min, &t.tm_sec) != 6)
    return;
  t.tm_year -= 1900;
  t.tm_mon -= 1;
  putenv ("TZ=");		/* Remove this to assume local time */
  if ((ts = mktime(&t)) > 0)
    timestamp = ts;
}

void CLASS nef_parse_exif (int base)
{
  int entries, tag, type, len, val, save;

  entries = fget2(ifp);
  while (entries--) {
    tag  = fget2(ifp);
    type = fget2(ifp);
    len  = fget4(ifp);
    val  = fget4(ifp);
    save = ftell(ifp);
    fseek (ifp, val+base, SEEK_SET);
    if (tag == 0x9003 || tag == 0x9004)
      get_timestamp();
    if (tag == 0x927c &&
	(!strncmp(make,"NIKON",5) || !strncmp(make,"OLYMPUS",7)))
      nef_parse_makernote();
    fseek (ifp, save, SEEK_SET);
  }
}

/*
   Parse a TIFF file looking for camera model and decompress offsets.
 */
void CLASS parse_tiff (int base)
{
  int doff, entries, tag, type, len, val, save;
  char software[64];
  int wide=0, high=0, cr2_offset=0, offset=0;
  static const int flip_map[] = { 0,1,3,2,4,6,7,5 };

  fseek (ifp, base, SEEK_SET);
  order = fget2(ifp);
  val = fget2(ifp);		/* Should be 42 for standard TIFF */
  while ((doff = fget4(ifp))) {
    fseek (ifp, doff+base, SEEK_SET);
    entries = fget2(ifp);
    while (entries--) {
      tag  = fget2(ifp);
      type = fget2(ifp);
      len  = fget4(ifp);
      if (type == 3 && len < 3) {
	val = fget2(ifp);  fget2(ifp);
      } else
	val = fget4(ifp);
      save = ftell(ifp);
      fseek (ifp, val+base, SEEK_SET);
      switch (tag) {
	case 0x11:
	  camera_red  = val / 256.0;
	  break;
	case 0x12:
	  camera_blue = val / 256.0;
	  break;
	case 0x100:			/* ImageWidth */
	  wide = val;
	  break;
	case 0x101:			/* ImageHeight */
	  high = val;
	  break;
	case 0x10f:			/* Make tag */
	  fgets (make, 64, ifp);
	  break;
	case 0x110:			/* Model tag */
	  fgets (model, 64, ifp);
	  break;
	case 0x111:			/* StripOffset */
	  cr2_offset = val;
	  offset = fget4(ifp);
	  break;
	case 0x112:			/* Rotation */
	  flip = flip_map[(val-1) & 7];
	  break;
	case 0x123:
	  curve_offset = val;
	  curve_length = len;
	  break;
	case 0x827d:			/* Model2 tag */
	  fgets (model2, 64, ifp);
	  break;
	case 0x131:			/* Software tag */
	  fgets (software, 64, ifp);
	  if (!strncmp(software,"Adobe",5))
	    make[0] = 0;
	  break;
	case 0x132:			/* DateTime tag */
	  get_timestamp();
	  break;
	case 0x144:
	  strcpy (make, "Leaf");
	  raw_width  = wide;
	  raw_height = high;
	  if (len > 1)
	    data_offset = fget4(ifp);
	  else
	    data_offset = val;
	  break;
	case 0x14a:			/* SubIFD tag */
	  if (len > 2 && !strcmp(make,"Kodak"))
	      len = 2;
	  if (len > 1)
	    while (len--) {
	      fseek (ifp, val+base, SEEK_SET);
	      fseek (ifp, fget4(ifp)+base, SEEK_SET);
	      tiff_parse_subifd(base);
	      val += 4;
	    }
	  else
	    tiff_parse_subifd(base);
	  break;
	case 0x8769:			/* Nikon EXIF tag */
	  nef_parse_exif(base);
	  break;
      }
      fseek (ifp, save, SEEK_SET);
    }
  }
  if (!strncmp(make,"OLYMPUS",7)) {
    make[7] = 0;
    raw_width = wide;
    raw_height = - (-high & -2);
    data_offset = offset;
  }
  if (!strcmp(make,"Canon") && strcmp(model,"EOS D2000C"))
    data_offset = cr2_offset;

  if (make[0] == 0 && wide == 680 && high == 680) {
    strcpy (make, "Imacon");
    strcpy (model,"Ixpress");
  }
}

/*
   CIFF block 0x1030 contains an 8x8 white sample.
   Load this into white[][] for use in scale_colors().
 */
void CLASS ciff_block_1030()
{
  static const ushort key[] = { 0x410, 0x45f3 };
  int i, bpp, row, col, vbits=0;
  unsigned long bitbuf=0;

  fget2(ifp);
  if (fget4(ifp) != 0x80008) return;
  if (fget4(ifp) == 0) return;
  bpp = fget2(ifp);
  if (bpp != 10 && bpp != 12) return;
  for (i=row=0; row < 8; row++)
    for (col=0; col < 8; col++) {
      if (vbits < bpp) {
	bitbuf = bitbuf << 16 | (fget2(ifp) ^ key[i++ & 1]);
	vbits += 16;
      }
      white[row][col] =
	bitbuf << (LONG_BIT - vbits) >> (LONG_BIT - bpp) << (14-bpp);
      vbits -= bpp;
    }
}

/*
   Parse the CIFF structure looking for two pieces of information:
   The camera model, and the decode table number.
 */
void CLASS parse_ciff (int offset, int length)
{
  int tboff, nrecs, i, type, len, roff, aoff, save, wbi=-1;
  static const int remap[] = { 1,2,3,4,5,1 };
  static const int remap_10d[] = { 0,1,3,4,5,6,0,0,2,8 };

  fseek (ifp, offset+length-4, SEEK_SET);
  tboff = fget4(ifp) + offset;
  fseek (ifp, tboff, SEEK_SET);
  nrecs = fget2(ifp);
  for (i=0; i < nrecs; i++) {
    type = fget2(ifp);
    len  = fget4(ifp);
    roff = fget4(ifp);
    aoff = offset + roff;
    save = ftell(ifp);
    if (type == 0x080a) {		/* Get the camera make and model */
      fseek (ifp, aoff, SEEK_SET);
      fread (make, 64, 1, ifp);
      fseek (ifp, aoff+strlen(make)+1, SEEK_SET);
      fread (model, 64, 1, ifp);
    }
    if (type == 0x102a) {		/* Find the White Balance index */
      fseek (ifp, aoff+14, SEEK_SET);	/* 0=auto, 1=daylight, 2=cloudy ... */
      wbi = fget2(ifp);
      if (((!strcmp(model,"Canon EOS DIGITAL REBEL") ||
	    !strcmp(model,"Canon EOS 300D DIGITAL"))) && wbi == 6)
	wbi++;
    }
    if (type == 0x102c) {		/* Get white balance (G2) */
      if (!strcmp(model,"Canon PowerShot G1") ||
	  !strcmp(model,"Canon PowerShot Pro90 IS")) {
	fseek (ifp, aoff+120, SEEK_SET);
	white[0][1] = fget2(ifp) << 4;
	white[0][0] = fget2(ifp) << 4;
	white[1][0] = fget2(ifp) << 4;
	white[1][1] = fget2(ifp) << 4;
      } else {
	fseek (ifp, aoff+100, SEEK_SET);
	if (wbi == 6 && fget4(ifp) == 0)
	  wbi = 15;
	else {
	  fseek (ifp, aoff+100, SEEK_SET);
	  goto common;
	}
      }
    }
    if (type == 0x0032) {		/* Get white balance (D30 & G3) */
      if (!strcmp(model,"Canon EOS D30")) {
	fseek (ifp, aoff+72, SEEK_SET);
common:
	camera_red   = fget2(ifp);
	camera_red   = fget2(ifp) / camera_red;
	camera_blue  = fget2(ifp);
	camera_blue /= fget2(ifp);
      } else {
	fseek (ifp, aoff+80 + (wbi < 6 ? remap[wbi]*8 : 0), SEEK_SET);
	if (!camera_red)
	  goto common;
      }
    }
    if (type == 0x10a9) {		/* Get white balance (D60) */
      if (!strcmp(model,"Canon EOS 10D"))
	wbi = remap_10d[wbi];
      fseek (ifp, aoff+2 + wbi*8, SEEK_SET);
      camera_red  = fget2(ifp);
      camera_red /= fget2(ifp);
      camera_blue = fget2(ifp);
      camera_blue = fget2(ifp) / camera_blue;
    }
    if (type == 0x1030 && wbi > 14) {	/* Get white sample */
      fseek (ifp, aoff, SEEK_SET);
      ciff_block_1030();
    }
    if (type == 0x1031) {		/* Get the raw width and height */
      fseek (ifp, aoff+2, SEEK_SET);
      raw_width  = fget2(ifp);
      raw_height = fget2(ifp);
    }
    if (type == 0x180e) {		/* Get the timestamp */
      fseek (ifp, aoff, SEEK_SET);
      timestamp = fget4(ifp);
    }
    if (type == 0x580e)
      timestamp = len;
    if (type == 0x1810) {		/* Get the rotation */
      fseek (ifp, aoff+12, SEEK_SET);
      switch (fget4(ifp)) {
	case 270:  flip = 5;  break;
	case 180:  flip = 3;  break;
	case  90:  flip = 6;
      }
    }
    if (type == 0x1835) {		/* Get the decoder table */
      fseek (ifp, aoff, SEEK_SET);
      crw_init_tables (fget4(ifp));
    }
    if (type >> 8 == 0x28 || type >> 8 == 0x30)	/* Get sub-tables */
      parse_ciff(aoff, len);
    fseek (ifp, save, SEEK_SET);
  }
  if (wbi == 0 && !strcmp(model,"Canon EOS D30"))
    camera_red = -1;			/* Use my auto WB for this photo */
}

void CLASS parse_rollei()
{
  char line[128], *val;
  int tx=0, ty=0;
  struct tm t;
  time_t ts;

  fseek (ifp, 0, SEEK_SET);
  do {
    fgets (line, 128, ifp);
    if ((val = strchr(line,'=')))
      *val++ = 0;
    else
      val = line + strlen(line);
    if (!strcmp(line,"DAT"))
      sscanf (val, "%d.%d.%d", &t.tm_mday, &t.tm_mon, &t.tm_year);
    if (!strcmp(line,"TIM"))
      sscanf (val, "%d:%d:%d", &t.tm_hour, &t.tm_min, &t.tm_sec);
    if (!strcmp(line,"HDR"))
      data_offset = atoi(val);
    if (!strcmp(line,"X  "))
      raw_width = atoi(val);
    if (!strcmp(line,"Y  "))
      raw_height = atoi(val);
    if (!strcmp(line,"TX "))
      tx = atoi(val);
    if (!strcmp(line,"TY "))
      ty = atoi(val);
  } while (strncmp(line,"EOHD",4));
  t.tm_year -= 1900;
  t.tm_mon -= 1;
  putenv ("TZ=");
  if ((ts = mktime(&t)) > 0)
    timestamp = ts;
  data_offset += tx * ty * 2;
  strcpy (make, "Rollei");
  strcpy (model,"d530flex");
}

void CLASS parse_foveon()
{
  char *buf, *bp, *np;
  int off1, off2, len, i;

  order = 0x4949;			/* Little-endian */
  fseek (ifp, -4, SEEK_END);
  off2 = fget4(ifp);
  fseek (ifp, off2, SEEK_SET);
  while (fget4(ifp) != 0x464d4143)	/* Search for "CAMF" */
    if (feof(ifp)) return;
  off1 = fget4(ifp);
  fseek (ifp, off1+8, SEEK_SET);
  off1 += (fget4(ifp)+3) * 8;
  len = (off2 - off1)/2;
  fseek (ifp, off1, SEEK_SET);
  buf = malloc (len);
  merror (buf, "parse_foveon()");
  for (i=0; i < len; i++)		/* Convert Unicode to ASCII */
    buf[i] = fget2(ifp);
  for (bp=buf; bp < buf+len; bp=np) {
    np = bp + strlen(bp) + 1;
    if (!strcmp(bp,"CAMMANUF"))
      strcpy (make, np);
    if (!strcmp(bp,"CAMMODEL"))
      strcpy (model, np);
    if (!strcmp(bp,"TIME"))
      timestamp = atoi(np);
  }
  fseek (ifp, 248, SEEK_SET);
  raw_width  = fget4(ifp);
  raw_height = fget4(ifp);
  free (buf);
}

void CLASS foveon_coeff()
{
  static const float foveon[3][3] = {
    {  2.0343955, -0.727533, -0.3067457 },
    { -0.2287194,  1.231793, -0.0028293 },
    { -0.0086152, -0.153336,  1.1617814 }
  };
  int i, j;

  for (i=0; i < 3; i++)
    for (j=0; j < 3; j++)
      coeff[i][j] = foveon[i][j] * pre_mul[i];
  use_coeff = 1;
}

/*
   The grass is always greener in my PowerShot G2 when this
   function is called.  Use at your own risk!
 */
void CLASS canon_rgb_coeff (float juice)
{
  static const float my_coeff[3][3] =
  { {  1.116187, -0.107427, -0.008760 },
    { -1.551374,  4.157144, -1.605770 },
    {  0.090939, -0.399727,  1.308788 } };
  int i, j;

  for (i=0; i < 3; i++)
    for (j=0; j < 3; j++)
      coeff[i][j] = my_coeff[i][j] * juice + (i==j) * (1-juice);
  use_coeff = 1;
}

void CLASS nikon_e950_coeff()
{
  int r, g;
  static const float my_coeff[3][4] =
  { { -1.936280,  1.800443, -1.448486,  2.584324 },
    {  1.405365, -0.524955, -0.289090,  0.408680 },
    { -1.204965,  1.082304,  2.941367, -1.818705 } };

  for (r=0; r < 3; r++)
    for (g=0; g < 4; g++)
      coeff[r][g] = my_coeff[r][g];
  use_coeff = 1;
}

/*
   Given a matrix that converts RGB to GMCY, create a matrix to do
   the opposite.  Only square matrices can be inverted, so I create
   four 3x3 matrices by omitting a different GMCY color in each one.
   The final coeff[][] matrix is the sum of these four.
 */
void CLASS gmcy_coeff()
{
  static const float gmcy[4][3] = {
/*    red  green  blue			   */
    { 0.11, 0.86, 0.08 },	/* green   */
    { 0.50, 0.29, 0.51 },	/* magenta */
    { 0.11, 0.92, 0.75 },	/* cyan    */
    { 0.81, 0.98, 0.08 }	/* yellow  */
  };
  double invert[3][6], num;
  int ignore, i, j, k, r, g;

  memset (coeff, 0, sizeof coeff);
  for (ignore=0; ignore < 4; ignore++) {
    for (j=0; j < 3; j++) {
      g = (j < ignore) ? j : j+1;
      for (r=0; r < 3; r++) {
	invert[j][r] = gmcy[g][r];	/* 3x3 matrix to invert */
	invert[j][r+3] = (r == j);	/* Identity matrix	*/
      }
    }
    for (j=0; j < 3; j++) {
      num = invert[j][j];		/* Normalize this row	*/
      for (i=0; i < 6; i++)
	invert[j][i] /= num;
      for (k=0; k < 3; k++) {		/* Subtract it from the other rows */
	if (k==j) continue;
	num = invert[k][j];
	for (i=0; i < 6; i++)
	  invert[k][i] -= invert[j][i] * num;
      }
    }
    for (j=0; j < 3; j++) {		/* Add the result to coeff[][] */
      g = (j < ignore) ? j : j+1;
      for (r=0; r < 3; r++)
	coeff[r][g] += invert[r][j+3];
    }
  }
  for (r=0; r < 3; r++) {		/* Normalize such that:		*/
    for (num=g=0; g < 4; g++)		/* (1,1,1,1) x coeff = (1,1,1) */
      num += coeff[r][g];
    for (g=0; g < 4; g++)
      coeff[r][g] /= num;
  }
  use_coeff = 1;
}

/*
   Identify which camera created this file, and set global variables
   accordingly.  Return nonzero if the file cannot be decoded.
 */
int CLASS identify()
{
  char head[32], *c;
  unsigned hlen, fsize, i;
  static const struct {
    int fsize;
    char make[12], model[16];
  } table[] = {
    {    62464, "Kodak",    "DC20" },
    {   124928, "Kodak",    "DC20" },
    {  2465792, "NIKON",    "E950" },
    {  2940928, "NIKON",    "E2100" },
    {  4771840, "NIKON",    "E990" },
    {  5865472, "NIKON",    "E4500" },
    {  5869568, "NIKON",    "E4300" },
    {   787456, "Creative", "PC-CAM 600" },
    {  1976352, "Casio",    "QV-2000UX" },
    {  3217760, "Casio",    "QV-3*00EX" },
    {  6218368, "Casio",    "QV-5700" },
    {  7684000, "Casio",    "QV-4000" },
    {  9313536, "Casio",    "EX-P600" },
    {  4841984, "Pentax",   "Optio S" },
    {  6114240, "Pentax",   "Optio S4" },
    { 12582980, "Sinar",    "" } };
  static const char *corp[] =
    { "Canon", "NIKON", "Kodak", "PENTAX", "Minolta", "Konica" };

/*  What format is this file?  Set make[] if we recognize it. */

  raw_height = raw_width = flip = 0;
  make[0] = model[0] = model2[0] = 0;
  memset (white, 0, sizeof white);
  camera_red = camera_blue = timestamp = 0;
  data_offset = curve_offset = tiff_data_compression = 0;
  zero_after_ff = 0;

  order = fget2(ifp);
  hlen = fget4(ifp);
  fseek (ifp, 0, SEEK_SET);
  fread (head, 1, 32, ifp);
  fseek (ifp, 0, SEEK_END);
  fsize = ftell(ifp);
  if ((c = memmem (head, 32, "MMMMRawT", 8))) {
    strcpy (make, "Phase One");
    data_offset = c - head;
    fseek (ifp, data_offset + 8, SEEK_SET);
    fseek (ifp, fget4(ifp) + 136, SEEK_CUR);
    raw_width = fget4(ifp);
    fseek (ifp, 12, SEEK_CUR);
    raw_height = fget4(ifp);
  } else if (order == 0x4949 || order == 0x4d4d) {
    if (!memcmp (head+6,"HEAPCCDR",8)) {
      data_offset = hlen;
      parse_ciff (hlen, fsize - hlen);
    } else {
      parse_tiff(0);
      if (!strncmp(make,"NIKON",5) && raw_width == 0)
	make[0] = 0;
    }
  } else if (!memcmp (head,"\0MRM",4)) {
    parse_tiff(48);
    fseek (ifp, 4, SEEK_SET);
    data_offset = fget4(ifp) + 8;
    fseek (ifp, 24, SEEK_SET);
    raw_height = fget2(ifp);
    raw_width  = fget2(ifp);
    fseek (ifp, 12, SEEK_SET);			/* PRD */
    fseek (ifp, fget4(ifp) +  4, SEEK_CUR);	/* TTW */
    fseek (ifp, fget4(ifp) + 12, SEEK_CUR);	/* WBG */
    camera_red  = fget2(ifp);
    camera_red /= fget2(ifp);
    camera_blue = fget2(ifp);
    camera_blue = fget2(ifp) / camera_blue;
  } else if (!memcmp (head,"\xff\xd8\xff\xe1",4) &&
	     !memcmp (head+6,"Exif",4)) {
    fseek (ifp, 4, SEEK_SET);
    fseek (ifp, 4 + fget2(ifp), SEEK_SET);
    if (fgetc(ifp) != 0xff)
      parse_tiff(12);
  } else if (!memcmp (head,"BM",2)) {
    data_offset = 0x1000;
    order = 0x4949;
    fseek (ifp, 38, SEEK_SET);
    if (fget4(ifp) == 2834 && fget4(ifp) == 2834) {
      strcpy (model, "BMQ");
      goto nucore;
    }
  } else if (!memcmp (head,"BR",2)) {
    strcpy (model, "RAW");
nucore:
    strcpy (make, "Nucore");
    order = 0x4949;
    fseek (ifp, 10, SEEK_SET);
    data_offset += fget4(ifp);
    fget4(ifp);
    raw_width  = fget4(ifp);
    raw_height = fget4(ifp);
    if (model[0] == 'B' && raw_width == 2597) {
      raw_width++;
      data_offset -= 0x1000;
    }
  } else if (!memcmp (head+25,"ARECOYK",7)) {
    strcpy (make, "CONTAX");
    strcpy (model,"N DIGITAL");
  } else if (!memcmp (head,"FUJIFILM",8)) {
    fseek (ifp, 84, SEEK_SET);
    parse_tiff (fget4(ifp)+12);
    order = 0x4d4d;
    fseek (ifp, 100, SEEK_SET);
    data_offset = fget4(ifp);
  } else if (!memcmp (head,"DSC-Image",9))
    parse_rollei();
  else if (!memcmp (head,"FOVb",4))
    parse_foveon();
  else
    for (i=0; i < sizeof table / sizeof *table; i++)
      if (fsize == table[i].fsize) {
	strcpy (make,  table[i].make );
	strcpy (model, table[i].model);
      }

  for (i=0; i < sizeof corp / sizeof *corp; i++)
    if (strstr (make, corp[i]))		/* Simplify company names */
	strcpy (make, corp[i]);
  if (!strncmp (make,"KODAK",5))
    make[16] = model[16] = 0;
  c = make + strlen(make);		/* Remove trailing spaces */
  while (*--c == ' ') *c = 0;
  c = model + strlen(model);
  while (*--c == ' ') *c = 0;
  i = strlen(make);			/* Remove make from model */
  if (!strncmp (model, make, i++))
    memmove (model, model+i, 64-i);

  if (make[0] == 0) {
    fprintf (stderr, "%s: unsupported file format.\n", ifname);
    return 1;
  }

/*  File format is OK.  Do we support this camera? */
/*  Start with some useful defaults:		   */

  load_raw = NULL;
  height = raw_height;
  width  = raw_width;
  top_margin = left_margin = 0;
  colors = 3;
  filters = 0x94949494;
  black = is_cmy = is_foveon = use_coeff = 0;
  pre_mul[0] = pre_mul[1] = pre_mul[2] = pre_mul[3] = 1;
  xmag = ymag = 1;
  rgb_max = 0x4000;

/*  We'll try to decode anything from Canon or Nikon. */

  if ((is_canon = !strcmp(make,"Canon"))) {
    if (memcmp (head+6,"HEAPCCDR",8)) {
      filters = 0x61616161;
      load_raw = lossless_jpeg_load_raw;
    } else if (raw_width)
      load_raw = canon_compressed_load_raw;
  }
  if (!strcmp(make,"NIKON"))
    load_raw = nikon_is_compressed() ?
	nikon_compressed_load_raw : nikon_load_raw;

  if (!strcmp(model,"PowerShot 600")) {
    height = 613;
    width  = 854;
    colors = 4;
    filters = 0xe1e4e1e4;
    load_raw = canon_600_load_raw;
    pre_mul[0] = 1.137;
    pre_mul[1] = 1.257;
  } else if (!strcmp(model,"PowerShot A5") ||
	     !strcmp(model,"PowerShot A5 Zoom")) {
    height = 776;
    width  = 960;
    raw_width = 992;
    colors = 4;
    filters = 0x1e4e1e4e;
    load_raw = canon_a5_load_raw;
    pre_mul[0] = 1.5842;
    pre_mul[1] = 1.2966;
    pre_mul[2] = 1.0419;
  } else if (!strcmp(model,"PowerShot A50")) {
    height =  968;
    width  = 1290;
    raw_width = 1320;
    colors = 4;
    filters = 0x1b4e4b1e;
    load_raw = canon_a5_load_raw;
    pre_mul[0] = 1.750;
    pre_mul[1] = 1.381;
    pre_mul[3] = 1.182;
  } else if (!strcmp(model,"PowerShot Pro70")) {
    height = 1024;
    width  = 1552;
    colors = 4;
    filters = 0x1e4b4e1b;
    load_raw = canon_a5_load_raw;
    pre_mul[0] = 1.389;
    pre_mul[1] = 1.343;
    pre_mul[3] = 1.034;
  } else if (!strcmp(model,"PowerShot Pro90 IS")) {
    width  = 1896;
    colors = 4;
    filters = 0xb4b4b4b4;
    pre_mul[0] = 1.496;
    pre_mul[1] = 1.509;
    pre_mul[3] = 1.009;
  } else if (is_canon && raw_width == 2144) {
    height = 1550;
    width  = 2088;
    top_margin  = 8;
    left_margin = 4;
    if (!strcmp(model,"PowerShot G1")) {
      colors = 4;
      filters = 0xb4b4b4b4;
      pre_mul[0] = 1.446;
      pre_mul[1] = 1.405;
      pre_mul[2] = 1.016;
    } else {
      pre_mul[0] = 1.785;
      pre_mul[2] = 1.266;
    }
  } else if (is_canon && raw_width == 2224) {
    height = 1448;
    width  = 2176;
    top_margin  = 6;
    left_margin = 48;
    pre_mul[0] = 1.592;
    pre_mul[2] = 1.261;
  } else if (is_canon && raw_width == 2376) {
    height = 1720;
    width  = 2312;
    top_margin  = 6;
    left_margin = 12;
#ifdef CUSTOM
    if (write_fun == write_ppm)		/* Pro users may not want my matrix */
      canon_rgb_coeff (0.1);
#endif
    if (!strcmp(model,"PowerShot G2") ||
	!strcmp(model,"PowerShot S40")) {
      pre_mul[0] = 1.965;
      pre_mul[2] = 1.208;
    } else {				/* G3 and S45 */
      pre_mul[0] = 1.855;
      pre_mul[2] = 1.339;
    }
  } else if (is_canon && raw_width == 2672) {
    height = 1960;
    width  = 2616;
    top_margin  = 6;
    left_margin = 12;
    pre_mul[0] = 1.895;
    pre_mul[2] = 1.403;
  } else if (is_canon && raw_width == 3152) {
    height = 2056;
    width  = 3088;
    top_margin  = 12;
    left_margin = 64;
    pre_mul[0] = 2.242;
    pre_mul[2] = 1.245;
    if (!strcmp(model,"EOS Kiss Digital")) {
      pre_mul[0] = 1.882;
      pre_mul[2] = 1.094;
    }
    rgb_max = 16000;
  } else if (is_canon && raw_width == 3160) {
    height = 2328;
    width  = 3112;
    top_margin  = 12;
    left_margin = 44;
    pre_mul[0] = 1.85;
    pre_mul[2] = 1.53;
  } else if (is_canon && raw_width == 3344) {
    height = 2472;
    width  = 3288;
    top_margin  = 6;
    left_margin = 4;
    pre_mul[0] = 1.621;
    pre_mul[2] = 1.528;
  } else if (!strcmp(model,"EOS-1D")) {
    height = 1662;
    width  = 2496;
    data_offset = 288912;
    pre_mul[0] = 1.976;
    pre_mul[2] = 1.282;
  } else if (!strcmp(model,"EOS-1DS")) {
    height = 2718;
    width  = 4082;
    data_offset = 289168;
    pre_mul[0] = 1.66;
    pre_mul[2] = 1.13;
    rgb_max = 14464;
  } else if (!strcmp(model,"EOS-1D Mark II") ||
	     !strcmp(model,"EOS 20D")) {
    raw_height = 2360;
    raw_width  = 3596;
    top_margin  = 12;
    left_margin = 74;
    height = raw_height - top_margin;
    width  = raw_width - left_margin;
    filters = 0x94949494;
    pre_mul[0] = 1.95;
    pre_mul[2] = 1.36;
  } else if (!strcmp(model,"EOS D2000C")) {
    black = 800;
    pre_mul[2] = 1.25;
  } else if (!strcmp(model,"D1")) {
    filters = 0x16161616;
    pre_mul[0] = 0.838;
    pre_mul[2] = 1.095;
  } else if (!strcmp(model,"D1H")) {
    filters = 0x16161616;
    pre_mul[0] = 2.301;
    pre_mul[2] = 1.129;
  } else if (!strcmp(model,"D1X")) {
    width  = 4024;
    filters = 0x16161616;
    ymag = 2;
    pre_mul[0] = 1.910;
    pre_mul[2] = 1.220;
  } else if (!strcmp(model,"D100")) {
    if (tiff_data_compression == 34713 && load_raw == nikon_load_raw)
      raw_width = (width += 3) + 3;
    filters = 0x61616161;
    pre_mul[0] = 2.374;
    pre_mul[2] = 1.677;
    rgb_max = 15632;
  } else if (!strcmp(model,"D2H")) {
    width  = 2482;
    left_margin = 6;
    filters = 0x49494949;
    pre_mul[0] = 2.8;
    pre_mul[2] = 1.2;
  } else if (!strcmp(model,"D70")) {
    filters = 0x16161616;
    pre_mul[0] = 2.043;
    pre_mul[2] = 1.625;
  } else if (!strcmp(model,"E950")) {
    height = 1203;
    width  = 1616;
    filters = 0x4b4b4b4b;
    colors = 4;
    load_raw = nikon_e950_load_raw;
    nikon_e950_coeff();
    pre_mul[0] = 1.18193;
    pre_mul[2] = 1.16452;
    pre_mul[3] = 1.17250;
  } else if (!strcmp(model,"E990")) {
    height = 1540;
    width  = 2064;
    colors = 4;
    if (nikon_e990()) {
      filters = 0xb4b4b4b4;
      nikon_e950_coeff();
      pre_mul[0] = 1.196;
      pre_mul[1] = 1.246;
      pre_mul[2] = 1.018;
    } else {
      strcpy (model, "E995");
      filters = 0xe1e1e1e1;
      pre_mul[0] = 1.253;
      pre_mul[1] = 1.178;
      pre_mul[3] = 1.035;
    }
  } else if (!strcmp(model,"E2100")) {
    width = 1616;
    if (nikon_e2100()) {
      height = 1206;
      load_raw = nikon_e2100_load_raw;
      pre_mul[0] = 1.945;
      pre_mul[2] = 1.040;
    } else {
      strcpy (model, "E2500");
      height = 1204;
      filters = 0x4b4b4b4b;
      goto coolpix;
    }
  } else if (!strcmp(model,"E4300")) {
    height = 1710;
    width  = 2288;
    filters = 0x16161616;
  } else if (!strcmp(model,"E4500")) {
    height = 1708;
    width  = 2288;
    filters = 0xb4b4b4b4;
    goto coolpix;
  } else if (!strcmp(model,"E5000") || !strcmp(model,"E5700")) {
    filters = 0xb4b4b4b4;
coolpix:
    colors = 4;
    pre_mul[0] = 1.300;
    pre_mul[1] = 1.300;
    pre_mul[3] = 1.148;
  } else if (!strcmp(model,"E5400")) {
    filters = 0x16161616;
    pre_mul[0] = 1.700;
    pre_mul[2] = 1.344;
  } else if (!strcmp(model,"E8700")) {
    filters = 0x16161616;
    pre_mul[0] = 2.131;
    pre_mul[2] = 1.300;
  } else if (!strcmp(model,"FinePixS2Pro")) {
    height = 3584;
    width  = 3583;
    filters = 0x61616161;
    load_raw = fuji_s2_load_raw;
    black = 512;
    pre_mul[0] = 1.424;
    pre_mul[2] = 1.718;
  } else if (!strcmp(model,"FinePix S5000")) {
    height = 2499;
    width  = 2500;
    filters = 0x49494949;
    load_raw = fuji_s5000_load_raw;
    pre_mul[0] = 1.639;
    pre_mul[2] = 1.438;
    rgb_max = 0xf7ff;
  } else if (!strcmp(model,"FinePix S7000")) {
    pre_mul[0] = 1.62;
    pre_mul[2] = 1.38;
    goto fuji_s7000;
  } else if (!strcmp(model,"FinePix E550")) {
    pre_mul[0] = 1.45;
    pre_mul[2] = 1.25;
fuji_s7000:
    height = 3587;
    width  = 3588;
    filters = 0x49494949;
    load_raw = fuji_s7000_load_raw;
    rgb_max = 0xf7ff;
  } else if (!strcmp(model,"FinePix F700")) {
    height = 2523;
    width  = 2524;
    filters = 0x49494949;
    load_raw = fuji_f700_load_raw;
    pre_mul[0] = 1.639;
    pre_mul[2] = 1.438;
    rgb_max = 14000;
  } else if (!strcmp(model,"Digital Camera KD-400Z")) {
    height = 1712;
    width  = 2312;
    raw_width = 2336;
    data_offset = 4034;
    fseek (ifp, 2032, SEEK_SET);
    goto konica_400z;
  } else if (!strcmp(model,"Digital Camera KD-510Z")) {
    data_offset = 4032;
    fseek (ifp, 2032, SEEK_SET);
    goto konica_510z;
  } else if (!strcmp(make,"Minolta")) {
    load_raw = be_low_12_load_raw;
    rgb_max = 15860;
    if (!strncmp(model,"DiMAGE A",8)) {
      load_raw = packed_12_load_raw;
      rgb_max = model[8] == '1' ? 15916:16380;
    } else if (!strncmp(model,"DiMAGE G",8)) {
      if (model[8] <= '5') {
	data_offset = 4016;
	fseek (ifp, 1936, SEEK_SET);
konica_510z:
	height = 1956;
	width  = 2607;
	raw_width = 2624;
      } else if (model[8] == '6') {
	data_offset = 4032;
	fseek (ifp, 2030, SEEK_SET);
	height = 2136;
	width  = 2848;
      }
      filters = 0x61616161;
konica_400z:
      load_raw = be_low_10_load_raw;
      rgb_max = 15856;
      order = 0x4d4d;
      camera_red   = fget2(ifp);
      camera_blue  = fget2(ifp);
      camera_red  /= fget2(ifp);
      camera_blue /= fget2(ifp);
    }
    pre_mul[0] = 1.42;
    pre_mul[2] = 1.25;
  } else if (!strcmp(model,"*ist D")) {
    height = 2024;
    width  = 3040;
    data_offset = 0x10000;
    load_raw = be_low_12_load_raw;
    pre_mul[0] = 1.76;
    pre_mul[1] = 1.07;
  } else if (!strcmp(model,"Optio S")) {
    height = 1544;
    width  = 2068;
    raw_width = 3136;
    load_raw = packed_12_load_raw;
    pre_mul[0] = 1.506;
    pre_mul[2] = 1.152;
  } else if (!strcmp(model,"Optio S4")) {
    height = 1737;
    width  = 2324;
    raw_width = 3520;
    load_raw = packed_12_load_raw;
    pre_mul[0] = 1.308;
    pre_mul[2] = 1.275;
  } else if (!strcmp(make,"Phase One")) {
    switch (raw_height) {
      case 2060:
	strcpy (model, "LightPhase");
	height = 2048;
	width  = 3080;
	top_margin  = 5;
	left_margin = 22;
	pre_mul[0] = 1.331;
	pre_mul[2] = 1.154;
	break;
      case 2682:
	strcpy (model, "H10");
	height = 2672;
	width  = 4012;
	top_margin  = 5;
	left_margin = 26;
	break;
      case 4128:
	strcpy (model, "H20");
	height = 4098;
	width  = 4098;
	top_margin  = 20;
	left_margin = 26;
	pre_mul[0] = 1.963;
	pre_mul[2] = 1.430;
	break;
      case 5488:
	strcpy (model, "H25");
	height = 5458;
	width  = 4098;
	top_margin  = 20;
	left_margin = 26;
	pre_mul[0] = 2.80;
	pre_mul[2] = 1.20;
    }
    filters = top_margin & 1 ? 0x94949494 : 0x49494949;
    load_raw = phase_one_load_raw;
    rgb_max = 0xffff;
  } else if (!strcmp(model,"Ixpress")) {
    height = 4084;
    width  = 4080;
    filters = 0x49494949;
    load_raw = ixpress_load_raw;
    pre_mul[0] = 1.963;
    pre_mul[2] = 1.430;
    rgb_max = 0xffff;
  } else if (!strcmp(make,"Sinar") && !memcmp(head,"8BPS",4)) {
    fseek (ifp, 14, SEEK_SET);
    height = fget4(ifp);
    width  = fget4(ifp);
    filters = 0x61616161;
    data_offset = 68;
    load_raw = be_16_load_raw;
    rgb_max = 0xffff;
  } else if (!strcmp(make,"Leaf")) {
    if (height > width)
      filters = 0x16161616;
    load_raw = be_16_load_raw;
    pre_mul[0] = 1.1629;
    pre_mul[2] = 1.3556;
    rgb_max = 0xffff;
  } else if (!strcmp(model,"DIGILUX 2") || !strcmp(model,"DMC-LC1")) {
    height = 1928;
    width  = 2568;
    data_offset = 1024;
    load_raw = le_high_12_load_raw;
    pre_mul[0] = 1.883;
    pre_mul[2] = 1.367;
  } else if (!strcmp(model,"E-1")) {
    filters = 0x61616161;
    load_raw = le_high_12_load_raw;
    pre_mul[0] = 1.57;
    pre_mul[2] = 1.48;
  } else if (!strcmp(model,"E-10")) {
    load_raw = be_high_12_load_raw;
    pre_mul[0] = 1.43;
    pre_mul[2] = 1.77;
  } else if (!strncmp(model,"E-20",4)) {
    load_raw = be_high_12_load_raw;
    black = 640;
    pre_mul[0] = 1.43;
    pre_mul[2] = 1.77;
  } else if (!strcmp(model,"C5050Z")) {
    filters = 0x16161616;
    load_raw = olympus_cseries_load_raw;
    pre_mul[0] = 1.533;
    pre_mul[2] = 1.880;
  } else if (!strcmp(model,"C5060WZ")) {
    load_raw = olympus_cseries_load_raw;
    pre_mul[0] = 2.285;
    pre_mul[2] = 1.023;
  } else if (!strcmp(model,"C8080WZ")) {
    filters = 0x16161616;
    load_raw = olympus_cseries_load_raw;
    pre_mul[0] = 2.335;
    pre_mul[2] = 1.323;
  } else if (!strcmp(model,"N DIGITAL")) {
    height = 2047;
    width  = 3072;
    filters = 0x61616161;
    data_offset = 0x1a00;
    load_raw = packed_12_load_raw;
    pre_mul[0] = 1.366;
    pre_mul[2] = 1.251;
  } else if (!strcmp(model,"DSC-F828")) {
    height = 2460;
    width = 3288;
    raw_width = 3360;
    left_margin = 5;
    load_raw = sony_load_raw;
    sony_rgbe_coeff();
    filters = 0xb4b4b4b4;
    colors = 4;
    pre_mul[0] = 1.512;
    pre_mul[1] = 1.020;
    pre_mul[2] = 1.405;
  } else if (!strcasecmp(make,"KODAK")) {
    filters = 0x61616161;
    if (!strcmp(model,"NC2000F")) {
      width -= 4;
      left_margin = 1;
      curve_length = 176;
      pre_mul[0] = 1.509;
      pre_mul[2] = 2.686;
    } else if (!strcmp(model,"EOSDCS3B")) {
      width -= 4;
      left_margin = 2;
      pre_mul[0] = 1.629;
      pre_mul[2] = 2.767;
    } else if (!strcmp(model,"EOSDCS1")) {
      width -= 4;
      left_margin = 2;
      pre_mul[0] = 1.386;
      pre_mul[2] = 2.405;
    } else if (!strcmp(model,"DCS315C")) {
      black = 32;
      pre_mul[1] = 1.068;
      pre_mul[2] = 1.036;
    } else if (!strcmp(model,"DCS330C")) {
      black = 32;
      pre_mul[1] = 1.012;
      pre_mul[2] = 1.804;
    } else if (!strcmp(model,"DCS420")) {
      width -= 4;
      left_margin = 2;
      pre_mul[0] = 1.327;
      pre_mul[2] = 2.074;
    } else if (!strcmp(model,"DCS460")) {
      width -= 4;
      left_margin = 2;
      pre_mul[0] = 1.724;
      pre_mul[2] = 2.411;
    } else if (!strcmp(model,"DCS460A")) {
      width -= 4;
      left_margin = 2;
      colors = 1;
      filters = 0;
    } else if (!strcmp(model,"DCS520C")) {
      black = 720;
      pre_mul[0] = 1.006;
      pre_mul[2] = 1.858;
    } else if (!strcmp(model,"DCS560C")) {
      black = 750;
      pre_mul[1] = 1.053;
      pre_mul[2] = 1.703;
    } else if (!strcmp(model,"DCS620C")) {
      black = 720;
      pre_mul[1] = 1.002;
      pre_mul[2] = 1.818;
    } else if (!strcmp(model,"DCS620X")) {
      black = 740;
      pre_mul[0] = 1.486;
      pre_mul[2] = 1.280;
      is_cmy = 1;
    } else if (!strcmp(model,"DCS660C")) {
      black = 855;
      pre_mul[0] = 1.156;
      pre_mul[2] = 1.626;
    } else if (!strcmp(model,"DCS660M")) {
      black = 855;
      colors = 1;
      filters = 0;
    } else if (!strcmp(model,"DCS720X")) {
      pre_mul[0] = 1.35;
      pre_mul[2] = 1.18;
      is_cmy = 1;
    } else if (!strcmp(model,"DCS760C")) {
      pre_mul[0] = 1.06;
      pre_mul[2] = 1.72;
    } else if (!strcmp(model,"DCS760M")) {
      colors = 1;
      filters = 0;
    } else if (!strcmp(model,"ProBack")) {
      pre_mul[0] = 1.06;
      pre_mul[2] = 1.385;
    } else if (!strncmp(model2,"PB645C",6)) {
      pre_mul[0] = 1.0497;
      pre_mul[2] = 1.3306;
    } else if (!strncmp(model2,"PB645H",6)) {
      pre_mul[0] = 1.2010;
      pre_mul[2] = 1.5061;
    } else if (!strncmp(model2,"PB645M",6)) {
      pre_mul[0] = 1.01755;
      pre_mul[2] = 1.5424;
    } else if (!strcasecmp(model,"DCS Pro 14n")) {
      pre_mul[1] = 1.0323;
      pre_mul[2] = 1.258;
    } else if (!strcasecmp(model,"DCS Pro 14nx")) {
      pre_mul[0] = 1.336;
      pre_mul[2] = 1.3155;
    } else if (!strcasecmp(model,"DCS Pro SLR/c")) {
      pre_mul[0] = 1.425;
      pre_mul[2] = 1.293;
    } else if (!strcasecmp(model,"DCS Pro SLR/n")) {
      pre_mul[0] = 1.324;
      pre_mul[2] = 1.483;
    }
    switch (tiff_data_compression) {
      case 0:				/* No compression */
      case 1:
	load_raw = kodak_easy_load_raw;
	break;
      case 7:				/* Lossless JPEG */
	load_raw = lossless_jpeg_load_raw;
      case 32867:
	break;
      case 65000:			/* Kodak DCR compression */
	if (kodak_data_compression == 32803)
	  load_raw = kodak_compressed_load_raw;
	else {
	  load_raw = kodak_yuv_load_raw;
	  height = (height+1) & -2;
	  width  = (width +1) & -2;
	  filters = 0;
	}
	break;
      default:
	fprintf (stderr, "%s: %s %s uses unsupported compression method %d.\n",
		ifname, make, model, tiff_data_compression);
	return 1;
    }
    if (!strcmp(model,"DC20")) {
      height = 242;
      if (fsize < 100000) {
	width = 249;
	raw_width = 256;
      } else {
	width = 501;
	raw_width = 512;
      }
      data_offset = raw_width + 1;
      colors = 4;
      filters = 0x8d8d8d8d;
      kodak_dc20_coeff (0.5);
      pre_mul[1] = 1.179;
      pre_mul[2] = 1.209;
      pre_mul[3] = 1.036;
      load_raw = kodak_easy_load_raw;
    } else if (strstr(model,"DC25")) {
      strcpy (model, "DC25");
      height = 242;
      if (fsize < 100000) {
	width = 249;
	raw_width = 256;
	data_offset = 15681;
      } else {
	width = 501;
	raw_width = 512;
	data_offset = 15937;
      }
      colors = 4;
      filters = 0xb4b4b4b4;
      load_raw = kodak_easy_load_raw;
    } else if (!strcmp(model,"Digital Camera 40")) {
      strcpy (model, "DC40");
      height = 512;
      width = 768;
      data_offset = 1152;
      load_raw = kodak_radc_load_raw;
    } else if (strstr(model,"DC50")) {
      strcpy (model, "DC50");
      height = 512;
      width = 768;
      data_offset = 19712;
      load_raw = kodak_radc_load_raw;
    } else if (strstr(model,"DC120")) {
      strcpy (model, "DC120");
      height = 976;
      width = 848;
      if (tiff_data_compression == 7)
	load_raw = kodak_jpeg_load_raw;
      else
	load_raw = kodak_dc120_load_raw;
    }
  } else if (!strcmp(make,"Rollei")) {
    switch (raw_width) {
      case 1316:
	height = 1030;
	width  = 1300;
	top_margin  = 1;
	left_margin = 6;
	break;
      case 2568:
	height = 1960;
	width  = 2560;
	top_margin  = 2;
	left_margin = 8;
    }
    filters = 0x16161616;
    load_raw = rollei_load_raw;
    pre_mul[0] = 1.8;
    pre_mul[2] = 1.3;
  } else if (!strcmp(make,"SIGMA")) {
    switch (raw_height) {
      case  763:  height =  756;  top_margin =  2;  break;
      case 1531:  height = 1514;  top_margin =  7;  break;
    }
    switch (raw_width) {
      case 1152:  width = 1136;  left_margin =  8;  break;
      case 2304:  width = 2271;  left_margin = 17;  break;
    }
    if (height*2 < width) ymag = 2;
    filters = 0;
    load_raw = foveon_load_raw;
    is_foveon = 1;
    pre_mul[0] = 1.179;
    pre_mul[2] = 0.713;
    if (!strcmp(model,"SD10")) {
      pre_mul[0] *= 2.07;
      pre_mul[2] *= 2.30;
    }
    foveon_coeff();
    rgb_max = 5600;
  } else if (!strcmp(model,"PC-CAM 600")) {
    height = 768;
    data_offset = width = 1024;
    filters = 0x49494949;
    load_raw = eight_bit_load_raw;
    pre_mul[0] = 1.14;
    pre_mul[2] = 2.73;
  } else if (!strcmp(model,"QV-2000UX")) {
    height = 1208;
    width  = 1632;
    data_offset = width * 2;
    load_raw = eight_bit_load_raw;
  } else if (!strcmp(model,"QV-3*00EX")) {
    height = 1546;
    width  = 2070;
    raw_width = 2080;
    load_raw = eight_bit_load_raw;
  } else if (!strcmp(model,"QV-4000")) {
    height = 1700;
    width  = 2260;
    load_raw = be_high_12_load_raw;
  } else if (!strcmp(model,"QV-5700")) {
    height = 1924;
    width  = 2576;
    load_raw = casio_qv5700_load_raw;
  } else if (!strcmp(model,"EX-P600")) {
    height = 2142;
    width  = 2844;
    raw_width = 4288;
    load_raw = packed_12_load_raw;
    pre_mul[0] = 2.356;
    pre_mul[1] = 1.069;
  } else if (!strcmp(make,"Nucore")) {
    filters = 0x61616161;
    load_raw = nucore_load_raw;
  }
  if (!load_raw || !height) {
    fprintf (stderr, "%s: %s %s is not yet supported.\n",
	ifname, make, model);
    return 1;
  }
#ifdef NO_JPEG
  if (load_raw == kodak_jpeg_load_raw) {
    fprintf (stderr, "%s: decoder was not linked with libjpeg.\n", ifname);
    return 1;
  }
#endif
  if (!raw_height) raw_height = height;
  if (!raw_width ) raw_width  = width;
  if (colors == 4 && !use_coeff)
    gmcy_coeff();
  if (use_coeff)		 /* Apply user-selected color balance */
    for (i=0; i < colors; i++) {
      coeff[0][i] *= red_scale;
      coeff[2][i] *= blue_scale;
    }
  if (four_color_rgb && filters && colors == 3) {
    for (i=0; i < 32; i+=4) {
      if ((filters >> i & 15) == 9)
	filters |= 2 << i;
      if ((filters >> i & 15) == 6)
	filters |= 8 << i;
    }
    colors++;
    pre_mul[3] = pre_mul[1];
    if (use_coeff)
      for (i=0; i < 3; i++)
	coeff[i][3] = coeff[i][1] /= 2;
  }
  fseek (ifp, data_offset, SEEK_SET);
  return 0;
}

/*
   Convert the entire image to RGB colorspace and build a histogram.
 */
void CLASS convert_to_rgb()
{
  int row, col, r, g, c=0;
  ushort *img;
  float rgb[3], mag;

  if (document_mode)
    colors = 1;
  memset (histogram, 0, sizeof histogram);
  for (row = trim; row < height-trim; row++)
    for (col = trim; col < width-trim; col++) {
      img = image[row*width+col];
      if (document_mode)
	c = FC(row,col);
      if (colors == 4 && !use_coeff)	/* Recombine the greens */
	img[1] = (img[1] + img[3]) >> 1;
      if (colors == 1)			/* RGB from grayscale */
	for (r=0; r < 3; r++)
	  rgb[r] = img[c];
      else if (use_coeff) {		/* RGB from GMCY or Foveon */
	for (r=0; r < 3; r++)
	  for (rgb[r]=g=0; g < colors; g++)
	    rgb[r] += img[g] * coeff[r][g];
      } else if (is_cmy) {		/* RGB from CMY */
	rgb[0] = img[0] + img[1] - img[2];
	rgb[1] = img[1] + img[2] - img[0];
	rgb[2] = img[2] + img[0] - img[1];
      } else				/* RGB from RGB (easy) */
	goto norgb;
      for (r=0; r < 3; r++) {
	if (rgb[r] < 0)
	    rgb[r] = 0;
	if (rgb[r] > rgb_max)
	    rgb[r] = rgb_max;
	img[r] = rgb[r];
      }
norgb:
      if (write_fun == write_ppm) {
	for (mag=r=0; r < 3; r++)
	  mag += (unsigned) img[r]*img[r];
	mag = sqrt(mag)/2;
	if (mag > 0xffff)
	    mag = 0xffff;
	img[3] = mag;
	histogram[img[3] >> 3]++;
      }
    }
}

void CLASS flip_image()
{
  unsigned *flag;
  int size, base, dest, next, row, col, temp;
  INT64 *img, hold;

  img = (INT64 *) image;
  size = height * width;
  flag = calloc ((size+31) >> 5, sizeof *flag);
  merror (flag, "flip_image()");
  for (base=0; base < size; base++) {
    if (flag[base >> 5] & (1 << (base & 31)))
      continue;
    dest = base;
    hold = img[base];
    while (1) {
      if (flip & 4) {
	row = dest % height;
	col = dest / height;
      } else {
	row = dest / width;
	col = dest % width;
      }
      if (flip & 2)
	row = height - 1 - row;
      if (flip & 1)
	col = width - 1 - col;
      next = row * width + col;
      if (next == base) break;
      flag[next >> 5] |= 1 << (next & 31);
      img[dest] = img[next];
      dest = next;
    }
    img[dest] = hold;
  }
  free (flag);
  if (flip & 4) {
    temp = height;
    height = width;
    width = temp;
    temp = ymag;
    ymag = xmag;
    xmag = temp;
  }
}

/*
   Write the image to a 24-bpp PPM file.
 */
void CLASS write_ppm (FILE *ofp)
{
  int row, col, i, c, val, total;
  float max, mul, scale[0x10000];
  ushort *rgb;
  uchar (*ppm)[3];

/*
   Set the white point to the 99th percentile
 */
  i = width * height * (strcmp(make,"FUJIFILM") ? 0.01 : 0.005);
  for (val=0x2000, total=0; --val; )
    if ((total += histogram[val]) > i) break;
  max = val << 4;

  fprintf (ofp, "P6\n%d %d\n255\n",
	xmag*(width-trim*2), ymag*(height-trim*2));

  ppm = calloc (width-trim*2, 3*xmag);
  merror (ppm, "write_ppm()");
  mul = bright * 442 / max;
  scale[0] = 0;
  for (i=1; i < 0x10000; i++)
    scale[i] = mul * pow (i*2/max, gamma_val-1);

  for (row=trim; row < height-trim; row++) {
    for (col=trim; col < width-trim; col++) {
      rgb = image[row*width+col];
      for (c=0; c < 3; c++) {
	val = rgb[c] * scale[rgb[3]];
	if (val > 255) val=255;
	for (i=0; i < xmag; i++)
	  ppm[xmag*(col-trim)+i][c] = val;
      }
    }
    for (i=0; i < ymag; i++)
      fwrite (ppm, width-trim*2, 3*xmag, ofp);
  }
  free (ppm);
}

/*
   Write the image to a 48-bpp Photoshop file.
 */
void CLASS write_psd (FILE *ofp)
{
  char head[] = {
    '8','B','P','S',		/* signature */
    0,1,0,0,0,0,0,0,		/* version and reserved */
    0,3,			/* number of channels */
    0,0,0,0,			/* height, big-endian */
    0,0,0,0,			/* width, big-endian */
    0,16,			/* 16-bit color */
    0,3,			/* mode (1=grey, 3=rgb) */
    0,0,0,0,			/* color mode data */
    0,0,0,0,			/* image resources */
    0,0,0,0,			/* layer/mask info */
    0,0				/* no compression */
  };
  int hw[2], psize, row, col, c, val;
  ushort *buffer, *pred, *rgb;

  hw[0] = htonl(height-trim*2);	/* write the header */
  hw[1] = htonl(width-trim*2);
  memcpy (head+14, hw, sizeof hw);
  fwrite (head, 40, 1, ofp);

  psize = (height-trim*2) * (width-trim*2);
  buffer = calloc (6, psize);
  merror (buffer, "write_psd()");
  pred = buffer;

  for (row = trim; row < height-trim; row++) {
    for (col = trim; col < width-trim; col++) {
      rgb = image[row*width+col];
      for (c=0; c < 3; c++) {
	val = rgb[c] * bright;
	if (val > 0xffff)
	    val = 0xffff;
	pred[c*psize] = htons(val);
      }
      pred++;
    }
  }
  fwrite(buffer, psize, 6, ofp);
  free (buffer);
}

/*
   Write the image to a 48-bpp PPM file.
 */
void CLASS write_ppm16 (FILE *ofp)
{
  int row, col, c, val;
  ushort *rgb, (*ppm)[3];

  val = rgb_max * bright;
  if (val < 256)
      val = 256;
  if (val > 0xffff)
      val = 0xffff;
  fprintf (ofp, "P6\n%d %d\n%d\n",
	width-trim*2, height-trim*2, val);

  ppm = calloc (width-trim*2, 6);
  merror (ppm, "write_ppm16()");

  for (row = trim; row < height-trim; row++) {
    for (col = trim; col < width-trim; col++) {
      rgb = image[row*width+col];
      for (c=0; c < 3; c++) {
	val = rgb[c] * bright;
	if (val > 0xffff)
	    val = 0xffff;
	ppm[col-trim][c] = htons(val);
      }
    }
    fwrite (ppm, width-trim*2, 6, ofp);
  }
  free (ppm);
}

int CLASS main (int argc, char **argv)
{
  int arg, status=0, user_flip=-1;
  int identify_only=0, write_to_stdout=0, half_size=0;
  char opt, *ofname, *cp;
  const char *write_ext = ".ppm";
  FILE *ofp = stdout;

  if (argc == 1)
  {
    fprintf (stderr,
    "\nRaw Photo Decoder \"dcraw\" v6.06"
    "\nby Dave Coffin, dcoffin a cybercom o net"
    "\n\nUsage:  %s [options] file1 file2 ...\n"
    "\nValid options:"
    "\n-i        Identify files but don't decode them"
    "\n-c        Write to standard output"
    "\n-v        Print verbose messages while decoding"
    "\n-f        Interpolate RGBG as four colors"
    "\n-d        Document Mode (no color, no interpolation)"
    "\n-q        Quick, low-quality color interpolation"
    "\n-h        Half-size color image (3x faster than -q)"
    "\n-g <num>  Set gamma      (0.6 by default, only for 24-bpp output)"
    "\n-b <num>  Set brightness (1.0 by default)"
    "\n-a        Use automatic white balance"
    "\n-w        Use camera white balance, if possible"
    "\n-r <num>  Set red  multiplier (daylight = 1.0)"
    "\n-l <num>  Set blue multiplier (daylight = 1.0)"
    "\n-t [0-7]  Flip image (0 = none, 3 = 180, 5 = 90CCW, 6 = 90CW)"
    "\n-2        Write 24-bpp PPM (default)"
    "\n-3        Write 48-bpp PSD (Adobe Photoshop)"
    "\n-4        Write 48-bpp PPM"
    "\n\n", argv[0]);
    return 1;
  }

  argv[argc] = "";
  for (arg=1; argv[arg][0] == '-'; ) {
    opt = argv[arg++][1];
    if (strchr ("gbrl", opt) && !isdigit(argv[arg][0])) {
      fprintf (stderr, "\"-%c\" requires a numeric argument.\n", opt);
      return 1;
    }
    switch (opt)
    {
      case 'g':  gamma_val   = atof(argv[arg++]);  break;
      case 'b':  bright      = atof(argv[arg++]);  break;
      case 'r':  red_scale   = atof(argv[arg++]);  break;
      case 'l':  blue_scale  = atof(argv[arg++]);  break;
      case 't':  user_flip   = atoi(argv[arg++]);  break;

      case 'i':  identify_only     = 1;  break;
      case 'c':  write_to_stdout   = 1;  break;
      case 'v':  verbose           = 1;  break;
      case 'h':  half_size         = 1;		/* "-h" implies "-f" */
      case 'f':  four_color_rgb    = 1;  break;
      case 'd':  document_mode     = 1;  break;
      case 'q':  quick_interpolate = 1;  break;
      case 'a':  use_auto_wb       = 1;  break;
      case 'w':  use_camera_wb     = 1;  break;

      case '2':  write_fun = write_ppm;   write_ext = ".ppm";  break;
      case '3':  write_fun = write_psd;   write_ext = ".psd";  break;
      case '4':  write_fun = write_ppm16; write_ext = ".ppm";  break;

      default:
	fprintf (stderr, "Unknown option \"-%c\".\n", opt);
	return 1;
    }
  }
  if (arg == argc) {
    fprintf (stderr, "No files to process.\n");
    return 1;
  }
  if (write_to_stdout) {
    if (isatty(1)) {
      fprintf (stderr, "Will not write an image to the terminal!\n");
      return 1;
    }
#if defined(WIN32) || defined(DJGPP)
    if (setmode(1,O_BINARY) < 0) {
      perror ("setmode()");
      return 1;
    }
#endif
  }

  for ( ; arg < argc; arg++)
  {
    status = 1;
    image = NULL;
    if (setjmp (failure)) {
      if (fileno(ifp) > 2) fclose(ifp);
      if (fileno(ofp) > 2) fclose(ofp);
      if (image) free (image);
      status = 1;
      continue;
    }
    ifname = argv[arg];
    if (!(ifp = fopen (ifname, "rb"))) {
      perror (ifname);
      continue;
    }
    if ((status = identify())) {
      fclose(ifp);
      continue;
    }
    if (identify_only) {
      fprintf (stderr, "%s is a %s %s image.\n", ifname, make, model);
      fclose(ifp);
      continue;
    }
    shrink = half_size && filters;
    iheight = (height + shrink) >> shrink;
    iwidth  = (width  + shrink) >> shrink;
    image = calloc (iheight * iwidth, sizeof *image);
    merror (image, "main()");
    if (verbose)
      fprintf (stderr,
	"Loading %s %s image from %s...\n", make, model, ifname);
    (*load_raw)();
    fclose(ifp);
    bad_pixels();
    height = iheight;
    width  = iwidth;
    if (is_foveon) {
      if (verbose)
	fprintf (stderr, "Foveon interpolation...\n");
      foveon_interpolate();
    } else {
      scale_colors();
    }
    if (shrink) filters = 0;
    trim = 0;
    if (filters && !document_mode) {
      trim = 1;
      if (verbose)
	fprintf (stderr, "%s interpolation...\n",
	  quick_interpolate ? "Bilinear":"VNG");
      vng_interpolate();
    }
    if (verbose)
      fprintf (stderr, "Converting to RGB colorspace...\n");
    convert_to_rgb();
    if (user_flip >= 0)
      flip = user_flip;
    if (flip) {
      if (verbose)
	fprintf (stderr, "Flipping image %c:%c:%c...\n",
	  flip & 1 ? 'H':'0', flip & 2 ? 'V':'0', flip & 4 ? 'T':'0');
      flip_image();
    }
    ofname = malloc (strlen(ifname) + 16);
    merror (ofname, "main()");
    if (write_to_stdout)
      strcpy (ofname, "standard output");
    else {
      strcpy (ofname, ifname);
      if ((cp = strrchr (ofname, '.'))) *cp = 0;
      strcat (ofname, write_ext);
      ofp = fopen (ofname, "wb");
      if (!ofp) {
	status = 1;
	perror (ofname);
	goto cleanup;
      }
    }
    if (verbose)
      fprintf (stderr, "Writing data to %s...\n", ofname);
    (*write_fun)(ofp);
    if (ofp != stdout)
      fclose(ofp);
cleanup:
    free (ofname);
    free (image);
  }
  return status;
}
