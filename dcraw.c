/*
   Raw Photo Decoder (formerly "Canon PowerShot Converter")
   Copyright 1997-2002 by Dave Coffin <dcoffin@shore.net>

   This is a portable ANSI C program to convert raw image files from
   any digital camera into PPM or PNG format.  TIFF and CIFF parsing
   are based upon public specifications, but no such documentation
   is available for the raw sensor data, so writing this program has
   been an immense effort.

   This code is freely licensed for all uses, commercial and
   otherwise.  Comments, questions, and encouragement are welcome.

   $Revision: 1.78 $
   $Date: 2002/12/01 04:34:57 $

   The Canon EOS-1D and some Kodak cameras compress their raw data
   with lossless JPEG.  To read such images, you must also download:

	http://www.shore.net/~dcoffin/powershot/ljpeg_decode.tar.gz
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <winsock2.h>
typedef __int64 INT64;
#else
#include <netinet/in.h>
typedef long long INT64;
#endif

#ifndef NO_PNG
#include <png.h>
#endif

#ifdef LJPEG_DECODE
#include "jpeg.h"
#include "mcu.h"
#include "proto.h"
#endif

typedef unsigned char uchar;
typedef unsigned short ushort;

/* Global Variables */

FILE *ifp;
short order;
int height, width, trim, colors, is_cmy, is_canon, black, rgb_max;
int raw_height, raw_width;	/* Including black borders */
int tiff_data_offset, tiff_data_compression;
int nef_curve_offset;
int kodak_645m;
unsigned filters;
char make[64], model[64];
ushort (*image)[4];
void (*read_crw)();
float gamma_val=0.8, bright=1.0, red_scale=1.0, blue_scale=1.0;
float rgb_mul[3];
float coeff[3][4];

struct decode {
  struct decode *branch[2];
  int leaf;
} first_decode[32], second_decode[512];

/*
   I assume that the influence of one pixel upon another is proportionate
   to the inverse square of the distance between them.  So I've designed
   three neighborhoods for aspect ratios of 1:1, 1:2, and 2:1.

   Triplet format is y-offset,x-offset,weight.  0,0,0 marks the end.
 */
const char square_hood[] = {
       -1,-1,1, -1,0,2, -1,1,1,
	0,-1,2,          0,1,2,
	1,-1,1,  1,0,2,  1,1,1,
0,0,0 };

const char wide_hood[] = {
       -1,-1,1, -1,0,1, -1,1,1,
0,-2,1, 0,-1,4,          0,1,4,  0,2,1,
	1,-1,1,  1,0,1,  1,1,1,
0,0,0 };

const char tall_hood[] = {
		-2,0,1,
       -1,-1,1, -1,0,4, -1,1,1,
	0,-1,1,		 0,1,1,
	1,-1,1,  1,0,4,  1,1,1,
		 2,0,1,
0,0,0 };

const char *hood;

/*
   In order to inline this calculation, I make the risky
   assumption that all filter patterns can be described
   by a repeating pattern of eight rows and two columns

   Return values are either 0/1/2/3 = G/M/C/Y or 0/1/2 = R/G/B
 */
#define FC(row,col) \
	(filters >> ((((row) << 1 & 14) + ((col) & 1)) << 1) & 3)
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

   These are Bayer grids, used by most RGB cameras:

	0x94949494:	0x61616161:	0x16161616:

	  0 1 2 3 4 5	  0 1 2 3 4 5	  0 1 2 3 4 5
	0 R G R G R G	0 G R G R G R	0 B G B G B G
	1 G B G B G B	1 B G B G B G	1 G R G R G R
	2 R G R G R G	2 G R G R G R	2 B G B G B G
	3 G B G B G B	3 B G B G B G	3 G R G R G R

   The Fuji S2 does not use a Bayer grid.  It has flat, wide
   pixels in the following pattern, stored in the file with
   a ninety-degree rotation (not shown here).

	bbbb|rrrr|bbbb|rrrr|bbbb|rrrr
	gggg|gggg|gggg|gggg|gggg|gggg
	rrrr|bbbb|rrrr|bbbb|rrrr|bbbb
	gggg|gggg|gggg|gggg|gggg|gggg

 */

void ps600_read_crw()
{
  uchar  data[1120], *dp;
  ushort pixel[896], *pix;
  int irow, orow, col;

/*
   Immediately after the 26-byte header come the data rows.  First
   the even rows 0..612, then the odd rows 1..611.  Each row is 896
   pixels, ten bits per pixel, packed into 1120 bytes (8960 bits).
 */
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
/*
   Copy 854 pixels into the image[] array.  The other 42 pixels
   are black.  Left-shift by 4 for extra precision in upcoming
   calculations.
 */
    for (col=0; col < width; col++)
      image[orow*width+col][FC(orow,col)] = pixel[col] << 4;
    for (col=width; col < 896; col++)
      black += pixel[col];

    if ((orow+=2) > height)	/* Once we've read all the even rows, */
      orow = 1;			/* read the odd rows. */
  }
  black = ((INT64) black << 4) / ((896 - width) * height);
}

void a5_read_crw()
{
  uchar  data[1240], *dp;
  ushort pixel[992], *pix;
  int row, col;

/*
   Each data row is 992 ten-bit pixels, packed into 1240 bytes.
 */
  for (row=0; row < height; row++) {
    fread (data, 1240, 1, ifp);
    for (dp=data, pix=pixel; dp < data+1240; dp+=10, pix+=8)
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
/*
   Copy 960 pixels into the image[] array.  The other 32 pixels
   are black.  Left-shift by 4 for extra precision in upcoming
   calculations.
 */
    for (col=0; col < width; col++)
      image[row*width+col][FC(row,col)] = (pixel[col] & 0x3ff) << 4;
    for (col=width; col < 992; col++)
      black += pixel[col] & 0x3ff;
  }
  black = ((INT64) black << 4) / ((992 - width) * height);
}

void a50_read_crw()
{
  uchar  data[1650], *dp;
  ushort pixel[1320], *pix;
  int row, col;

/*
  Each row is 1320 ten-bit pixels, packed into 1650 bytes.
 */
  for (row=0; row < height; row++) {
    fread (data, 1650, 1, ifp);
    for (dp=data, pix=pixel; dp < data+1650; dp+=10, pix+=8)
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
/*
   Copy 1290 pixels into the image[] array.  The other 30 pixels
   are black.  Left-shift by 4 for extra precision in upcoming
   calculations.
 */
    for (col=0; col < width; col++)
      image[row*width+col][FC(row,col)] = (pixel[col] & 0x3ff) << 4;
    for (col=width; col < 1320; col++)
      black += pixel[col] & 0x3ff;
  }
  black = ((INT64) black << 4) / ((1320 - width) * height);
}

void pro70_read_crw()
{
  uchar  data[1940], *dp;
  ushort pixel[1552], *pix;
  int row, col;

/*
  Each row is 1552 ten-bit pixels, packed into 1940 bytes.
 */
  for (row=0; row < height; row++) {
    fread (data, 1940, 1, ifp);
    for (dp=data, pix=pixel; dp < data+1940; dp+=10, pix+=8)
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
/*
   Copy all pixels into the image[] array.  Left-shift by 4 for
   extra precision in upcoming calculations.  No black pixels?
 */
    for (col=0; col < width; col++)
      image[row*width+col][FC(row,col)] = (pixel[col] & 0x3ff) << 4;
  }
}

/*
   A rough description of Canon's compression algorithm:

+  Each pixel outputs a 10-bit sample, from 0 to 1023.
+  Split the data into blocks of 64 samples each.
+  Subtract from each sample the value of the sample two positions
   to the left, which has the same color filter.  From the two
   leftmost samples in each row, subtract 512.
+  For each nonzero sample, make a token consisting of two four-bit
   numbers.  The low nibble is the number of bits required to
   represent the sample, and the high nibble is the number of
   zero samples preceding this sample.
+  Output this token as a variable-length bitstring using
   one of three tablesets.  Follow it with a fixed-length
   bitstring containing the sample.

   The "first_decode" table is used for the first sample in each
   block, and the "second_decode" table is used for the others.
 */

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
void make_decoder(struct decode *dest, const uchar *source, int level)
{
  static struct decode *free;	/* Next unused node */
  static int leaf;		/* number of leaves already added */
  int i, next;

  if (level==0) {
    free = dest;
    leaf = 0;
  }
  free++;
/*
   At what level should the next leaf appear?
 */
  for (i=next=0; i <= leaf && next < 16; )
    i += source[next++];

  if (level < next) {		/* Are we there yet? */
    dest->branch[0] = free;
    make_decoder (free, source, level+1);
    dest->branch[1] = free;
    make_decoder (free, source, level+1);
  } else
    dest->leaf = source[16 + leaf++];
}

void init_tables(unsigned table)
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
  memset ( first_decode, 0, sizeof first_decode);
  memset (second_decode, 0, sizeof second_decode);
  make_decoder ( first_decode,  first_tree[table], 0);
  make_decoder (second_decode, second_tree[table], 0);
}

/*
   getbits(-1) initializes the buffer
   getbits(n) where 0 <= n <= 25 returns an n-bit integer
 */
unsigned long getbits(int nbits)
{
  static unsigned long bitbuf=0, ret=0;
  static int vbits=0;
  unsigned char c;

  if (nbits == 0) return 0;
  if (nbits == -1)
    ret = bitbuf = vbits = 0;
  else {
    ret = bitbuf << (32 - vbits) >> (32 - nbits);
    vbits -= nbits;
  }
  while (vbits < 25) {
    c = fgetc(ifp);
    bitbuf = (bitbuf << 8) + c;
    if (c == 0xff && is_canon)	/* Canon puts an extra 0 after 0xff */
      fgetc(ifp);
    vbits += 8;
  }
  return ret;
}

/*
   Decompress "count" blocks of 64 samples each.

   Note that the width passed to this function is slightly
   larger than the global width, because it includes some
   blank pixels that (*read_crw) will strip off.
 */
void decompress(ushort *outbuf, int count)
{
  struct decode *decode, *dindex;
  int i, leaf, len, sign, diff, diffbuf[64];
  static int carry, pixel, base[2];

  if (!outbuf) {			/* Initialize */
    carry = pixel = 0;
    fseek (ifp, count, SEEK_SET);
    getbits(-1);
    return;
  }
  while (count--) {
    memset(diffbuf,0,sizeof diffbuf);
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
      sign=(getbits(1));	/* 1 is positive, 0 is negative */
      diff=getbits(len-1);
      if (sign)
	diff += 1 << (len-1);
      else
	diff += (-1 << len) + 1;
      if (i < 64) diffbuf[i] = diff;
    }
    diffbuf[0] += carry;
    carry = diffbuf[0];
    for (i=0; i < 64; i++ ) {
      if (pixel++ % raw_width == 0)
	base[0] = base[1] = 512;
      outbuf[i] = ( base[i & 1] += diffbuf[i] );
    }
    outbuf += 64;
  }
}

/*
   Return 0 if the image starts with compressed data,
   1 if it starts with uncompressed low-order bits.

   In Canon compressed data, 0xff is always followed by 0x00.
 */
int canon_has_lowbits()
{
  uchar test[8192];
  int ret=1, i;

  fseek (ifp, 0, SEEK_SET);
  fread (test, 1, 8192, ifp);
  for (i=540; i < 8191; i++)
    if (test[i] == 0xff) {
      if (test[i+1]) return 1;
      ret=0;
    }
  return ret;
}

void canon_compressed_read_crw()
{
  ushort *pixel, *prow;
  int lowbits, shift, i, row, r, col, save;
  unsigned top=0, left=0, irow, icol;
  uchar c;

/* Set the width of the black borders */
  switch (raw_width) {
    case 2144:  top = 8;  left =  4;  break;	/* G1 */
    case 2224:  top = 6;  left = 48;  break;	/* EOS D30 */
    case 2376:  top = 6;  left = 12;  break;	/* G2 or G3 */
    case 3152:  top =12;  left = 64;  break;	/* EOS D60 */
  }
  pixel = calloc (raw_width*8, sizeof *pixel);
  if (!pixel) {
    perror("canon_compressed_read_crw() calloc failed");
    exit(1);
  }
  lowbits = canon_has_lowbits();
  shift = 4 - lowbits*2;
  decompress(0, 540 + lowbits*raw_height*raw_width/4);
  for (row=0; row < raw_height; row+=8) {
    decompress(pixel, raw_width/8);		/* Get eight rows */
    if (lowbits) {
      save = ftell(ifp);			/* Don't lose our place */
      fseek (ifp, 26 + row*raw_width/4, SEEK_SET);
      for (prow=pixel, i=0; i < raw_width*2; i++) {
	c = fgetc(ifp);
	for (r=0; r < 8; r+=2)
	  *prow++ = (*prow << 2) + ((c >> r) & 3);
      }
      fseek (ifp, save, SEEK_SET);
    }
    for (r=0; r < 8; r++)
      for (col=0; col < raw_width; col++) {
	irow = row+r-top;
	icol = col-left;
	if (irow >= height) continue;
	if (icol < width)
	  image[irow*width+icol][FC(irow,icol)] =
		pixel[r*raw_width+col] << shift;
	else
	    black += pixel[r*raw_width+col];
      }
  }
  free (pixel);
  black = ((INT64) black << shift) / ((raw_width - width) * height);
}

#ifdef LJPEG_DECODE
/*
   Lossless JPEG code calls this function to get data.
 */
int ReadJpegData (char *buffer, int numBytes)
{
  return fread(buffer, 1, numBytes, ifp);
}

/*
   Called from DecodeImage() in huffd.c to write one row.
   Notice that one row of the JPEG data is two rows for us.
   Canon did this so that the predictors could work against
   like colors.  Quite clever!

   Kodak didn't think of that.  8-/
 */
void PmPutRow(ushort **buf, int numComp, int numCol, int row)
{
  register int r, col;
  int trick=1;

  if (make[0] == 'C') trick=2;		/* Canon */
  row *= trick;
  for (r = row; r < row+trick; r++)
    for (col=0; col < width; ) {
      image[r*width+col++][FC(r,col)] = buf[0][0] << 2;
      image[r*width+col++][FC(r,col)] = buf[0][1] << 2;
      buf++;
    }
}

void lossless_jpeg_read_crw()
{
  DecompressInfo dcInfo;

  fseek (ifp, tiff_data_offset, SEEK_SET);

  MEMSET(&dcInfo, 0, sizeof(dcInfo));
  ReadFileHeader (&dcInfo);
  ReadScanHeader (&dcInfo);
  DecoderStructInit (&dcInfo);
  HuffDecoderInit (&dcInfo);
  DecodeImage (&dcInfo);
  FreeArray2D (mcuROW1);
  FreeArray2D (mcuROW2);
}
#else
void lossless_jpeg_read_crw() { }
#endif /* LJPEG_DECODE */

ushort fget2 (FILE *f);
int    fget4 (FILE *f);

void nikon_compressed_read_crw()
{
  int waste=0;
  static const uchar nikon_tree[] = {
    0,1,5,1,1,1,1,1,1,2,0,0,0,0,0,0,
    5,4,3,6,2,7,1,0,8,9,11,10,12
  };
  int vpred[4], hpred[2], csize, row, col, i, len;
  ushort *curve;
  struct decode *dindex;
  register int diff;

  if (!strcmp(model,"D1X"))
    waste = 4;
  if (!strcmp(model,"D100"))
    width = 3034;

  memset (first_decode, 0, sizeof first_decode);
  make_decoder (first_decode,  nikon_tree, 0);

  fseek (ifp, nef_curve_offset, SEEK_SET);
  for (i=0; i < 4; i++)
    vpred[i] = fget2(ifp);
  csize = fget2(ifp);
  curve = calloc (csize, sizeof *curve);
  if (!curve) {
    perror("curve calloc failed");
    exit(1);
  }
  for (i=0; i < csize; i++)
    curve[i] = fget2(ifp);

  fseek (ifp, tiff_data_offset, SEEK_SET);
  getbits(-1);

  for (row=0; row < height; row++)
    for (col=0; col < width+waste; col++)
    {
      for (dindex=first_decode; dindex->branch[0]; )
	dindex = dindex->branch[getbits(1)];
      len = dindex->leaf;
      diff = getbits(len);
      if ((diff & (1 << (len-1))) == 0)
	diff -= (1 << len) - 1;
      if (col < 2) {
	i = 2*(row & 1) + col;
	vpred[i] += diff;
	hpred[col] = vpred[i];
      } else
	hpred[col & 1] += diff;
      if (col >= width) continue;
      diff = hpred[col & 1];
      if (diff < 0) diff = 0;
      if (diff >= csize) diff = csize-1;
      image[row*width+col][FC(row,col)] = curve[diff] << 2;
    }
  free (curve);
}

/*
   Try to figure out if the image is compressed, based on my limited
   collection of NEF files.  For the D100, every 16th byte of an
   uncompressed image is zero.
 */
int nikon_is_compressed()
{
  uchar test[256];
  int i;

  if (strcmp(model,"D100"))
    return tiff_data_compression == 34713;
  fseek (ifp, tiff_data_offset, SEEK_SET);
  fread (test, 1, 256, ifp);
  for (i=15; i < 256; i+=16)
    if (test[i]) return 1;
  return 0;
}

void nikon_read_crw()
{
  int waste=0, skip16=0;
  int irow, row, col, i;

  if (nikon_is_compressed()) {
    nikon_compressed_read_crw();
    return;
  }
  if (!strcmp(model,"D1X"))
    waste = 4;
  if (!strcmp(model,"D100")) {
    waste = 3;
    skip16 = 1;
  }

  fseek (ifp, tiff_data_offset, SEEK_SET);
  getbits(-1);
  for (irow=0; irow < height; irow++) {
    row = irow;
    if (model[0] == 'E')
      row = irow * 2 % height + irow / (height/2);
    for (col=0; col < width+waste; col++) {
      i = getbits(12);
      if (col < width)
	image[row*width+col][FC(row,col)] = i << 2;
      if (skip16 && (col % 10) == 9)
	getbits(8);
    }
  }
}

/*
   Rotate the image ninety degrees.
 */
void fuji_read_crw()
{
  ushort pixel[2944];
  int gray=0, row, col;

  fseek (ifp, 0, SEEK_SET);
  while (gray < 2944)
    if (fget2(ifp) == 2048) gray++;
    else gray=0;
  fseek (ifp, (2944*23+32) << 1, SEEK_CUR);
  for (col=width; col--; ) {
    fread (pixel, 2, 2944, ifp);
    for (row=0; row < height; row++)
      image[row*width+col][FC(row,col)] = ntohs(pixel[row]) << 2;
  }
}

void minolta_read_crw()
{
  ushort *pixel;
  int row, col;

  pixel = calloc (width, sizeof *pixel);
  if (!pixel) {
    perror("minolta_read_crw() calloc failed");
    exit(1);
  }
  fseek (ifp, tiff_data_offset, SEEK_SET);
  for (row=0; row < height; row++) {
    fread (pixel, 2, width, ifp);
    for (col=0; col < width; col++)
      image[row*width+col][FC(row,col)] = ntohs(pixel[col]) << 2;
  }
  free (pixel);
}

void olympus_read_crw()
{
  ushort *pixel;
  int row, col;

  pixel = calloc (width, sizeof *pixel);
  if (!pixel) {
    perror("olympus_read_crw() calloc failed");
    exit(1);
  }
  fseek (ifp, 0x4000, SEEK_SET);
  for (row=0; row < height; row++) {
    fread (pixel, 2, width, ifp);
    for (col=0; col < width; col++)
      image[row*width+col][FC(row,col)] = ntohs(pixel[col]) >> 2;
  }
  free (pixel);
}

void kodak_easy_read_crw()
{
  uchar *pixel;
  int row, col, margin;

  if ((margin = (raw_width - width)/2))
    black = 0;
  pixel = calloc (raw_width, sizeof *pixel);
  if (!pixel) {
    perror("kodak_easy_read_crw() calloc failed");
    exit(1);
  }
  fseek (ifp, tiff_data_offset, SEEK_SET);
  for (row=0; row < height; row++) {
    fread (pixel, 1, raw_width, ifp);
    for (col=0; col < width; col++)
      image[row*width+col][FC(row,col)] = (ushort) pixel[col+margin] << 6;
    if (margin == 2)
      black += pixel[0] + pixel[1] + pixel[raw_width-2] + pixel[raw_width-1];
  }
  if (margin)
    black = ((INT64) black << 6) / (4 * height);
  free (pixel);
}

void kodak_compressed_read_crw()
{
  uchar c, blen[256];
  unsigned row, col, len, i, bits=0, pred[2];
  INT64 bitbuf=0;
  register int diff;

  fseek (ifp, tiff_data_offset, SEEK_SET);

  for (row=0; row < height; row++)
    for (col=0; col < width; col++)
    {
      if ((col & 255) == 0) {		/* Get the bit-lengths of the */
	len = width - col;		/* next 256 pixel values      */
	if (len > 256) len = 256;
	for (i=0; i < len; ) {
	  c = fgetc(ifp);
	  blen[i++] = c & 15;
	  blen[i++] = c >> 4;
	}
	bitbuf = bits = pred[0] = pred[1] = 0;
      }
      len = blen[col & 255];		/* Number of bits for this pixel */
      if (bits < len) {			/* Got enough bits in the buffer? */
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
      image[row*width+col][FC(row,col)] = diff << 2;
    }
}

void subtract_black()
{
  ushort *img;
  int size;

  img = image[0];
  size = width * height * 4;
  while (size--)
    if (*img > black)
      *img++ -= black;
    else
      *img++ = 0;
  rgb_max -= black;
}

/*
   RGB interpolation algorithm designed by Matt Dillon.
   GMCY cameras still use my algorithm.
   His explanation follows:

 * When this function is called, we only have one color for
 * each pixel.  This code searches the 5x5 neighborhood and
 * synthesizes the other colors based on interpolation.  The
 * algorithm works as follows.  Take one portion of the
 * CCD filter pattern:
 *
 *	  0 1 2 3 4
 *	0 G R G R G
 *	1 B G B G B
 *	2 G R<G>R G
 *	3 B G B G B
 *	4 G R G R G
 *
 * Lets say we are on pixel (x,y) = (2,2), the bracketed green above, and
 * we are trying to synthesize the Blue and Red guns for that pixel based
 * on the surrounding pixels.
 *
 * We could average the surrounding blue pixels to synthesize a blue for
 * (2,2) but that will blur the image somewhat if we are on the boundary
 * of an edge.  Instead what we do is calculate a temporary green for each
 * blue pixel and then generate the blue for (2,2) by scaling it against
 * the differential between the temporary green and the green in (2,2).
 *
 * For example, the green for the blue pixel at (2,1) is calculated by
 * averaging the green at (2,0) and (2,2).  A scaling factor is generated
 * using this green and the green at (2,2), we multiply in the Blue pixel
 * at (2,1), and that is the Blue we store for (2,2).  It's actually somewhat
 * more complicated since there are other blue's around (2,2).  What we do
 * is execute this calculate for each blue and do a weighted average of the
 * result, and that final number is the Blue we store at (2,2).
 *
 * The scaling factor tends to blow up in low-light situations due to
 * the calculation of the scaling factor becoming skewed (think about the
 * difference between two pixel brightnesses of 10 and 15, and 4000 and 4005).
 * To compensate we introduce a constant in the scaling process, llfactor.
 * The higher the constant the closer the scaling factor gets to 1:1 (a
 * straight average of the surrounding blue's regardless of the scale
 * generated using the green's).  This results in somewhat more blurring
 * but hides low-light dropouts.
 */
#define lowlight_val 0
void dillon_interpolate()
{
  int row, col, cc, c;
  int vb, vr, val;
  int dx, dy;
  int avg[4], sum[4];
  int base = 4096 + 64;
  int llfactor = 8 + lowlight_val * 16;
  int weight;
  int size;
  ushort (*oimage)[4];

  size = height * width * 8;
  oimage = malloc(size);
  if (!oimage) {
    perror("dillon_interpolate() calloc failed");
    exit(1);
  }
  memcpy (oimage, image, size);

  for (row=2; row < height-2; row++) {
    for (col=2; col < width-2; col++) {
      cc = FC(row,col);
      vb = oimage[row*width+col][cc];
      avg[0] = avg[1] = avg[2] = avg[3] = 0;
      sum[0] = sum[1] = sum[2] = sum[3] = 0;
      for (dy = -1; dy <= 1; ++dy) {
	for (dx = -1; dx <= 1; ++dx) {
	  c = FC(row+dy, col+dx);
	  vr = oimage[(row+dy*2)*width + (col+dx*2)][cc];
	  vr = (vb + vr) / 2;
	  /*
	   * RGB CCDs almost universally repeat the same
	   * color filter 2 pixels in any direction.
	   */
	  val = oimage[(row+dy)*width + (col+dx)][c];
	  weight = base - abs(vr - vb);
	  val = (llfactor + vb) * val / (llfactor + vr);
	  weight = 100;
	  avg[c] += val * weight;
	  sum[c] += weight;
	}
      }
      for (c=0; c < colors; c++) {
	if (sum[c])
	  image[row*width+col][c] = (avg[c] + (sum[c]/2)) / sum[c];
      }
    }
  }
  free (oimage);
}

/*
   When this function is called, we only have one color for
   each pixel.  Search the 3x3 neighborhood for pixels of
   other colors, and average them.  Diagonal neighbors get
   counted once, orthogonal neighbors twice.
 */
void bilinear_interpolate()
{
  unsigned row, col, cc, y, x, w, c, avg[8];
  const char *h;

  for (row=1; row < height-1; row++)
    for (col=1; col < width-1; col++) {
      cc = FC(row,col);
      memset (avg, 0, sizeof avg);
      for (h = hood; ; ) {
	y = row + *h++;
	x = col + *h++;
	if ((w = *h++) == 0) break;
	if (y >= height || x >= width) continue;
	if ((c = FC(y,x)) == cc) continue;
	avg[c]   += w * image[y*width+x][c];
	avg[c+4] += w;
      }
      for (c=0; c < colors; c++)
	if (c != cc)
	  image[row*width+col][c] = avg[c] / avg[c+4];
    }
}

/*
   We now have all color values for each pixel.  Sharpen the
   spatial detail by assuming that neighboring pixels have
   similar color ratios.

   To avoid amplifying noise, wherever any color value falls
   below a certain threshold, that pixel and all neighboring
   pixels will be left unchanged.
*/
void smooth_hues()
{
  ushort (*last)[4];
  ushort (*curr)[4];
  void *tmp;
  unsigned row, col, cc, y, x, w, c, val, avg[8];
  const char *h;

  last = calloc (width, sizeof *curr);
  curr = calloc (width, sizeof *curr);
  if (!last || !curr) {
    perror("smooth_hues() calloc failed");
    exit(1);
  }
  for (row=2; row < height-2; row++) {
    for (col=2; col < width-2; col++) {
      memset (avg, 0, sizeof avg);
      for (h = hood; ; ) {	/* Any noisy neighbors in the 'hood? */
	y = row + *h++;
	x = col + *h++;
	if ((w = *h++) == 0) break;
	for (c=0; c < colors; c++)
	  if (image[y*width+x][c] < 75) goto noise;
      }
      cc = FC(row,col);		/* Neighbors are OK to use */
      for (h = hood; ; ) {
	y = row + *h++;
	x = col + *h++;
	if ((w = *h++) == 0) break;
	if ((c = FC(y,x)) == cc) continue;
	val = ((unsigned long) image[y*width+x][c] << 16) /
		image[y*width+x][cc] * image[row*width+col][cc] >> 16;
	avg[c]   += w * val;
	avg[c+4] += w;
      }
noise:
      for (c=0; c < colors; c++)
	curr[col][c] = avg[c+4] ? avg[c]/avg[c+4] : image[row*width+col][c];
    }
    if (row > 2)
      memcpy (image[(row-1)*width+2], last+2, (width-4)*sizeof *last);
    tmp = last;
    last = curr;
    curr = tmp;
  }
  memcpy (image[(row-1)*width+2], last+2, (width-4)*sizeof *last);
  free (last);
  free (curr);
}

/*
   Get a 2-byte integer, making no assumptions about CPU byte order.
   Nor should we assume that the compiler evaluates left-to-right.
 */
ushort fget2 (FILE *f)
{
  register uchar a, b;

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
int fget4 (FILE *f)
{
  register uchar a, b, c, d;

  a = fgetc(f);
  b = fgetc(f);
  c = fgetc(f);
  d = fgetc(f);
  if (order == 0x4949)
    return a + (b << 8) + (c << 16) + (d << 24);
  else
    return (a << 24) + (b << 16) + (c << 8) + d;
}

void tiff_parse_subifd(int base)
{
  int entries, tag, type, len, val, save;

  entries = fget2(ifp);
  while (entries--) {
    tag  = fget2(ifp);
    type = fget2(ifp);
    len  = fget4(ifp);
    if (type == 3) {		/* short int */
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
      case 0x111:		/* StripOffset */
	if (len == 1)
	  tiff_data_offset = val;
	else {
	  save = ftell(ifp);
	  fseek (ifp, val+base, SEEK_SET);
	  tiff_data_offset = fget4(ifp);
	  fseek (ifp, save, SEEK_SET);
	}
	break;
      case 0x115:		/* SamplesPerRow */
	break;
      case 0x116:		/* RowsPerStrip */
	break;
      case 0x117:		/* StripByteCounts */
	break;
      case 0x828d:		/* Unknown */
      case 0x828e:		/* Unknown */
      case 0x9217:		/* Unknown */
	break;
      case 0x920e:
	kodak_645m = 0;
	break;
      case 0xa20e:
	kodak_645m = 1;
	break;
    }
  }
}

void nef_parse_makernote()
{
  int base=0, offset=0, entries, tag, type, len, val;
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
  } else
    fseek (ifp, -10, SEEK_CUR);

  entries = fget2(ifp);
  while (entries--) {
    tag  = fget2(ifp);
    type = fget2(ifp);
    len  = fget4(ifp);
    val  = fget4(ifp);
    if (tag == 0x8c)
      nef_curve_offset = base+val + 2112;
    if (tag == 0x96)
      nef_curve_offset = base+val + 2;
  }
  order = sorder;
}

void nef_parse_exif()
{
  int entries, tag, type, len, val, save;

  entries = fget2(ifp);
  while (entries--) {
    tag  = fget2(ifp);
    type = fget2(ifp);
    len  = fget4(ifp);
    val  = fget4(ifp);
    save = ftell(ifp);
    if (tag == 0x927c) {		/* MakerNote */
      fseek (ifp, val, SEEK_SET);
      nef_parse_makernote();
      fseek (ifp, save, SEEK_SET);
    }
  }
}

/*
   Parse a TIFF file looking for camera model and decompress offsets.
 */
void parse_tiff(int base)
{
  int doff, entries, tag, type, len, val, save;

  tiff_data_offset = 0;
  tiff_data_compression = 0;
  nef_curve_offset = 0;
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
      val  = fget4(ifp);
      save = ftell(ifp);
      fseek (ifp, val+base, SEEK_SET);
      switch (tag) {
	case 271:			/* Make tag */
	  fread (make, 64, 1, ifp);
	  if (len > 63) len=63;
	  make[len]=0;
	  break;
	case 272:			/* Model tag */
	  fread (model, 64, 1, ifp);
	  if (len > 63) len=63;
	  model[len]=0;
	  break;
	case 330:			/* SubIFD tag */
	  if (len > 2) len=2;
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
	  nef_parse_exif();
	  break;
      }
      fseek (ifp, save, SEEK_SET);
    }
  }
}

/*
   Parse the CIFF structure looking for two pieces of information:
   The camera model, and the decode table number.
 */
void parse_ciff(int offset, int length)
{
  int tboff, nrecs, i, type, len, roff, aoff, save;

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
    if (type == 0x1031) {		/* Get the raw width and height */
      fseek (ifp, aoff+2, SEEK_SET);
      raw_width  = fget2(ifp);
      raw_height = fget2(ifp);
    }
    if (type == 0x1835) {		/* Get the decoder table */
      fseek (ifp, aoff, SEEK_SET);
      init_tables (fget4(ifp));
    }
    if (type >> 8 == 0x28 || type >> 8 == 0x30)	/* Get sub-tables */
      parse_ciff(aoff, len);
    fseek (ifp, save, SEEK_SET);
  }
}

void make_coeff();

/*
   Open a CRW file, identify which camera created it, and set
   global variables accordingly.  Returns nonzero if an error occurs.
 */
int open_and_id(char *fname)
{
  char head[8], *c;
  unsigned magic, hlen;

  rgb_mul[0] = 1.592;
  rgb_mul[1] = 1.0;
  rgb_mul[2] = 1.261;
  rgb_max = 0x4000;
  colors = 4;
  is_cmy = 0;
  hood = square_hood;

  ifp = fopen(fname,"rb");
  if (!ifp) {
    perror(fname);
    return 1;
  }
  model[0] = 0;
  order = fget2(ifp);
  if (order == 0x4949 || order == 0x4d4d) {
    hlen = fget4(ifp);
    fread (head, 1, 8, ifp);
    if (!memcmp(head,"HEAPCCDR",8)) {
      fseek (ifp, 0, SEEK_END);
      parse_ciff(hlen, ftell(ifp) - hlen);
      fseek (ifp, hlen, SEEK_SET);
    } else
      parse_tiff(0);
  } else {
    fseek (ifp, 0, SEEK_SET);
    magic = fget4(ifp);
    if (magic == 0x46554a49)		/* "FUJI" */
      parse_tiff(120);
    else if (magic == 0x4d524d) {	/* "\0MRM" */
      parse_tiff(48);
      fseek (ifp, 4, SEEK_SET);
      tiff_data_offset = fget4(ifp) + 8;
      fseek (ifp, 24, SEEK_SET);
      raw_height = fget2(ifp);
      raw_width  = fget2(ifp);
    }
  }
  /* Remove excess wordage */
  if (!strncmp(make,"NIKON",5) || !strncmp(make,"Canon",5))
    make[5] = 0;
  if (!strncmp(make,"OLYMPUS",7) || !strncmp(make,"Minolta",7))
    make[7] = 0;
  if (!strncmp(make,"KODAK",5))
    model[16] = 0;
  if (!strncmp(model,"Canon",5) || !strncmp(model,"NIKON",5))
    memmove (model, model+6, 64-6);

  /* Remove trailing spaces */
  c = make + strlen(make);
  while (*--c == ' ') *c = 0;
  c = model + strlen(model);
  while (*--c == ' ') *c = 0;
  if (model[0] == 0) {
    fprintf (stderr, "%s: unsupported file format.\n", fname);
    return 1;
  }
  is_canon = !strcmp(make,"Canon");
  if (!strcmp(model,"PowerShot 600")) {
    height = 613;
    width  = 854;
    filters = 0xe1e4e1e4;
    read_crw = ps600_read_crw;
    rgb_mul[0] = 1.667;
    rgb_mul[2] = 1.667;
  } else if (!strcmp(model,"PowerShot A5")) {
    height = 776;
    width  = 960;
    filters = 0x1e4e1e4e;
    read_crw = a5_read_crw;
    rgb_mul[0] = 1.111;
    rgb_mul[2] = 0.978;
  } else if (!strcmp(model,"PowerShot A50")) {
    height =  968;
    width  = 1290;
    filters = 0x1b4e4b1e;
    read_crw = a50_read_crw;
    rgb_mul[0] = 1.316;
    rgb_mul[2] = 0.776;
  } else if (!strcmp(model,"PowerShot Pro70")) {
    height = 1024;
    width  = 1552;
    filters = 0x1e4b4e1b;
    read_crw = pro70_read_crw;
  } else if (!strcmp(model,"PowerShot Pro90 IS")) {
    height = 1416;
    width  = 1896;
    filters = 0xb4b4b4b4;
    read_crw = canon_compressed_read_crw;
  } else if (!strcmp(model,"PowerShot G1")) {
    height = 1550;
    width  = 2088;
    filters = 0xb4b4b4b4;
    read_crw = canon_compressed_read_crw;
    rgb_mul[0] = 1.469;
    rgb_mul[2] = 1.327;
  } else if (!strcmp(model,"PowerShot S30")) {
    height = 1550;
    width  = 2088;
    colors = 3;
    filters = 0x94949494;
    read_crw = canon_compressed_read_crw;
    rgb_mul[0] = 1.785;
    rgb_mul[2] = 1.266;
  } else if (!strcmp(model,"PowerShot G2")  ||
	     !strcmp(model,"PowerShot G3")  ||
	     !strcmp(model,"PowerShot S40") ||
	     !strcmp(model,"PowerShot S45")) {
    height = 1720;
    width  = 2312;
    colors = 3;
    filters = 0x94949494;
    read_crw = canon_compressed_read_crw;
    rgb_mul[0] = 1.966;
    rgb_mul[2] = 1.208;
  } else if (!strcmp(model,"EOS D30")) {
    height = 1448;
    width  = 2176;
    colors = 3;
    filters = 0x94949494;
    read_crw = canon_compressed_read_crw;
  } else if (!strcmp(model,"EOS D60")) {
    height = 2056;
    width  = 3088;
    colors = 3;
    filters = 0x94949494;
    read_crw = canon_compressed_read_crw;
    rgb_mul[0] = 2.242;
    rgb_mul[2] = 1.245;
    rgb_max = 16000;
  } else if (!strcmp(model,"EOS-1D")) {
    height = 1662;
    width  = 2496;
    colors = 3;
    filters = 0x61616161;
    read_crw = lossless_jpeg_read_crw;
    tiff_data_offset = 288912;
    rgb_mul[0] = 1.976;
    rgb_mul[2] = 1.282;
  } else if (!strcmp(model,"EOS-1DS")) {
    height = 2718;
    width  = 4082;
    colors = 3;
    filters = 0x61616161;
    read_crw = lossless_jpeg_read_crw;
    tiff_data_offset = 289168;
    rgb_mul[0] = 1.66;
    rgb_mul[2] = 1.13;
    rgb_max = 14464;
  } else if (!strcmp(model,"D1")) {
    height = 1324;
    width  = 2012;
    colors = 3;
    filters = 0x16161616;
    read_crw = nikon_read_crw;
    rgb_mul[0] = 0.838;
    rgb_mul[2] = 1.095;
  } else if (!strcmp(model,"D1H")) {
    height = 1324;
    width  = 2012;
    colors = 3;
    filters = 0x16161616;
    read_crw = nikon_read_crw;
    rgb_mul[0] = 1.347;
    rgb_mul[2] = 3.279;
  } else if (!strcmp(model,"D1X")) {
    height = 1324;
    width  = 4024;
    colors = 3;
    filters = 0x16161616;
    hood = wide_hood;
    read_crw = nikon_read_crw;
    rgb_mul[0] = 1.910;
    rgb_mul[2] = 1.220;
  } else if (!strcmp(model,"D100")) {
    height = 2024;
    width  = 3037;
    colors = 3;
    filters = 0x61616161;
    read_crw = nikon_read_crw;
    rgb_mul[0] = 2.374;
    rgb_mul[2] = 1.677;
  } else if (!strcmp(model,"E5000") || !strcmp(model,"E5700")) {
    height = 1924;
    width  = 2576;
    filters = 0xb4b4b4b4;
    read_crw = nikon_read_crw;
    rgb_mul[0] = 2.126;
    rgb_mul[2] = 1.197;
  } else if (!strcmp(model,"FinePixS2Pro")) {
    height = 2880;
    width  = 2144;
    colors = 3;
    filters = 0x58525852;
    hood = tall_hood;
    read_crw = fuji_read_crw;
    rgb_mul[0] = 1.424;
    rgb_mul[2] = 1.718;
  } else if (!strcmp(make,"Minolta")) {
    height = raw_height;
    width  = raw_width;
    colors = 3;
    filters = 0x94949494;
    read_crw = minolta_read_crw;
    rgb_mul[0] = 1;
    rgb_mul[2] = 1;
  } else if (!strcmp(model,"E-10")) {
    height = 1684;
    width  = 2256;
    colors = 3;
    filters = 0x94949494;
    read_crw = olympus_read_crw;
    rgb_mul[0] = 1.43;
    rgb_mul[2] = 1.77;
  } else if (!strncmp(model,"E-20",4)) {
    height = 1924;
    width  = 2576;
    colors = 3;
    filters = 0x94949494;
    read_crw = olympus_read_crw;
    rgb_mul[0] = 1.43;
    rgb_mul[2] = 1.77;
  } else if (!strcasecmp(make,"KODAK")) {
    height = raw_height;
    width  = raw_width;
    colors = 3;
    filters = 0x61616161;
    rgb_mul[0] = 1;
    rgb_mul[2] = 1;
    black = 400;
    if (!strcmp(model,"DCS315C")) {
      rgb_mul[0] = 0.973;
      rgb_mul[2] = 0.987;
      black = 0;
    } else if (!strcmp(model,"DCS330C")) {
      rgb_mul[0] = 0.996;
      rgb_mul[2] = 1.279;
      black = 0;
    } else if (!strcmp(model,"DCS420")) {
      rgb_mul[0] = 1.21;
      rgb_mul[2] = 1.63;
      width -= 4;
    } else if (!strcmp(model,"DCS460")) {
      rgb_mul[0] = 1.46;
      rgb_mul[2] = 1.84;
      width -= 4;
    } else if (!strcmp(model,"DCS460A")) {
      colors = 1;
      filters = 0;
      width -= 4;
    } else if (!strcmp(model,"EOSDCS3B")) {
      rgb_mul[0] = 1.43;
      rgb_mul[2] = 2.16;
      width -= 4;
    } else if (!strcmp(model,"EOSDCS1")) {
      rgb_mul[0] = 1.28;
      rgb_mul[2] = 2.00;
      width -= 4;
    } else if (!strcmp(model,"DCS520C")) {
      rgb_mul[0] = 1.00;
      rgb_mul[2] = 1.20;
    } else if (!strcmp(model,"DCS560C")) {
      rgb_mul[0] = 0.985;
      rgb_mul[2] = 1.15;
    } else if (!strcmp(model,"DCS620C")) {
      rgb_mul[0] = 1.00;
      rgb_mul[2] = 1.20;
    } else if (!strcmp(model,"DCS620X")) {
      rgb_mul[0] = 1.12;
      rgb_mul[2] = 1.07;
      is_cmy = 1;
    } else if (!strcmp(model,"DCS660C")) {
      rgb_mul[0] = 1.05;
      rgb_mul[2] = 1.17;
    } else if (!strcmp(model,"DCS660M")) {
      colors = 1;
      filters = 0;
    } else if (!strcmp(model,"DCS720X")) {
      rgb_mul[0] = 1.35;
      rgb_mul[2] = 1.18;
      is_cmy = 1;
    } else if (!strcmp(model,"DCS760C")) {
      rgb_mul[0] = 1.06;
      rgb_mul[2] = 1.72;
    } else if (!strcmp(model,"DCS760M")) {
      colors = 1;
      filters = 0;
    } else if (!strcmp(model,"ProBack")) {
      rgb_mul[0] = 1.06;
      rgb_mul[2] = 1.385;
    } else if (!strcmp(model,"ProBack645")) {
      if (kodak_645m) {
	rgb_mul[0] = 1.06;		/* 645M sample image */
	rgb_mul[2] = 1.50;
      } else {
	rgb_mul[0] = 1.20;		/* 645H sample image */
	rgb_mul[2] = 1.52;
      }
    }
    switch (tiff_data_compression) {
      case 0:				/* No compression */
      case 1:
	read_crw = kodak_easy_read_crw;  break;
      case 7:				/* Lossless JPEG */
	read_crw = lossless_jpeg_read_crw;  break;
      case 65000:			/* Kodak DCR compression */
	black = 0;
	read_crw = kodak_compressed_read_crw;  break;
      default:
	fprintf (stderr, "%s: %s %s uses unsupported compression method %d.\n",
		fname, make, model, tiff_data_compression);
	return 1;
    }
  } else {
    fprintf (stderr, "%s: %s %s is not yet supported.\n",fname, make, model);
    return 1;
  }
#ifndef LJPEG_DECODE
  if (read_crw == lossless_jpeg_read_crw) {
    fprintf (stderr, "%s: %s %s requires lossless JPEG decoder.\n",
	fname, make, model);
    return 1;
  }
#endif
  rgb_mul[0] *= red_scale;	/* Apply user-selected color balance */
  rgb_mul[2] *= blue_scale;
  if (colors == 4) make_coeff();
  return 0;
}

/*
   Given a matrix that converts RGB to GMCY, create a matrix to do
   the opposite.  Only square matrices can be inverted, so I create
   four 3x3 matrices by omitting a different GMCY color in each one.
   The final coeff[][] matrix is the sum of these four.
 */
void make_coeff()
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
  for (r=0; r < 3; r++)			/* Multiply coeff[][] by rgb_mul[] */
    for (g=0; g < 4; g++)
      coeff[r][g] *= rgb_mul[r];
}

void get_rgb(float rgb[4], ushort image[4])
{
  int r, g;
  float cmy[4];

  memset (rgb, 0, 4 * sizeof (float));
  if (colors == 1) {
    for (r=0; r < 3; r++)		/* RGB from grayscale */
      rgb[r] = image[0];
    rgb[3] = 3 * rgb[0]*rgb[0];
  } else if (colors == 3)
    for (r=0; r < 3; r++) {		/* RGB from RGB */
      rgb[r] = image[r] * rgb_mul[r];
      if (rgb[r] > rgb_max)
	  rgb[r] = rgb_max;
      rgb[3] += rgb[r]*rgb[r];		/* Compute magnitude */
    }
  else
    for (r=0; r < 3; r++) {		/* RGB from GMCY */
      for (g=0; g < 4; g++)
	rgb[r] += image[g] * coeff[r][g];
      if (rgb[r] < 0) rgb[r] = 0;
      rgb[3] += rgb[r]*rgb[r];
    }
  if (is_cmy) {
    memcpy (cmy, rgb, sizeof cmy);
    rgb[0] = cmy[0] + cmy[1] - cmy[2];
    rgb[1] = cmy[1] + cmy[2] - cmy[0];
    rgb[2] = cmy[2] + cmy[0] - cmy[1];
    rgb[3] = rgb[0]*rgb[0] + rgb[1]*rgb[1] + rgb[2]*rgb[2];
  }
}

/*
   Write the image to a 24-bit PPM file.
 */
void write_ppm(FILE *ofp)
{
  int y, x, i, ymag=1, xmag=1;
  register unsigned c, val;
  uchar (*ppm)[3];
  float rgb[4], max, max2, expo, mul, scale;
  int total, histogram[0x1000];

/*
   Build a histogram of magnitudes using 4096 bins of 64 values each.
 */
  memset (histogram, 0, sizeof histogram);
  for (y=trim; y < height-trim; y++)
    for (x=trim; x < width-trim; x++) {
      get_rgb (rgb, image[y*width+x]);
      val = (int) sqrt(rgb[3]) >> 6;
      if (val > 0xfff) val=0xfff;
      histogram[val]++;
    }
/*
   Set the white point to the 99.66th percentile
 */
  for (val=0x1000, total=0; --val; )
    if ((total+=histogram[val]) > (int)(width*height*0.01)) break;
  max = val << 6;
  max2 = max * max;

  if (hood == tall_hood) xmag = 2;
  if (hood == wide_hood) ymag = 2;
  fprintf (ofp, "P6\n%d %d\n255\n",
	xmag*(width-trim*2), ymag*(height-trim*2));

  ppm = calloc (width-trim*2, 3*xmag);
  if (!ppm) {
    perror("ppm calloc failed");
    exit(1);
  }
  expo = (gamma_val-1)/2;		/* Pull these out of the loop */
  mul = bright * 442 / max;

  for (y=trim; y < height-trim; y++)
  {
    for (x=trim; x < width-trim; x++)
    {
      get_rgb(rgb,image[y*width+x]);
/* In some math libraries, pow(0,expo) doesn't return zero */
      scale = 0;
      if (rgb[3]) scale = mul * pow(rgb[3]/max2,expo);

      for (c=0; c < 3; c++)
      {
	val=rgb[c]*scale;
	if (val > 255) val=255;
	for (i=0; i < xmag; i++)
	  ppm[xmag*(x-trim)+i][c] = val;
      }
    }
    for (i=0; i < ymag; i++)
      fwrite (ppm, width-trim*2, 3*xmag, ofp);
  }
  free (ppm);
}

/*
   Write the image to a 48-bit Photoshop file.
 */
void write_psd(FILE *ofp)
{
  unsigned char head[] = {
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
  int hw[2], psize, y, x, c, val;
  float rgb[4];
  ushort *buffer, *pred;

  hw[0] = htonl(height-trim*2);	/* write the header */
  hw[1] = htonl(width-trim*2);
  memcpy (head+14, hw, sizeof hw);
  fwrite (head, 40, 1, ofp);

  psize = (height-trim*2) * (width-trim*2);
  buffer = calloc (6, psize);
  if (!buffer) {
    perror("psd calloc failed");
    exit(1);
  }
  pred = buffer;

  for (y=trim; y < height-trim; y++)
  {
    for (x=trim; x < width-trim; x++)
    {
      get_rgb(rgb, image[y * width + x]);
      for (c=0; c < 3; c++) {
	val = rgb[c] * bright;
	if (val > 0xffff) val=0xffff;
	pred[c*psize] = htons(val);
      }
      pred++;
    }
  }
  fwrite(buffer, psize, 6, ofp);
  free (buffer);
}

#ifndef NO_PNG
/*
   Write the image to a 48-bit PNG file.
 */
void write_png(FILE *ofp)
{
  png_structp png_ptr;
  png_infop info_ptr;
  ushort (*png)[3];
  int y, x, c, val;
  float rgb[4];

  png_ptr = png_create_write_struct
       (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) return;
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_write_struct(&png_ptr, (png_infopp)NULL);
    return;
  }
  if (setjmp(png_ptr->jmpbuf)) {
    png_destroy_write_struct(&png_ptr, &info_ptr);
    return;
  }
  png_init_io (png_ptr, ofp);
  png_set_IHDR (png_ptr, info_ptr, width-trim*2, height-trim*2,
       16, PNG_COLOR_TYPE_RGB, PNG_INTERLACE_NONE,
       PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

/* Comment out this line if you want compression */
  png_set_compression_level(png_ptr, 0);

  png_write_info(png_ptr, info_ptr);

  if (htons(0xaa55) != 0xaa55)
    png_set_swap(png_ptr);

  png = calloc(width-trim*2,6);
  if (!png) {
    perror("png calloc failed");
    exit(1);
  }

  for (y=trim; y < height-trim; y++) {
    for (x=trim; x < width-trim; x++) {
      get_rgb(rgb, image[y*width+x]);
      for (c=0; c < 3; c++) {
	val = rgb[c] * bright;
	if (val > 0xffff) val=0xffff;
	png[x-trim][c] = val;
      }
    }
    png_write_row(png_ptr, (png_bytep) png);
  }
  free (png);
  png_write_end(png_ptr, NULL);
  png_destroy_write_struct(&png_ptr, &info_ptr);
}
#endif

/*
   Creates a new filename with a different extension
 */
void exten(char *new_name, const char *old, const char *ext)
{
  char *cp;

  strcpy(new_name,old);
  cp=strrchr(new_name,'.');
  if (!cp) cp=new_name+strlen(new_name);
  strcpy(cp,ext);
}

int main(int argc, char **argv)
{
  char data[256];
  int i, arg, write_to_files=1, dillon=0, dillon_ok, smooth=1;
  void (*write_fun)(FILE *) = write_ppm;
  const char *write_ext = ".ppm";
  FILE *ofp;

  if (argc == 1)
  {
    fprintf (stderr,
    "\nRaw Photo Decoder v3.60"
#ifdef LJPEG_DECODE
    " with Lossless JPEG support"
#endif
    "\nby Dave Coffin (dcoffin@shore.net)"
    "\n\nUsage:  %s [options] file1.crw file2.crw ...\n"
    "\nValid options:"
    "\n-c        Write to standard output"
    "\n-d        Use Dillon interpolation if possible"
    "\n-s <num>  Number of times to smooth hues (1 by default)"
    "\n-g <num>  Set gamma value (%5.3f by default, only for 24-bit output)"
    "\n-b <num>  Set brightness  (%5.3f by default)"
    "\n-r <num>  Set red  scaling (daylight = 1.0)"
    "\n-l <num>  Set blue scaling (daylight = 1.0)"
    "\n-2        Write 24-bit PPM (default)"
    "\n-3        Write 48-bit PSD (Adobe Photoshop)"
#ifndef NO_PNG
    "\n-4        Write 48-bit PNG"
#endif
    "\n\n",
      argv[0], gamma_val, bright);
    exit(1);
  }

/* Parse out the options */

  for (arg=1; argv[arg][0] == '-'; arg++)
    switch (argv[arg][1])
    {
      case 'c':
	write_to_files = 0;  break;
      case 'd':
	dillon = 1;  break;
      case 's':
	smooth = atoi(argv[++arg]);  break;
      case 'g':
	gamma_val = atof(argv[++arg]);  break;
      case 'b':
	bright = atof(argv[++arg]);  break;
      case 'r':
	red_scale = atof(argv[++arg]);  break;
      case 'l':
	blue_scale = atof(argv[++arg]);  break;
      case '2':
	write_fun = write_ppm;
	write_ext = ".ppm";
	break;
      case '3':
	write_fun = write_psd;
	write_ext = ".psd";
	break;
#ifndef NO_PNG
      case '4':
	write_fun = write_png;
	write_ext = ".png";
	break;
#endif
      default:
	fprintf (stderr, "Unknown option \"%s\"\n", argv[arg]);
	exit(1);
    }

/* Process the named files  */

  for ( ; arg < argc; arg++)
  {
    black = 0;
    if (open_and_id(argv[arg])) {
      if (ifp) fclose(ifp);
      continue;
    }
    image = calloc (height * width, sizeof *image);
    if (!image) {
      perror("image calloc failed");
      exit(1);
    }
    fprintf (stderr, "Loading %s %s image from %s...\n",
	make, model, argv[arg]);
    (*read_crw)();
    fclose(ifp);
    if (black) {
      fprintf (stderr, "Subtracting thermal noise (%d)...\n", black);
      subtract_black();
    }
    trim = 0;
    if (filters == 0) goto nointerp;
    dillon_ok = dillon;
    for (i=8; i < 32; i+=8)
      if ((filters >> i & 0xff) != (filters & 0xff))
	dillon_ok = 0;
    if (dillon_ok) {
      fprintf (stderr, "Dillon interpolation...\n");
      dillon_interpolate();
      trim = 2;
    } else {
      if (dillon)
	fprintf (stderr, "Filter pattern is not Dillon-compatible.\n");
      fprintf (stderr, "Bilinear interpolation...\n");
      bilinear_interpolate();
      for (i=0; i < smooth; i++) {
	fprintf (stderr, "Smoothing hues...\n");
	smooth_hues();
      }
      trim = 1;
    }
nointerp:
    ofp = stdout;
    strcpy (data, "standard output");
    if (write_to_files) {
      exten(data, argv[arg], write_ext);
      ofp = fopen(data,"wb");
      if (!ofp) {
	perror(data);
	continue;
      }
    }
    fprintf (stderr, "Writing data to %s...\n", data);
    (*write_fun)(ofp);
    if (write_to_files)
      fclose(ofp);

    free (image);
  }
  return 0;
}
