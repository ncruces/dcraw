/*
   Canon PowerShot Converter
   Copyright (c)1997-2001 by Dave Coffin <dcoffin@shore.net>

   A portable ANSI C program to convert raw CRW files from Canon
   PowerShot digital cameras into PPM format.

   This is an entirely original work; no other copyrights apply.
   Any similarity to Canon's code is only to the extent necessary
   to decode image formats of Canon's design.

   This code is freely licensed for all uses, commercial and
   otherwise.  Comments and questions are welcome.

   $Revision: 1.26 $
   $Date: 2001/09/30 00:04:43 $
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char uchar;
typedef unsigned short ushort;

/* Global Variables */

FILE *ifp;
int height, width;
ushort (*gmcy)[4];
int (*filter)(int,int);
void (*read_crw)(int);
int histogram[1024];
float gamma_val=0.8, bright=1.0;
float ymul[4];
float coeff[3][4];

struct decode {
  struct decode *branch[2];
  int leaf;
} first_decode[32], second_decode[512];

/*
   Filter pattern of the PowerShot 600:

	  0 1 2 3 4 5
	0 G M G M G M		Return values
	1 C Y C Y C Y		 0  1  2  3
	2 M G M G M G		 G  M  C  Y
	3 C Y C Y C Y
 */
ps600_filter(int row, int col)
{
  return (0xe1e4 >> ((((row) << 1 & 6) + ((col) & 1)) << 1) & 3);
}

/*
   Load CCD pixel values into the gmcy[] array.  Unknown colors
   (such as cyan under a magenta filter) must be set to zero.
 */
void ps600_read_crw(int row)
{
  uchar  data[1120], *dp;
  ushort pixel[896], *pix;
  int irow, orow, col;

/*
   Immediately after the 26-byte header come the data rows.  First
   the even rows 0..612, then the odd rows 1..611.  Each row is 896
   pixels, ten bits per pixel, packed into 1120 bytes (8960 bits).

   Since the rows are not stored in top-to-bottom order like the
   other cameras, we must load all rows before processing can begin.
 */
  if (row) return;

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

    memset(gmcy + row*width, 0, width*sizeof *gmcy);
/*
   Copy 854 pixels into the gmcy[] array.  The other 42 pixels
   are blank.  Left-shift by 4 for extra precision in upcoming
   calculations.
 */
    for (col=0; col < width; col++)
      gmcy[orow*width+col][(*filter)(orow,col)] = pixel[col] << 4;

    if ((orow+=2) > height)	/* Once we've read all the even rows, */
      orow = 1;			/* read the odd rows. */
  }
}

/*
   Filter pattern of the PowerShot A5:

	  0 1 2 3 4 5
	0 C Y C Y C Y		Return values
	1 G M G M G M		 0  1  2  3
	2 C Y C Y C Y		 G  M  C  Y
	3 M G M G M G
 */
a5_filter(int row, int col)
{
  return (0x1e4e >> ((((row) << 1 & 6) + ((col) & 1)) << 1) & 3);
}

/*
   Load CCD pixel values into the gmcy[] array.  Unknown colors
   (such as cyan under a magenta filter) must be set to zero.
 */
void a5_read_crw(int row)
{
  uchar  data[1240], *dp;
  ushort pixel[992], *pix;
  int col;

/*
   Each data row is 992 ten-bit pixels, packed into 1240 bytes.
 */
  fread(data, 1240, 1, ifp);
  for (dp=data, pix=pixel; dp < data+1200; dp+=10, pix+=8)
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

  memset(gmcy + row*width, 0, width*sizeof *gmcy);
/*
   Copy 960 pixels into the gmcy[] array.  The other 32 pixels
   are blank.  Left-shift by 4 for extra precision in upcoming
   calculations.
 */
  for (col=0; col < width; col++)
    gmcy[row*width+col][(*filter)(row,col)] = (pixel[col] & 0x3ff) << 4;
}

/*
   Filter pattern of the PowerShot A50:

	  0 1 2 3 4 5
	0 C Y C Y C Y		Return values
	1 M G M G M G		 0  1  2  3
	2 Y C Y C Y C		 G  M  C  Y
	3 G M G M G M
	4 C Y C Y C Y
	5 G M G M G M
	6 Y C Y C Y C
	7 M G M G M G
 */
a50_filter(int row, int col)
{
  return (0x1b4e4b1e >> ((((row) << 1 & 14) + ((col) & 1)) << 1) & 3);
}

/*
   Load CCD pixel values into the gmcy[] array.  Unknown colors
   (such as cyan under a magenta filter) must be set to zero.
 */
void a50_read_crw(int row)
{
  uchar  data[1650], *dp;
  ushort pixel[1320], *pix;
  int col;

/*
  Each row is 1320 ten-bit pixels, packed into 1650 bytes.
 */
  fread(data, 1650, 1, ifp);
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

  memset(gmcy + row*width, 0, width*sizeof *gmcy);
/*
   Copy 1290 pixels into the gmcy[] array.  The other 30 pixels
   are blank.  Left-shift by 4 for extra precision in upcoming
   calculations.
 */
  for (col=0; col < width; col++)
    gmcy[row*width+col][(*filter)(row,col)] = (pixel[col] & 0x3ff) << 4;
}

/*
   Filter pattern of the PowerShot Pro70:

	  0 1 2 3 4 5
	0 Y C Y C Y C		Return values
	1 M G M G M G		 0  1  2  3
	2 C Y C Y C Y		 G  M  C  Y
	3 G M G M G M
	4 Y C Y C Y C
	5 G M G M G M
	6 C Y C Y C Y
	7 M G M G M G
 */
pro70_filter(int row, int col)
{
  return (0x1e4b4e1b >> ((((row) << 1 & 14) + ((col) & 1)) << 1) & 3);
}

void pro70_read_crw(int row)
{
  uchar  data[1940], *dp;
  ushort pixel[1552], *pix;
  int col;

/*
  Each row is 1552 ten-bit pixels, packed into 1940 bytes.
 */
  fread(data, 1940, 1, ifp);
  for (dp=data, pix=pixel; dp < data+1940; dp+=10, pix+=8)
  {
    pix[0] = (dp[1] << 2) + (dp[0] >> 6);	/* Same as PS A5 */
    pix[1] = (dp[0] << 4) + (dp[3] >> 4);
    pix[2] = (dp[3] << 6) + (dp[2] >> 2);
    pix[3] = (dp[2] << 8) + (dp[5]     );
    pix[4] = (dp[4] << 2) + (dp[7] >> 6);
    pix[5] = (dp[7] << 4) + (dp[6] >> 4);
    pix[6] = (dp[6] << 6) + (dp[9] >> 2);
    pix[7] = (dp[9] << 8) + (dp[8]     );
  }

  memset(gmcy + row*width, 0, width*sizeof *gmcy);
/*
   Copy all pixels into the gmcy[] array.  Left-shift by 4 for
   extra precision in upcoming calculations.
 */
  for (col=0; col < width; col++)
    gmcy[row*width+col][(*filter)(row,col)] = (pixel[col] & 0x3ff) << 4;
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
int make_decoder(struct decode *dest, const uchar *source, int level)
{
  static struct decode *free;	/* Next unused node */
  static leaf;			/* number of leaves already added */
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

init_tables(int table)
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
    if (c == 0xff) fgetc(ifp);	/* always extra 00 after ff */
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
decompress(ushort *outbuf, int width, int count)
{
  struct decode *decode, *dindex;
  int i, leaf, len, sign, diff, diffbuf[64];
  static int carry, pixel, base[2];

  if (!count) {			/* Initialize */
    carry = pixel = 0;
    fseek (ifp, 540, SEEK_SET);
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
      if (pixel++ % width == 0)
	base[0] = base[1] = 512;
      outbuf[i] = ( base[i & 1] += diffbuf[i] );
    }
    outbuf += 64;
  }
}

/*
   Filter pattern of the PowerShot Pro90 and G1:

	  0 1 2 3 4 5		Return values
	0 G M G M G M		 0  1  2  3
	1 Y C Y C Y C		 G  M  C  Y
 */
pro90_filter(int row, int col)
{
  return (0xb4 >> ((((row) << 1 & 2) + ((col) & 1)) << 1) & 3);
}

void pro90_read_crw(int row)
{
  ushort pixel[1944*8];
  int r, col;

/* Read rows eight at a time */
  if (row & 7) return;
  decompress(pixel,1944,243);

  memset(gmcy + row*width, 0, 8*width*sizeof *gmcy);
  for (r=0; r < 8; r++)
    for (col=0; col < width; col++)
      gmcy[(row+r)*width+col][(*filter)(row+r,col)] =
		(pixel[(r*1944)+col] & 0x3ff) << 4;
}

void g1_read_crw(int row)
{
  ushort pixel[2144*2];
  int r, col;

/* Read rows two at a time */
  if (row & 1) return;

/* The first eight rows are blank.  Discard them. */
  if (row == 0)
    for (r=4; r--; )
      decompress(pixel,2144,67);

  decompress(pixel,2144,67);

  memset(gmcy + row*width, 0, 2*width*sizeof *gmcy);
  for (r=0; r < 2; r++)
    for (col=0; col < width; col++)
      gmcy[(row+r)*width+col][(*filter)(row+r,col)] =
		(pixel[(r*2144)+col+4] & 0x3ff) << 4;
}

/*
   Filter pattern of the PowerShot G2:

	  0 1 2 3 4 5		Return values
	0 G M G M G M		 0  1  2  3
	1 Y C Y C Y C		 G  M  C  Y
 */
g2_filter(int row, int col)
{
  return (0x4e >> ((((row) << 1 & 2) + ((col) & 1)) << 1) & 3);
}

void g2_read_crw(int row)
{
  ushort pixel[2376*8], *prow, (*grow)[4];
  int start=0, end, r, orow, col;

/*  Read rows eight at a time, discarding the first six rows */

  if (row == 0)
    start = 6;
  else if ((row & 7) != 2)
    return;

  decompress(pixel,2376,297);

  end = height - row;
  if (end > 8) end = 8;
  memset(gmcy + row*width, 0, end*width*sizeof *gmcy);
  for (r=start; r < end; r++) {
    orow = row-start + r;
    grow = gmcy + orow*width;
    prow = pixel + r*2376 + 12;
    for (col=0; col < width; col++)
      grow[col][(*filter)(orow,col)] = (prow[col] & 0x3ff) << 4;
  }
}

/*
   Search for string str in block data of length len.
   Return pointer to matching string, or zero if not found.
 */
char *search(char *data, int len, char *str)
{
  char *d;
  int slen, i;

  slen=strlen(str);
  for (d=data; d < data+len-slen; d++) {
    for (i=0; d[i]==str[i] && i++ < slen; );
    if (i >= slen) return d;
  }
  return 0;
}

#define tlen 0x8000
/*
   Open a CRW file, identify which camera created it, and set
   global variables accordingly.  Returns nonzero if an error occurs.
 */
open_and_id(char *fname)
{
  char head[26], tail[tlen], *name, *p;
  static const float def_coeff[3][4] = {
    { -2.400719,  3.539540, -2.515721,  3.421035 },	/* red from GMCY */
    {  4.013642, -1.710916,  0.690795,  0.417247 },	/* green from GMCY */
    { -2.345669,  3.385090,  3.521597, -2.249256 }	/* blue from GMCY */
  };
  float rgb_mul[3] = { 1.0, 1.0, 1.0 };
  int i, r, g;

  for (i=0; i < 4; i++) ymul[i]=1.0;

  ifp = fopen(fname,"rb");
  if (!ifp) {
    perror(fname);
    return 1;
  }
  fread (head,1,26,ifp);
  if (memcmp(head+6,"HEAPCCDR",8)) {
    fprintf(stderr,"%s is not a Canon CRW file.\n",fname);
    return 1;
  }
/*
   Read the last tlen bytes of the file
 */
  fseek (ifp, -tlen, SEEK_END);
  fread (tail, 1, tlen, ifp);
  fseek (ifp, 26, SEEK_SET);

  name = search(tail, tlen, "Canon PowerShot ");
  if (!name) {
    fprintf(stderr,"%s: camera is not a Canon PowerShot.\n",fname);
    return 1;
  } else if (!strcmp(name,"Canon PowerShot 600")) {
    height = 613;
    width  = 854;
    rgb_mul[1] = 0.6;
    rgb_mul[2] = 1.0;
    ymul[1] = 1.0125;
    ymul[3] = 0.9866;
    filter   = ps600_filter;
    read_crw = ps600_read_crw;
  } else if (!strcmp(name,"Canon PowerShot A5")) {
    height = 776;
    width  = 960;
    rgb_mul[1] = 0.90;
    rgb_mul[2] = 0.88;
    ymul[0] = 1.0056;
    ymul[1] = 0.9980;
    ymul[2] = 0.9959;
    ymul[3] = 1.0005;
    filter   = a5_filter;
    read_crw = a5_read_crw;
  } else if (!strcmp(name,"Canon PowerShot A50")) {
    height =  968;
    width  = 1290;
    rgb_mul[1] = 0.76;
    rgb_mul[2] = 0.59;
    filter   = a50_filter;
    read_crw = a50_read_crw;
  } else if (!strcmp(name,"Canon PowerShot Pro70")) {
    height = 1024;
    width  = 1552;
    rgb_mul[1] = 0.628;
    rgb_mul[2] = 0.792;
    filter   = pro70_filter;
    read_crw = pro70_read_crw;
  } else if (!strncmp(name,"Canon PowerShot Pro90",21)) {
    height = 1416;
    width  = 1896;
    rgb_mul[1] = 0.628;
    rgb_mul[2] = 0.792;
    filter   = pro90_filter;
    read_crw = pro90_read_crw;
    init_tables(name[4762]);
    decompress(0,0,0);
  } else if (!strcmp(name,"Canon PowerShot G1")) {
    height = 1550;
    width  = 2088;
    rgb_mul[1] = 0.628;
    rgb_mul[2] = 0.792;
    filter   = pro90_filter;
    read_crw = g1_read_crw;
    init_tables(name[4762]);
    decompress(0,0,0);
  } else if (!strcmp(name,"Canon PowerShot G2")) {
    height = 1720;
    height = 100;
    width  = 2312;
    rgb_mul[1] = 0.628;
    rgb_mul[2] = 0.792;
    filter   = g2_filter;
    read_crw = g2_read_crw;
    init_tables(name[4774]);
    decompress(0,0,0);
  } else {
    fprintf(stderr,"Sorry, the %s is not yet supported.\n",name);
    return 1;
  }
  for (r=0; r < 3; r++)	{
    for (g=0; g < 4; g++)
      coeff[r][g] = def_coeff[r][g] * rgb_mul[r];
  }
  return 0;
}
#undef tlen

/*
   When this function is called, we only have one GMCY value for
   each pixel.  Do linear interpolation to get the other three.

   read_crw(row+1) must happen before first_interpolate(row).
   first_interpolate() is non-destructive, so this row can be
   referenced while interpolating the next row.
 */
first_interpolate(int y)
{
  int x, sy, sx, c;
  static const uchar shift[]={ 2,1,2, 1,16,1, 2,1,2 }, *sp;

  for (x=1; x < width-1; x++)
  {
    sp=shift;
    for (sy=y-1; sy < y+2; sy++)
    {
      for (sx=x-1; sx < x+2; sx++)
      {
	c=(*filter)(sy,sx);
	gmcy[y*width+x][c] += gmcy[sy*width+sx][c] >> *sp++;
      }
    }
  }
}

/*
   Convert a GMCY quadruplet to an RGB triplet.

   The following table shows how the four CCD pixel types respond
   to the three primary colors, on a scale of 0-100.

     RGB--->   red    green    blue
    GMCY-v
    green	11	86	 8
    magenta	50	29	51
    cyan	11	92	75
    yellow	81	98	 8

   get_rgb() is based on an inversion of this table.
 */
get_rgb(float rgb[4], ushort gmcy[4])
{
  int r, g;

  memset(rgb,0,4 * sizeof (float));
  for (r=0; r < 3; r++)	{		/* RGB colors */
    for (g=0; g < 4; g++)		/* GMCY colors */
      rgb[r] += coeff[r][g] * gmcy[g];
    rgb[3] += rgb[r]*rgb[r];		/* Compute magnitude */
  }
}

/*
   Now that we have four GMCY values for each pixel (one known, three
   interpolated), adjust each interpolated value so that its ratio to
   the known value approximates that of neighboring pixels.

   second_interpolate(row) must be called after first_interpolate(row+1)
   A copy of the original row is needed to interpolating the next row.
   Therefore, second_interpolate() writes the modified row one pixel
   above and to the left of the original.

   Edge pixels are discarded.  Pixels one in from the edge are memcpy'd
   to their new locations.

   Convert each GMCY value to RGB, and compile a histogram of their
   magnitudes.  Discard the RGB values.
 */
second_interpolate(int y)
{
  int x, c, sy, sx, sc;
  ushort this[4];
  static const uchar shift[]={ 2,1,2, 1,  1, 2,1,2 }, *sp;
  float rgb[4];
  unsigned val;

  if (y==1 || y==height-2)	/* y is never outside this range */
  {
    memcpy(gmcy+(y-1)*width,gmcy+y*width+1,(width-2)*sizeof this);
    return;
  }
  if (y==2) memset(histogram,0,sizeof histogram);
  memcpy(gmcy+(y-1)*width,gmcy+y*width+1,sizeof this);
  for (x=2; x < width-2; x++)
  {
    c=(*filter)(y,x);
    sp=shift;
    memset(this,0,sizeof this);
    this[c]=gmcy[y*width+x][c];
    for (sy=y-1; sy < y+2; sy++)	/* 28% of run-time is this loop */
      for (sx=x-1; sx < x+2; sx = sx+1+(sy==y))
      {
	sc=(*filter)(sy,sx);
	this[sc] +=
	 ( (unsigned long) gmcy[sy*width+sx][sc] << 16) /
	    gmcy[sy*width+sx][c] * gmcy[y*width+x][c] >> (16 + *sp++);
      }
    memcpy(gmcy+(y-1)*width+x-1,this,sizeof this);
    get_rgb(rgb,this);
    val = rgb[3]/0x1000000;	/* Collect statistics */
    if (val > 1023) val=1023;
    histogram[val]++;
  }
  memcpy(gmcy+(y-1)*width+x-1,gmcy+y*width+x,sizeof this);
}

/*
   Convert the GMCY grid to RGB and write it to a PPM file.
 */
write_ppm(FILE *ofp)
{
  int y, x, i;
  register unsigned c, val;
  uchar (*ppm)[3];
  float rgb[4], max, max2, expo, scale;
  float gymul[4];
  int total;
  char p6head[32];

/*
   Set the maximum magnitude to the 98th percentile
 */
  for (val=1024, total=0; --val; )
    if ((total+=histogram[val]) > (int)(width*height*0.06)) break;
  max2 = val << 24;
  max = sqrt(max2);

  fprintf(ofp,"P6\n%d %d\n255\n",width-2,height-2);

  ppm = calloc(width-2,3);
  if (!ppm) {
    perror("ppm calloc failed");
    exit(1);
  }
  expo = (gamma_val-1)/2;		/* Pull these out of the loop */
  for (y=0; y < 4; y++)
    gymul[y] = bright * 362 / max * pow(ymul[y],gamma_val);

  for (y=0; y < height-2; y++)
  {
    for (x=0; x < width-2; x++)
    {
      get_rgb(rgb,gmcy[y*width+x]);
      scale = gymul[y&3] * pow(rgb[3]/max2,expo);

      for (c=0; c < 3; c++)
      {
	val=rgb[c]*scale;
	if (val < 0) val=0;
	if (val > 255) val=255;
	ppm[x][c]=val;
      }
    }
    fwrite (ppm, width-2, 3, ofp);
  }
  free (ppm);
}

/*
   Creates a new filename with a different extension
 */
exten(char *new, const char *old, const char *ext)
{
  char *cp;

  strcpy(new,old);
  cp=strrchr(new,'.');
  if (!cp) cp=new+strlen(new);
  strcpy(cp,ext);
}

main(int argc, char **argv)
{
  char data[256];
  int i, arg, write_to_files=1, row;
  FILE *ofp;

  if (argc == 1)
  {
    fprintf(stderr,
    "\nCanon PowerShot Converter v1.92"
    "\nby Dave Coffin (dcoffin@shore.net)"
    "\n\nUsage:  %s [options] file1.crw file2.crw ...\n"
    "\nValid options:"
    "\n-c        Write PPM to standard output"
    "\n-g <num>  Set gamma value (%5.3f by default)"
    "\n-b <num>  Set brightness  (%5.3f by default)\n\n",
      argv[0], gamma_val, bright);
    exit(1);
  }

/* Parse out the options */

  for (arg=1; argv[arg][0] == '-'; arg++)
    switch (argv[arg][1])
    {
      case 'c':
	write_to_files = 0;  break;
      case 'g':
	gamma_val = atof(argv[++arg]);  break;
      case 'b':
	bright = atof(argv[++arg]);  break;
      default:
	fprintf(stderr,"Unknown option \"%s\"\n",argv[arg]);
	exit(1);
    }

/* Process the named files  */

  for ( ; arg < argc; arg++)
  {
    if (open_and_id(argv[arg])) {
      if (ifp) fclose(ifp);
      continue;
    }
    gmcy = calloc(height*width,sizeof *gmcy);
    if (!gmcy) {
      perror("gmcy calloc failed");
      exit(1);
    }
    for (row=0; row < 3; row++)
      (*read_crw)(row);
    first_interpolate(1);

/* This loop is a good place to put a progress bar */

    for (row=3; row < height; row++) {
      (*read_crw)(row);
      first_interpolate(row-1);
      second_interpolate(row-2);
    }
    second_interpolate(height-2);
    fclose(ifp);

    if (write_to_files) {
      exten(data, argv[arg],".ppm");
      ofp = fopen(data,"wb");
      if (!ofp) {
	perror(data);
	continue;
      }
      write_ppm(ofp);
      fclose(ofp);
    } else
      write_ppm(stdout);
    free (gmcy);
  }
}
