/*
   Canon PowerShot Converter v0.96

   by Dave Coffin (dcoffin@shore.net)

   No rights reserved.  Do what you want with this code,
   but I accept no responsibility for any consequences
   of its (mis)use.

   $Revision: 1.17 $
   $Date: 2000/05/02 13:45:14 $
*/

#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG

#ifdef PS_600

#define H 613
#define W 854

#define RED_MUL 1.0
#define GRN_MUL 0.6
#define BLU_MUL 1.0

/* Use this to remove annoying horizontal patterns */
const float ymul[4] = { 0.9866, 1.0, 1.0125, 1.0 };

#elif defined(PS_A5)

#define H 776
#define W 960

#define RED_MUL 1.0
#define GRN_MUL 0.90
#define BLU_MUL 0.88

const float ymul[4] = { 1.0005, 1.0056, 0.9980, 0.9959 };

#elif defined(PS_A50)

#define H 968
#define W 1290

#define RED_MUL 1.0
#define GRN_MUL 0.76
#define BLU_MUL 0.59

const float ymul[4] = { 1.0, 1.0, 1.0, 1.0 };

#else
#error You must compile with -DPS_600, -DPS_A5, or -DPS_A50
#endif

/* Default values, which may be modified on the command line */

float gamma_val=0.8, bright=1.0;

/* DOS likes to trash binary files!! */

#ifndef O_BINARY
#define O_BINARY 0
#endif

#define WFLAGS O_WRONLY | O_CREAT | O_TRUNC | O_BINARY

typedef unsigned char uchar;

/* This array holds the GMCY values for each pixel */

ushort gmcy[H][W][4];
int histogram[1024];

#ifdef PS_600
/*
   Returns the filter color of a given pixel.
   The pattern is:

	  0 1 2 3 4 5
	0 G M G M G M		Return values
	1 C Y C Y C Y		 0  1  2  3
	2 M G M G M G		 G  M  C  Y
	3 C Y C Y C Y
*/

#define filter(row,col) \
	(0xe1e4 >> ((((row) << 1 & 6) + ((col) & 1)) << 1) & 3)

/*
   Load CCD pixel values into the gmcy[] array.  Unknown colors
   (such as cyan under a magenta filter) must be set to zero.
*/
void read_crw(int row, int fd)
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

  for (irow=orow=0; irow < H; irow++)
  {
    read (fd, data, 1120);
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
   Copy 854 pixels into the gmcy[] array.  The other 42 pixels
   are blank.  Left-shift by 4 for extra precision in upcoming
   calculations.
*/
    memset(gmcy[orow], 0, W*8);		/* Set row to zero */
    for (col=0; col < W; col++)
      gmcy[orow][col][filter(orow,col)] = pixel[col] << 4;

    if ((orow+=2) > H)	      /* Once we've read all the even rows, */
      orow = 1;			/* read the odd rows. */
  }
}

#elif defined(PS_A5)

/*
   Returns the filter color of a given pixel.
   The pattern is:

	  0 1 2 3 4 5
	0 C Y C Y C Y		Return values
	1 G M G M G M		 0  1  2  3
	2 C Y C Y C Y		 G  M  C  Y
	3 M G M G M G
*/

#define filter(row,col) \
	(0x1e4e >> ((((row) << 1 & 6) + ((col) & 1)) << 1) & 3)

/*
   Load CCD pixel values into the gmcy[] array.  Unknown colors
   (such as cyan under a magenta filter) must be set to zero.
*/
void read_crw(int row, int fd)
{
  uchar  data[1240], *dp;
  ushort pixel[992], *pix;
  int row, col;

/*
   Each data row is 992 ten-bit pixels, packed into 1240 bytes.
*/
  read(fd,data,1240);
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
/*
   Copy 960 pixels into the gmcy[] array.  The other 32 pixels
   are blank.  Left-shift by 4 for extra precision in upcoming
   calculations.
*/
  memset(gmcy[row], 0, W*8);		/* Set row to zero */
  for (col=0; col < W; col++)
    gmcy[row][col][filter(row,col)] = (pixel[col] & 0x3ff) << 4;
}

#elif defined(PS_A50)

/*
   Returns the filter color of a given pixel.
   The pattern is:

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
#define filter(row,col) \
	(0x1b4e4b1e >> ((((row) << 1 & 14) + ((col) & 1)) << 1) & 3)

/*
   Load CCD pixel values into the gmcy[] array.  Unknown colors
   (such as cyan under a magenta filter) must be set to zero.
*/
void read_crw(int row, int fd)
{
  uchar  data[1650], *dp;
  ushort pixel[1320], *pix;
  int col;

/*
  Each row is 1320 ten-bit pixels, packed into 1650 bytes.
*/
  read(fd,data,1650);
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
   Copy 1290 pixels into the gmcy[] array.  The other 30 pixels
   are blank.  Left-shift by 4 for extra precision in upcoming
   calculations.
*/
  memset(gmcy[row], 0, W*8);		/* Set row to zero */
  for (col=0; col < W; col++)
    gmcy[row][col][filter(row,col)] = (pixel[col] & 0x3ff) << 4;
}

#endif		/* End of model-specific code */

/*
   When this function is called, we only have one GMCY value for
   each pixel.  Do linear interpolation to get the other three.

   first_interpolate(row) must be called after read_crw(row+1)!
*/
first_interpolate(int y)
{
  int x, sy, sx, c;
  uchar shift[]={ 2,1,2, 1,16,1, 2,1,2 }, *sp;

  for (x=1; x < W-1; x++)
  {
    sp=shift;
    for (sy=y-1; sy < y+2; sy++)
    {
      for (sx=x-1; sx < x+2; sx++)
      {
	c=filter(sy,sx);
	gmcy[y][x][c] += gmcy[sy][sx][c] >> *sp++;
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

   get_rgb() is based on this table.
*/

get_rgb(float rgb[4], ushort gmcy[4])
{
  int r, g;
  static const float coeff[3][4] =
  {
    { -2.400719 * RED_MUL,  3.539540 * RED_MUL,	 /* red from GMCY */
      -2.515721 * RED_MUL,  3.421035 * RED_MUL },
    {  4.013642 * GRN_MUL, -1.710916 * GRN_MUL,  /* green from GMCY */
       0.690795 * GRN_MUL,  0.417247 * GRN_MUL },
    { -2.345669 * BLU_MUL,  3.385090 * BLU_MUL,  /* blue from GMCY */
       3.521597 * BLU_MUL, -2.249256 * BLU_MUL }
  };

  memset(rgb,0,16);
  for (r=0; r < 3; r++)			/* RGB colors */
  {
    for (g=0; g < 4; g++)		/* GMCY colors */
      rgb[r] += coeff[r][g] * gmcy[g];
    rgb[3] += rgb[r]*rgb[r];		/* Compute magnitude */
  }
}

/*
   We now have all four GMCY values for each pixel.  Smooth the color
   balance to avoid artifacts.  As a side effect, this moves the whole
   image one pixel up and to the left.  Convert each GMCY value to RGB,
   and compile a histogram of their magnitudes.  RGB values are discarded
   and re-calculated in write_ppm().

   second_interpolate(row) must be called after first_interpolate(row+1)!
*/

second_interpolate(int y)
{
  int x, c, sy, sx, sc;
  ushort this[4];
  static const uchar shift[]={ 2,1,2, 1,0,1, 2,1,2 }, *sp;
  float rgb[4];
  unsigned val;

  if (y==2) memset(histogram,0,sizeof histogram);
  for (x=2; x < W-2; x++)
  {
    memset(this,0,sizeof this);
    c=filter(y,x);
    sp=shift;
    for (sy=y-1; sy < y+2; sy++)
      for (sx=x-1; sx < x+2; sx++)
      {
	sc=filter(sy,sx);
	this[sc] +=
	 ( (unsigned long) gmcy[sy][sx][sc] << 16) /
	    gmcy[sy][sx][c] * gmcy[y][x][c] >> (16 + *sp++);
      }
    memcpy(&gmcy[y-1][x-1],this,sizeof this);
    get_rgb(rgb,this);
    val = rgb[3]/0x1000000;	/* Collect statistics */
    if (val > 1023) val=1023;
    histogram[val]++;
  }
}

/*
   Convert the GMCY grid to RGB and write it to a PPM file.
*/
write_ppm(int fd)
{
  int y, x, i;
  register unsigned c, val;
  uchar ppm[W][3];
  float rgb[4], max, max2, expo, scale;
  float gymul[4];
  int total;
  char p6head[32];

/*
   Set the maximum magnitude to the 98th percentile
*/
  for (val=1024, total=0; --val; )
    if ((total+=histogram[val]) > (int)(W*H*0.06)) break;
  max2 = val << 24;
  max = sqrt(max2);

#ifdef DEBUG
  fprintf(stderr,"Writing PPM file...\n");
#endif

  write(fd,p6head,sprintf(p6head,"P6\n%d %d\n255\n",W-2,H-2));

/*
   Second pass:  Scale RGB and write to PPM file
*/

  expo = (gamma_val-1)/2;		/* Pull these out of the loop */
  for (y=0; y < 4; y++)
    gymul[y] = bright * 362 / max * pow(ymul[y],gamma_val);

  for (y=1; y < H-1; y++)
  {
    for (x=1; x < W-1; x++)
    {
      get_rgb(rgb,gmcy[y][x]);
      scale = gymul[y&3] * pow(rgb[3]/max2,expo);

      for (c=0; c < 3; c++)
      {
	val=rgb[c]*scale;
	if (val < 0) val=0;
	if (val > 255) val=255;
	ppm[x][c]=val;
      }
    }
    write (fd, ppm+1, (W-2)*3);
  }

#ifdef DEBUG
  fprintf(stderr,"Done!\n");
#endif
}

/* Creates a new filename with a different extension */
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
  int i, arg, fd, write_to_files=1, row;
  ushort dummy;

  if (argc == 1)
  {
    fprintf(stderr,
    "\nCanon PowerShot "
#ifdef PS_600
    "600"
#elif defined(PS_A5)
    "A5"
#elif defined(PS_A50)
    "A50"
#endif
    " Converter v0.96"
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

/* Process the named files */

  for ( ; arg < argc; arg++)
  {
    fd = open(argv[arg],O_RDONLY | O_BINARY);
    if (fd < 0)
    { perror(argv[arg]);
      continue; }

/* Check the header to confirm this is a CRW file */

    read (fd, data, 26);
    if (memcmp(data+6,"HEAPCCDR",8))
    {
      fprintf(stderr,"%s is not a Canon PowerShot CRW file.\n",argv[arg]);
      continue;
    }
    for (row=0; row < H; row++)
    {
      for (i=0; i < W; i+=4)
	dummy=gmcy[row][i][0];
      read_crw(row,fd);
      if (row > 1) first_interpolate(row-1);
      if (row > 3) second_interpolate(row-2);
    }
    close(fd);
    if (write_to_files)
    {
      exten(data, argv[arg],".ppm");
      fd = open(data,WFLAGS,0644);
      if (fd < 0)
      { perror(data);
	return; }
      write_ppm(fd);
      close(fd);
    } else
      write_ppm(1);
  }
}
