/*
   CRW to PPM converter Version 0.85

   by Dave Coffin (dcoffin at shore dot net)  8/30/97

   No rights reserved.  Do what you want with this code,
   but I accept no responsibility for any consequences
   of its (mis)use.
*/

#include <ctype.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG

/* Use these to adjust the final color balance */

#define RED_MUL 1.0
#define GRN_MUL 0.6
#define BLU_MUL 1.0

/* Default values, which may be modified on the command line */

float gamma_val=0.8, bright=1.0;

/* DOS likes to trash binary files!! */

#ifndef O_BINARY
#define O_BINARY 0
#endif

#define WFLAGS O_WRONLY | O_CREAT | O_TRUNC | O_BINARY

int write_to_files=1;

typedef unsigned char  uchar;

/* This 4MB array holds the GMCY values for each pixel */

ushort gmcy[613][854][4];

/* Creates a new filename with a different extension */
exten(char *new, char *old, char *ext)
{
  char *cp;

  strcpy(new,old);
  cp=strrchr(new,'.');
  if (!cp) cp=new+strlen(new);
  strcpy(cp,ext);
}

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

read_crw(char *fname)
{
  uchar  data[1120], *dp;
  ushort pixel[896], *pix;
  int fd, irow, orow, col;

  fd = open(fname,O_RDONLY | O_BINARY);
  if (fd < 0)
  { perror(fname);
    return 0; }

/* Check the header to confirm this is a CRW file */

  read (fd, data, 26);
  if (memcmp(data,"MM",2) || memcmp(data+6,"HEAPCCDR",8))
  {
    fprintf(stderr,"%s is not a CRW file.\n",fname);
    return 0;
  }

#ifdef DEBUG
  fprintf(stderr,"Unpacking %s...\n",fname);
#endif

/*
    Immediately after the 26-byte header come the data rows.  First
the even rows 0..612, then the odd rows 1..611.  Each row is 896
pixels, ten bits per pixel, packed into 1120 bytes (8960 bits).
*/

  for (irow=orow=0; irow < 613; irow++)
  {
/* Read one packed row */
    read (fd, data, 1120);

/* Unpack the pixels */
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
    memset(gmcy[orow], 0, 854 * 8);	/* Set row to zero */
    for (col=0; col < 854; col++)
      gmcy[orow][col][filter(orow,col)] = pixel[col] << 4;

    if ((orow+=2) > 612)        /* Once we've read all the even rows, */
      orow = 1;                 /* read the odd rows. */
  }
  close(fd);
  return 1;                     /* Success */
}

/*
   When this function is called, we only have one GMCY
value for each pixel.  Do linear interpolation to get
the other three.
*/

first_interpolate()
{
  int y, x, sy, sx, c;
  uchar shift[]={ 2,1,2, 1,16,1, 2,1,2 }, *sp;

#ifdef DEBUG
  fprintf(stderr,"First interpolation...\n");
#endif

  for (y=1; y < 612; y++)
  {
    for (x=1; x < 853; x++)
    {
      sp=shift;
      for (sy=y-1; sy < y+2; sy++)
	for (sx=x-1; sx < x+2; sx++)
	{
	  c=filter(sy,sx);
	  gmcy[y][x][c] += gmcy[sy][sx][c] >> *sp++;
	}
    }
  }
}

/*
   We now have all four GMCY values for each pixel.  Smooth
the color balance to avoid artifacts.  This function may be
called more than once.
*/

second_interpolate()
{
  ushort data[2][854][4];
  ushort (*last_row)[4]=data[0];
  ushort (*this_row)[4]=data[1];
  void *tmp;

  int y, x, c, sy, sx, sc;
  uchar shift[]={ 2,1,2, 1,0,1, 2,1,2 }, *sp;

#ifdef DEBUG
  fprintf(stderr,"Second interpolation...\n");
#endif

  for (y=2; y < 611; y++)
  {
    memset(this_row, 0, 854*8);
    for (x=2; x < 852; x++)
    {
      sp=shift;
      c=filter(y,x);
      for (sy=y-1; sy < y+2; sy++)
	for (sx=x-1; sx < x+2; sx++)
	{
	  sc=filter(sy,sx);
	  this_row[x][sc] +=
	   ( (unsigned long) gmcy[sy][sx][sc] << 16)
	    / gmcy[sy][sx][c] * gmcy[y][x][c] >> (16 + *sp++);
	}
    }
    if (y > 2) memcpy(gmcy[y-1]+2,last_row+2,850*8);
    tmp = last_row;
    last_row = this_row;
    this_row = tmp;
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

get_rgb(float rgb[4], ushort gmcy[4])		/* 3.70 seconds */
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
   Convert the GMCY grid to RGB and write it to a PPM file.
*/

write_ppm(char *fname)
{
  int fd=1, y, x, i;
  register c, val;
  uchar ppm[854][3];
  float rgb[4], max, max2, expo, mult, scale;
  int histo[512], total;

/* Use this to remove an annoying horizontal pattern */
  float ymul[4]={ 0.9866, 1.0, 1.0125, 1.0 };

#ifdef DEBUG
  fprintf(stderr,"First pass RGB...\n");
#endif

/*
   First pass:  Gather stats on the RGB image
*/
  memset(histo,0,sizeof histo);
  for (y=2; y < 611; y++)
  {
    for (x=2; x < 852; x++)
    {
      get_rgb(rgb,gmcy[y][x]);
      for (c=0; c < 3; c++)
      {
	histo [(int)rgb[c] >> 10]++;
      }
    }
  }
/*
   Set the maximum to the 96th percentile
*/
  for (val=512, total=0; --val; )
    if ((total+=histo[val]) > 60000) break;
  max = val << 10;
  max2 = max*max;

#ifdef DEBUG
  fprintf(stderr,"Second pass RGB...\n");
#endif

  if (write_to_files)
  {
    fd = open(fname,WFLAGS,0644);
    if (fd < 0)
    { perror(fname);
      return; }
  }
  write(fd,"P6\n852 611\n255\n",15);

/*
   Second pass:  Scale RGB and write to PPM file
*/

  expo = (gamma_val-1)/2;			/* Pull these out of the loop */
  mult = bright * 362 / max;
  for (y=0; y < 4; y++)
    ymul[y] = pow(ymul[y],gamma_val);

  for (y=1; y < 612; y++)
  {
    for (x=1; x < 853; x++)
    {
      get_rgb(rgb,gmcy[y][x]);
      scale = mult * ymul[y&3] * pow(rgb[3]/max2,expo);

      for (c=0; c < 3; c++)
      {
	val=rgb[c]*scale;
	if (val < 0) val=0;
	if (val > 255) val=255;
	ppm[x][c]=val;
      }
    }
    write (fd, ppm+1, 852 * 3);
  }
  if (write_to_files) close(fd);

#ifdef DEBUG
  fprintf(stderr,"Done!\n");
#endif
}

main(int argc, char **argv)
{
  char fname[256];
  int arg;

  if (argc == 1)
  {
    fprintf(stderr,
    "\nCRW to PPM converter by Dave Coffin (dcoffin@shore.net)"
    "\nVersion 0.85, last modified 8/30/97\n"
    "\nUsage:  %s [options] file1.crw file2.crw ...\n"
    "\nValid options:"
    "\n-c        Write to standard output"
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
    if (read_crw(argv[arg]))
    {
      first_interpolate();
      second_interpolate();
      exten(fname, argv[arg],".ppm");
      write_ppm(fname);
    }
  }
}
