/*
   CRW to PPM converter Version 0.4

   by Dave Coffin (dcoffin at shore dot net)  8/24/97

   No rights reserved.  Do what you want with this code,
   but I accept no responsibility for any consequences
   of its (mis)use.
*/

#include <ctype.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

/*  Create extra files with debugging info */

#define DEBUG

/* Use these to adjust the final color balance */

#define RED_MUL 1.0
#define GRN_MUL 0.5
#define BLU_MUL 1.0

/* DOS likes to trash binary files!! */

#ifndef O_BINARY
#define O_BINARY 0
#endif

#define WFLAGS O_WRONLY | O_CREAT | O_TRUNC | O_BINARY

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

#ifdef DEBUG
debug (char *arg, char *ext)
{
  char fname[64];
  int fd;

  exten(fname,arg,ext);
  fd = open(fname,WFLAGS,0644);
  if (fd < 0)
  { perror(fname);
    fd = open("/dev/null",O_WRONLY);
  }
  return fd;
}
#endif

/*
   Returns the color of a given pixel.  The pattern goes:

(0,0)->	G M G M G M	row 0
	C Y C Y C Y	row 1
	M G M G M G	row 2
	C Y C Y C Y	row 3
*/
color (unsigned x, unsigned y)
{
  int color;

  color = ((y & 1) << 1) + (x & 1);
  if ((y & 3) == 2)		/* 0=green, 1=magenta, 2=cyan, 3=yellow */
    color ^= 1;			/* Swap green and magenta on row 2 */
  return color;
}

/* Read a CRW file into the pgm[] array */
read_crw(char *arg)
{
  uchar data[1120], *dp, *gp;
  int fd, irow, icol, orow, ocol;

#ifdef DEBUG
  int gfd, gcol, mfd, mcol;
  uchar grid[896], myst[224];

  gfd = debug(arg,".grid");
  mfd = debug(arg,".myst");

  write(gfd,"P5\n896 613\n255\n",15);
  write(mfd,"P4\n1792 613\n",12);
#endif

  fd = open(arg,O_RDONLY | O_BINARY);
  if (fd < 0)
  { perror(arg);
    return 0; }

  read (fd, data, 26);				/* Check the header */
  if (memcmp(data,"MM",2) || memcmp(data+6,"HEAPCCDR",8))
  {
    fprintf(stderr,"%s is not a CRW file.\n",arg);
    return 0;
  }

  for (irow=orow=0; irow < 613; irow++)
  {
    read (fd, data, 1120);                      /* Read one row */

#ifdef DEBUG
    for (dp=data, gp=grid; dp < data+1120; dp+=10, gp+=8)
    {
/* wrong */
      gp[0]=(dp[0] << 2) + (dp[1] >> 4 & 3);
      gp[1]=(dp[2] << 2) + (dp[1] >> 2 & 3);
      gp[2]=(dp[3] << 2) + (dp[1] >> 0 & 3);
      gp[3]=(dp[4] << 2) + (dp[1] >> 6 & 3);
      gp[4]=(dp[5] << 2) + (dp[9] >> 0 & 3);
      gp[5]=(dp[6] << 2) + (dp[9] >> 2 & 3);
      gp[6]=(dp[7] << 2) + (dp[9] >> 4 & 3);
      gp[7]=(dp[8] << 2) + (dp[9] >> 6 & 3);

#if 0
/* right */
      gp[0]=(dp[0] << 2) + (dp[1] >> 6 & 3);
      gp[1]=(dp[2] << 2) + (dp[1] >> 4 & 3);
      gp[2]=(dp[3] << 2) + (dp[1] >> 2 & 3);
      gp[3]=(dp[4] << 2) + (dp[1] >> 0 & 3);
      gp[4]=(dp[5] << 2) + (dp[9] >> 0 & 3);
      gp[5]=(dp[6] << 2) + (dp[9] >> 2 & 3);
      gp[6]=(dp[7] << 2) + (dp[9] >> 4 & 3);
      gp[7]=(dp[8] << 2) + (dp[9] >> 6 & 3);
#endif
    }
    lseek (gfd, orow*896+15, 0);
    write (gfd, grid, 896);
    lseek (mfd, orow*224+12, 0);
    write (mfd, myst, 224);
#endif

#if 0
    for (icol=gcol=mcol=0; icol < 1120; icol++)
    {
      if ( icol % 10 != 1 && icol % 10 != 9 )
	grid[gcol++] = data[icol];		/* Data pixels go here */
      else
	myst[mcol++] = data[icol];		/* Mystery pixels go here */
    }
    lseek (gfd, orow*896+15, 0);
    write (gfd, grid, 896);
    lseek (mfd, orow*224+12, 0);
    write (mfd, myst, 224);
#endif

    for (icol=ocol=0; icol < 1067; icol++)
    {
      if ( icol % 10 != 1 && icol % 10 != 9 )
      {
	gmcy[orow][ocol][color(orow,ocol)] = (ushort) data[icol] << 6;
	ocol++;
      }
    }
    if ((orow+=2) > 612)        /* Once we've read all the even rows, */
      orow = 1;                 /* read the odd rows. */
  }
  close(fd);

#ifdef DEBUG
  close(gfd);
  close(mfd);
#endif

  return 1;                     /* Success */
}

/*
   Converts a GMCY quadruplet into an RGB triplet.

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
get_rgb(float rgb[3], uchar gmcy[4])
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

  memset(rgb,0,12);
  for (r=0; r < 3; r++)		/* RGB colors */
    for (g=0; g < 4; g++)	/* GMCY colors */
      rgb[r] += coeff[r][g] * gmcy[g];
}


#if 0

/*
     For all values of (x,y), the pixels (x,y), (x+1,y), (x,y+1),
and (x+1,y+1) form a GMCY vector (i.e. no two have the same color).
Convert these vectors to RGB, then try to remove the artifacts that
appear when there's an edge between x and x+1.
*/
write_ppm(char *fname)
{
  int out, y, x, sy, sx, c, val, prev;
  uchar gmcy[4], ppm[853][3];
  float rgb[3];

  out = open(fname,WFLAGS,0644);
  if (out < 0)
  { perror(fname);
    return; }
  write(out,"P6\n853 612\n255\n",15);

  for (y=0; y < 612; y++)
  {
    for (x=0; x < 853; x++)
    {
      for (sy=y; sy < y+2; sy++)
	for (sx=x; sx < x+2; sx++)
	  gmcy[color(sx,sy)] = pgm[sy][sx];
      get_rgb(rgb,gmcy);
      for (c=0; c < 3; c++)
      {
	val = floor(rgb[c]);
	if (val < 0) val=0;
	if (val > 255) val=255;
	ppm[x][c] = val;
      }
    }
    write (out, ppm, sizeof ppm);
  }
  close(out);
}
#endif

main(int argc, char **argv)
{
  char fname[256];
  int arg, fd, x, y;
  short magic;

  if (argc < 2)
  {
    puts("\nCRW to PPM converter by Dave Coffin (dcoffin@shore.net)");
    puts("Version 0.4, last modified 8/24/97\n");
    printf("Usage:  %s file1.crw file2.crw ...\n\n",argv[0]);
    exit(1);
  }

  for (arg=1; arg < argc; arg++)
  {
    if (read_crw(argv[arg]))
    {
      exten(fname, argv[arg],".ppm");	/* Open a PPM file for RGB output */
#if 0
      write_ppm(fname);
#endif
    }
  }
}
