/*
   CRW to PPM converter Version 0.2

   by Dave Coffin (dcoffin at shore dot net)  2/24/97

   No rights reserved.  Do what you want with this code,
   but I accept no responsibility for any consequences
   of its (mis)use.
*/

#include <ctype.h>
#include <fcntl.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

typedef unsigned char uchar;

/* This array holds the raw pixel values */
uchar pgm[613][854];

/*
   Write the raw pixel values to a PGM file before
   RGB conversion.  Useful for debugging.
*/
#define WANT_PGM_FILE

/* DOS likes to trash binary files!! */
#ifndef O_BINARY
#define O_BINARY 0
#endif

#define WFLAGS O_WRONLY | O_CREAT | O_TRUNC | O_BINARY

/* Read a CRW file into the pgm[] array */
read_crw(int in)
{
  uchar data[1120];
  int bin, irow, icol, orow, ocol;

  read (in, data, 24);				/* Chop the header */

  for (irow=orow=0; irow < 613; irow++)
  {
    read (in, data, 1120);                      /* Read one row */
    for (icol=ocol=0; icol < 1067; icol++)
    {
      if ( icol % 10 != 1 && icol % 10 != 9 )   /* Cut out noise columns */
	pgm[orow][ocol++] = data[icol];
    }
    if ((orow+=2) > 612)        /* Once we've read all the even rows, */
      orow = 1;                 /* read the odd rows. */
  }
  close(in);
  return 0;                     /* Success */
}

/* Used for parsing PGM headers */
int get_num(FILE *fp)
{
  int num=0;
  char c;

  for (;;)
  {
    if (isspace(c=getc(fp)))		/* Ignore whitespace */
      continue;
    if ( c=='#' )			/* Ignore comments */
      while (getc(fp) != '\n');
    else if (isdigit(c))		/* Grab digits */
    { do {
	num = num*10 + (c - '0');	/* Make a number */
	c=getc(fp);
      } while (isdigit(c));
      return num;			/* Return it */
    } else
      fprintf(stderr,"Illegal byte in header\n");
  }
}

/* Read a PGM file, skipping the CRW -> PGM conversion */
read_pgm(int in)
{
  int width, height, maxval;
  FILE *fp;

#if O_BINARY != 0
  fp=fdopen(in,"rb");
#else
  fp=fdopen(in,"r");
#endif
  width=get_num(fp);
  height=get_num(fp);
  maxval=get_num(fp);

  if (width != 854 || height != 613 || maxval != 255)
  { fprintf(stderr, "PGM file must be 854x613, 256 colors!\n");
    fclose(fp);
    return 1;
  }
  fread(pgm,854,613,fp);
  fclose(fp);
  return 0;
}

/*
   Converts a GMCY quadruplet into an RGB triplet.
   Ask Canon how this works; I have no clue.
*/
get_rgb(float rgb[3], int gmcy[4])
{
  int r, g;
  static const float coeff[3][4] =
  {
    { -5.175675,  3.853136, -1.681311,  5.270232 },
    {  1.281218, -1.147541,  1.935073,  0.487001 },
    { -3.167515,  2.900609,  4.132598, -1.720971 }
  };

  memset(rgb,0,12);
  for (r=0; r < 3; r++)		/* RGB colors */
    for (g=0; g < 4; g++)	/* GMCY colors */
      rgb[r] += coeff[r][g] * gmcy[g];
}

/*
   Returns the color of a given pixel.  The pattern goes:

(0,0)->	G M G M G M
	C Y C Y C Y
	M G M G M G
	C Y C Y C Y
*/
color (unsigned x, unsigned y)
{
  int color;

  color = ((y & 1) << 1) + (x & 1);
  if ((y & 3) == 2)		/* 0=green, 1=magenta, 2=cyan, 3=yellow */
    color ^= 1;			/* Swap green and magenta */
  return color;
}

/*
   Interpolate GMCY values for each non-edge pixel (using
   all eight neighbors), then convert the result to RGB.
*/
write_ppm(char *fname)
{
  int out, y, x, sy, sx, c, val, gmcy[4];
  uchar ppm[852][3];
  float rgb[3];
  uchar shift[]={ 0,1,0, 1,2,1, 0,1,0 }, *sp;

  out = open(fname,WFLAGS,0644);
  if (out < 0)
  { perror(fname);
    return; }
  write(out,"P6\n852 611\n255\n",15);

  for (y=0; y < 611; y++)
  {
    for (x=0; x < 852; x++)
    {
      memset(gmcy,0,sizeof gmcy);
      sp=shift;
      for (sy=y; sy < y+3; sy++) /* Average a 3x3 block */
	for (sx=x; sx < x+3; sx++)
	  gmcy[color(sx,sy)] += pgm[sy][sx] << *sp++;

      get_rgb(rgb,gmcy);	/* Convert GMCY averages to RGB */
      for (c=0; c < 3; c++)
      {
	val = floor(rgb[c]/4);	/* Save the RGB values */
	if (val < 0) val=0;
	if (val > 255) val=255;
	ppm[x][c] = val;
      }
    }
    write (out, ppm, sizeof ppm);
  }
  close(out);
}

/* Creates a new filename with a different extension */
exten(char *new, char *old, char *ext)
{
  char *cp;

  strcpy(new,old);
  cp=strrchr(new,'.');
  if (!cp) cp=new+strlen(new);
  strcpy(cp,ext);
}

main(int argc, char **argv)
{
  char fname[256];
  int arg, fd, x, y;
  short magic;

  if (argc < 2)
  {
    puts("\nCRW to PPM converter by Dave Coffin (dcoffin@shore.net)");
    puts("Version 0.2, last modified 2/24/97\n");
    printf("Usage:  %s file1.crw file2.crw ...\n\n",argv[0]);
    exit(1);
  }

  for (arg=1; arg < argc; arg++)
  {
    fd = open(argv[arg],O_RDONLY | O_BINARY);
    if (fd < 0)
    { perror(argv[arg]);
      continue; }
    read(fd,&magic,2);
    if (magic == 0x4d4d)		/* "MM" */
    {
      read_crw(fd);

#ifdef WANT_PGM_FILE
      exten(fname, argv[arg],".pgm");	/* Write the pixel data to */
      fd = open(fname,WFLAGS,0644);	/* a PGM file */
      if (fd < 0)
      { perror(fname);
	continue; }
      write(fd,"P5\n854 613\n255\n",15);
      write(fd, pgm, 613*854);
      close(fd);
#endif
    }
    else if (magic==0x3550 || magic==0x5035)	/* "P5" or "5P" */
    {
      if (read_pgm(fd)) continue;
    }

    exten(fname, argv[arg],".ppm");	/* Open a PPM file for RGB output */
    write_ppm(fname);
  }
}
