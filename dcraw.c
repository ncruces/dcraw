/*
   CRW to PPM converter Version 0.3

   by Dave Coffin (dcoffin at shore dot net)  3/24/97

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
  if (memcmp(data+4,"HEAPCCDR",8))		/* Make sure it's CRW */
    return 1;

  for (irow=orow=0; irow < 613; irow++)
  {
    read (in, data, 1120);                      /* Read one row */
    for (icol=ocol=0; icol < 1067; icol++)
    {
      if ( icol % 10 != 1 && icol % 10 != 9 ) /* Delete the "mystery pixels" */
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
    { -2.400719,  3.539540, -2.515721,  3.421035 }, /* red */
#if 0
    {  4.013642, -1.710916,  0.690795,  0.417247 }, /* green */
#endif
    {  2.006821, -0.855458,  0.345397,  0.208623 }, /* green/2 (why??) */
    { -2.345669,  3.385090,  3.521597, -2.249256 }  /* blue */
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

/* This moire smoothing doesn't work very well. */

#ifdef MOIRE_SMOOTH
    for (c=0; c < 3; c++)
    {
      prev = ppm[0][c];
      for (x=1; x < 852; x++)
      {
	val = prev + ppm[x][c]*2 + ppm[x+1][c] + 1 >> 2;
	prev = ppm[x][c];
	ppm[x][c] = val;
      }
    }
#endif

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
    puts("Version 0.3, last modified 3/24/97\n");
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
      if (read_crw(fd))
      { fprintf(stderr,"%s is not a CRW file.\n",argv[arg]);
	continue;
      }

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
    else if (ntohs(magic)==0x5035)	/* "P5" */
    {
      if (read_pgm(fd)) continue;
    }

    exten(fname, argv[arg],".ppm");	/* Open a PPM file for RGB output */
    write_ppm(fname);
  }
}
