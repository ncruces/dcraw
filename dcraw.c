/*
   CRW to PPM converter Version 0.1

   by Dave Coffin (dcoffin@shore.net)  2/22/97

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
   Write the raw pixel values to a PGM file before RGB conversion.
   Useful for debugging.
*/
#define WANT_PGM_FILE

/* Subtract this number from all pixels after writing PGM file. */
#define BLACK 6

/* Matrix used for solving equations */
float mat[3][4];

/* DOS likes to trash binary files!! */
#ifndef O_BINARY
#define O_BINARY 0
#endif

#define WFLAGS O_WRONLY | O_CREAT | O_TRUNC | O_BINARY

/*
     This table describes the color response of the four
  pixel types, according to experiment.  I have no idea
  why some terms are negative.  Perhaps Canon would like
  to fill in the correct values?

     An intuitive, but incorrect, table would be:

	red	green	blue
	 0	 1	 0	green
	 1	 0	 1	magenta
	 0	 1	 1	cyan
	 1	 1	 0	yellow
*/
float coeff[4][3] =
#if 0
{
/*  red *   + green *  + blue *     =   pixel color */
  { 0.051146, 0.319806, -0.067939 },	/* green */
  { 0.219660, 0.045533,  0.048611 },	/* magenta */
  {-0.045775, 0.337888,  0.057842 },	/* cyan */
  { 0.184652, 0.391803, -0.127791 },	/* yellow */
};
#endif

{
  { 0.052296, 0.255409,-0.047466 },
  { 0.156993, 0.046792, 0.046537 },
  {-0.004344, 0.280239, 0.045865 },
  { 0.145482, 0.309211,-0.089371 },
};

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
     These three functions solve a 4x3 matrix (three equations,
  three unknowns), leaving the answers in the last column.  To
  improve performance, this should be inlined or written in
  assembly.
*/
norm(row)
{
  float div;
  int i;

  div=mat[row][row];
  for (i=row+1; i < 4; i++)
    mat[row][i] /= div;
}

sub(arow,brow)
{
  float mul;
  int i;

  mul=mat[arow][brow];
  for (i=brow+1; i < 4; i++)
    mat[arow][i] -= mat[brow][i] * mul;
}

solve()
{
  norm(0);
  sub(1,0);
  sub(2,0);
  norm(1);
  sub(0,1);
  sub(2,1);
  norm(2);
  sub(0,2);
  sub(1,2);
}

/* Add one pixel to the equation */
add_pixel (int row, unsigned x, unsigned y)
{
  int color, value, i;

  if (x >= 854 || y >= 613) return;	/* Don't count this pixel */
  color = ((y & 1) << 1) + (x & 1);
  if ((y & 3) == 2)		/* 0=green, 1=magenta, 2=cyan, 3=yellow */
    color ^= 1;			/* Swap green and magenta */
  value = pgm[y][x] - BLACK;
#if BLACK
  if (value < 0) value=0;
#endif
  for (i=0; i < 3; i++)
    mat[row][i] += coeff[color][i];
  mat[row][3] += value;
}

write_ppm(char *fname)
{
  int out, y, x, c, val;
  uchar ppm[854*3], *pptr, max[4];
  float scale=1;

  out = open(fname,WFLAGS,0644);
  if (out < 0)
  { perror(fname);
    return; }
  write(out,"P6\n854 613\n255\n",15);

#if 0
  memset(max,0,4);
  for (y=0; y < 613; y++)
    for (x=0; x < 854; x++)
    {
      c = ((y & 1) << 1) + (x & 1);
      if ((y & 3) == 2)		/* 0=green, 1=magenta, 2=cyan, 3=yellow */
	c ^= 1;			/* Swap green and magenta */
      if (max[c] < pgm[y][x])
	max[c] = pgm[y][x];
    }
#endif

  for (y=0; y < 613; y++)
  {
    pptr=ppm;
    for (x=0; x < 854; x++)
    {
      memset(mat,0,sizeof mat);	/* Zero out the matrix */
      add_pixel(0,x,y);		/* Equation 0 uses this pixel */
      add_pixel(1,x-1,y);	/* Equation 1 uses pixels left and right */
      add_pixel(1,x+1,y);
      add_pixel(2,x,y-1);	/* Equation 2 uses pixels above and below */
      add_pixel(2,x,y+1);
      solve();			/* Solve for red, green, and blue */
      for (c=0; c < 3; c++)	/* Scale and save these values */
      {
	val = floor(mat[c][3] * scale);
	if (val < 0) val=0;
	if (val > 255) val=255;
	*pptr++ = val;
      }
    }
    write (out, ppm, 854*3);
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
    puts("Version 0.1, last modified 2/22/97\n");
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
