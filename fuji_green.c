/*
   fuji_green -- read Fuji green pixels

   $Revision: 1.1 $
   $Date: 2005/01/19 22:27:43 $
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
char *ifname, make[64], model[64];
int data_offset;
int height, width, trim;
ushort *image;
void (*load_raw)();
float bright=1.0;
int verbose=0;
void write_ppm(FILE *);
void (*write_fun)(FILE *) = write_ppm;
jmp_buf failure;

#define CLASS

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

/*
   Faster than calling fget2() multiple times.
 */
void CLASS read_shorts (ushort *pixel, int count)
{
  fread (pixel, 2, count, ifp);
  if ((order == 0x4949) == (ntohs(0x1234) == 0x1234))
    swab (pixel, pixel, count*2);
}

void CLASS fuji_s2_load_raw()
{
  ushort pixel[2944];
  int row, col;

  fseek (ifp, (2944*24+32)*2, SEEK_CUR);
  for (col=width; col--; ) {
    read_shorts (pixel, 2944);
    for (row=0; row < height; row++)
      image[row*width+col] = pixel[row*2+1];
  }
}

void CLASS fuji_s3_load_raw()
{
  ushort pixel[4352];
  int row, col;

  for (row=0; row < height; row++) {
    read_shorts (pixel, width*2);
    for (col=0; col < width; col++)
      image[row*width+col] = pixel[col*2+1];
  }
}

void CLASS fuji_load_raw()
{
  ushort pixel[2944];
  int row, col;

  for (row=0; row < height; row++) {
    read_shorts (pixel, width);
    read_shorts (pixel, width);
    for (col=0; col < width; col++) {
      image[row*width+col] = pixel[col];
    }
  }
}

/*
   Parse a TIFF file looking for camera model and decompress offsets.
 */
void CLASS parse_tiff (int base)
{
  int doff, entries, tag, type, len, val, save;

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
      fseek (ifp, base+val, SEEK_SET);
      switch (tag) {
	case 0x10f:			/* Make tag */
	  fgets (make, 64, ifp);
	  break;
	case 0x110:			/* Model tag */
	  fgets (model, 64, ifp);
      }
      fseek (ifp, save, SEEK_SET);
    }
  }
}

int fgetint (int offset)
{
  fseek (ifp, offset, SEEK_SET);
  fread (&offset, 4, 1, ifp);
  return ntohl(offset);
}

/*
   Identify which camera created this file, and set global variables
   accordingly.  Return nonzero if the file cannot be decoded.
 */
int CLASS identify()
{
  char head[32], *c;
  unsigned i;

/*  What format is this file?  Set make[] if we recognize it. */

  make[0] = model[0] = 0;
  data_offset = 0;

  fread (head, 1, 32, ifp);
  if (!memcmp (head, "FUJIFILM", 8)) {
    parse_tiff (fgetint(84)+12);
    data_offset = fgetint(100);
  }
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
  height = width = 0;
  load_raw = fuji_load_raw;

  if (!strcmp(model,"FinePixS2Pro")) {
    height = 2880;
    width = 2144;
    load_raw = fuji_s2_load_raw;
  } else if (!strcmp(model,"FinePix S3Pro")) {
    fseek (ifp, 0, SEEK_END);
    height = (ftell(ifp) - data_offset) / 4352;
    width  = 4352/2;
    load_raw = fuji_s3_load_raw;
  } else if (!strcmp(model,"FinePix S5000")) {
    height = 2156;
    width  = 1472;
  } else if (!strcmp(model,"FinePix S7000") ||
	     !strcmp(model,"FinePix E550") ||
	     !strcmp(model,"FinePix F810")) {
    height = 3080;
    width  = 2048;
  } else if (!strcmp(model,"FinePix F700") ||
	     !strcmp(model,"FinePix S20Pro")) {
    height = 2168;
    width  = 2944;
  }
  if (!height) {
    fprintf (stderr, "%s: %s %s is not supported.\n",
	ifname, make, model);
    return 1;
  }
  height /= 2;
  fseek (ifp, data_offset, SEEK_SET);
  return 0;
}

void CLASS write_ppm (FILE *ofp)
{
  int row, col, i, val, total, histogram[0x2000];
  float white, r;
  uchar lut[0x10000];

/*  Set the white point to the 99th percentile  */
  memset (histogram, 0, sizeof histogram);
  for (row = trim; row < height-trim; row++)
    for (col = trim; col < width-trim; col++)
      histogram[image[row*width+col] >> 4]++;
  i = width * height * 0.01;
  for (val=0x2000, total=0; --val; )
    if ((total += histogram[val]) > i) break;
  white = (val << 4) / bright;

  for (i=0; i < 0x10000; i++) {
    r = i / white;
    val = (r <= 0.018 ? r*4.5 : pow(r,0.45)*1.099-0.099) * 256;
    if (val > 255) val = 255;
    lut[i] = val;
  }
  fprintf (ofp, "P5\n%d %d\n255\n",
	width-trim*2, height-trim*2);
  for (row=trim; row < height-trim; row++)
    for (col=trim; col < width-trim; col++)
      fputc (lut[image[row*width+col]], ofp);
}

void CLASS write_psd (FILE *ofp)
{
  char head[] = {
    '8','B','P','S',		/* signature */
    0,1,0,0,0,0,0,0,		/* version and reserved */
    0,1,			/* number of channels */
    0,0,0,0,			/* height, big-endian */
    0,0,0,0,			/* width, big-endian */
    0,16,			/* 16-bit color */
    0,1,			/* mode (1=grey, 3=rgb) */
    0,0,0,0,			/* color mode data */
    0,0,0,0,			/* image resources */
    0,0,0,0,			/* layer/mask info */
    0,0				/* no compression */
  };
  int hw[2], psize, row, col, val;
  ushort *buffer, *pred;

  hw[0] = htonl(height-trim*2);	/* write the header */
  hw[1] = htonl(width-trim*2);
  memcpy (head+14, hw, sizeof hw);
  fwrite (head, 40, 1, ofp);

  psize = (height-trim*2) * (width-trim*2);
  buffer = calloc (2, psize);
  merror (buffer, "write_psd()");
  pred = buffer;

  for (row = trim; row < height-trim; row++) {
    for (col = trim; col < width-trim; col++) {
      val = image[row*width+col] * bright;
      if (val > 0xffff)
	  val = 0xffff;
      *pred++ = htons(val);
    }
  }
  fwrite (buffer, psize, 2, ofp);
  free (buffer);
}

void CLASS write_ppm16 (FILE *ofp)
{
  int row, col, val;

  fprintf (ofp, "P5\n%d %d\n%d\n",
	width-trim*2, height-trim*2, 65535);

  for (row = trim; row < height-trim; row++) {
    for (col = trim; col < width-trim; col++) {
      val = image[row*width+col] * bright;
      if (val > 0xffff)
	  val = 0xffff;
      fputc (val >> 8, ofp);
      fputc (val & 255, ofp);
    }
  }
}

int CLASS main (int argc, char **argv)
{
  int arg, status=0;
  int identify_only=0, write_to_stdout=0;
  char opt, *ofname, *cp;
  const char *write_ext = ".pgm";
  FILE *ofp = stdout;

  if (argc == 1) {
    fprintf (stderr,
    "\nFuji Green channel output"
    "\nby Dave Coffin, dcoffin a cybercom o net"
    "\n\nUsage:  %s [options] file1 file2 ...\n"
    "\nValid options:"
    "\n-i        Identify files but don't decode them"
    "\n-c        Write to standard output"
    "\n-v        Print verbose messages while decoding"
    "\n-b <num>  Set brightness (1.0 by default)"
    "\n-2        Write  8-bit PGM (default)"
    "\n-3        Write 16-bit PSD (Adobe Photoshop)"
    "\n-4        Write 16-bit PGM"
    "\n\n", argv[0]);
    return 1;
  }
  argv[argc] = "";
  for (arg=1; argv[arg][0] == '-'; ) {
    opt = argv[arg++][1];
    if (strchr ("b", opt) && !isdigit(argv[arg][0])) {
      fprintf (stderr, "\"-%c\" requires a numeric argument.\n", opt);
      return 1;
    }
    switch (opt) {
      case 'b':  bright      = atof(argv[arg++]);  break;

      case 'i':  identify_only     = 1;  break;
      case 'c':  write_to_stdout   = 1;  break;
      case 'v':  verbose           = 1;  break;

      case '2':  write_fun = write_ppm;   write_ext = ".pgm";  break;
      case '3':  write_fun = write_psd;   write_ext = ".psd";  break;
      case '4':  write_fun = write_ppm16; write_ext = ".pgm";  break;

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
      perror("setmode()");
      return 1;
    }
#endif
  }
  for ( ; arg < argc; arg++) {
    status = 1;
    image = NULL;
    if (setjmp (failure)) {
      if (fileno(ifp) > 2) fclose (ifp);
      if (fileno(ofp) > 2) fclose (ofp);
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
      fclose (ifp);
      continue;
    }
    if (identify_only) {
      fprintf (stderr, "%s is a %s %s image.\n", ifname, make, model);
      fclose (ifp);
      continue;
    }
    image = calloc (height * width, sizeof *image);
    merror (image, "main()");
    if (verbose)
      fprintf (stderr,
	"Loading %s %s image from %s...\n", make, model, ifname);
    (*load_raw)();
    fclose (ifp);
    trim = 0;
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
      fclose (ofp);
cleanup:
    free (ofname);
    free (image);
  }
  return status;
}
