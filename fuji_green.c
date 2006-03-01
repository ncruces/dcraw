/*
   fuji_green -- read Fuji green pixels

   $Revision: 1.2 $
   $Date: 2006/03/01 01:46:47 $
 */

#include <ctype.h>
#include <math.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ushort UshORt
typedef unsigned char uchar;
typedef unsigned short ushort;

FILE *ifp;
short order;
char *ifname, make[64], model[64];
int data_offset, raw_height, raw_width, height, width;
int fuji_layout, fuji_secondary, use_secondary=0, verbose=0;
ushort *image;
void (*load_raw)();
float bright=1.0;
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

ushort CLASS get2()
{
  uchar a, b;

  a = fgetc(ifp);
  b = fgetc(ifp);
  if (order == 0x4949)		/* "II" means little-endian */
    return a + (b << 8);
  else				/* "MM" means big-endian */
    return (a << 8) + b;
}

int CLASS get4()
{
  uchar a, b, c, d;

  a = fgetc(ifp);
  b = fgetc(ifp);
  c = fgetc(ifp);
  d = fgetc(ifp);
  if (order == 0x4949)
    return a + (b << 8) + (c << 16) + (d << 24);
  else
    return (a << 24) + (b << 16) + (c << 8) + d;
}

/*
   Faster than calling get2() multiple times.
 */
void CLASS read_shorts (ushort *pixel, int count)
{
  fread (pixel, 2, count, ifp);
  if ((order == 0x4949) == (ntohs(0x1234) == 0x1234))
    swab (pixel, pixel, count*2);
}

void CLASS fuji_load_raw()
{
  ushort *pixel, *img;
  int row, col;

  pixel = calloc (raw_width, 2);
  merror (pixel, "fuji_load_raw()");
  for (row=0; row < height; row++)
    if (fuji_layout) {
      read_shorts (image+row*width, width);
      fseek (ifp, (raw_width*2 - width)*2, SEEK_CUR);
    } else {
      read_shorts (pixel, raw_width);
      for (img=image+row*width, col=0; col < width; col++)
	img[col] = pixel[col*2+1];
    }
  free (pixel);
}

void CLASS parse_fuji (int offset)
{
  unsigned entries, tag, len, save;

  fseek (ifp, offset, SEEK_SET);
  entries = get4();
  if (entries > 255) return;
  while (entries--) {
    tag = get2();
    len = get2();
    save = ftell(ifp);
    if (tag == 0x100) {
      raw_height = get2();
      raw_width  = get2();
    } else if (tag == 0x121) {
      height = get2();
      width  = get2();
    } else if (tag == 0x130)
      fuji_layout = fgetc(ifp) >> 7;
    fseek (ifp, save+len, SEEK_SET);
  }
  if (fuji_layout) {
    height *= 2;
    width  /= 2;
  }
}

void CLASS parse_tiff (int base)
{
  int doff, entries, tag, type, len, save;

  fseek (ifp, base, SEEK_SET);
  order = get2();
  get2();				/* Should be 42 for standard TIFF */
  while ((doff = get4())) {
    fseek (ifp, doff+base, SEEK_SET);
    entries = get2();
    while (entries--) {
      tag  = get2();
      type = get2();
      len  = get4();
      save = ftell(ifp)+4;
      fseek (ifp, base+get4(), SEEK_SET);
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

int CLASS identify()
{
  char head[32], *c;
  int thumb_offset;

  make[0] = model[0] = 0;
  data_offset = raw_height = raw_width = height = width = 0;
  fuji_secondary = 0;

  order = 0x4d4d;
  fread (head, 1, 32, ifp);
  if (memcmp (head, "FUJIFILM", 8)) return 1;
  fseek (ifp, 84, SEEK_SET);
  thumb_offset = get4();
  fseek (ifp, 92, SEEK_SET);
  parse_fuji (get4());
  if (thumb_offset > 120) {
    fseek (ifp, 120, SEEK_SET);
    fuji_secondary = get4() && 1;
  }
  fseek (ifp, 100, SEEK_SET);
  data_offset = get4();
  parse_tiff (thumb_offset+12);
  c = model + strlen(model);		/* Remove trailing spaces */
  while (*--c == ' ') *c = 0;
  if (!strcmp(model,"FinePix S5100") ||
      !strcmp(model,"FinePix S5500")) return 1;
  load_raw = fuji_load_raw;
  if (!strcmp(model+7,"S2Pro")) {
    strcpy (model+7," S2Pro");
    height = 2144;
    width  = 2880;
  }
  data_offset += (raw_height - height + 1)*raw_width - width;
  if (fuji_secondary)
    data_offset += use_secondary * ( strcmp(model+7," S3Pro")
		? (raw_width *= 2) : raw_height*raw_width*2 );
  data_offset += fuji_layout*raw_width*2;
  width >>= !fuji_layout;
  height >>= fuji_layout;
  fseek (ifp, data_offset, SEEK_SET);
  return 0;
}

void CLASS write_ppm (FILE *ofp)
{
  int i, size, val, total, histogram[0x2000];
  float white, r;
  uchar lut[0x10000];

  memset (histogram, 0, sizeof histogram);
  size = width * height;
  for (i = 0; i < size; i++)
    histogram[image[i] >> 4]++;
  i = size * 0.01;			/* 99th percentile white point */
  for (val=0x2000, total=0; --val; )
    if ((total += histogram[val]) > i) break;
  white = (val << 4) / bright;

  for (i=0; i < 0x10000; i++) {
    r = i / white;
    val = (r <= 0.018 ? r*4.5 : pow(r,0.45)*1.099-0.099) * 256;
    if (val > 255) val = 255;
    lut[i] = val;
  }
  fprintf (ofp, "P5\n%d %d\n255\n", width, height);
  for (i=0; i < size; i++)
    fputc (lut[image[i]], ofp);
}

void CLASS write_raw16 (FILE *ofp)
{
  if (ntohs(0x1234) != 0x1234)
    swab (image, image, width*height*2);
  fwrite (image, width*height, 2, ofp);
}

void CLASS write_ppm16 (FILE *ofp)
{
  fprintf (ofp, "P5\n%d %d\n%d\n", width, height, 65535);
  write_raw16 (ofp);
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
    0,0 };			/* no compression */
  int hw[2];

  hw[0] = htonl(height*2);	/* write the header */
  hw[1] = htonl(width*2);
  memcpy (head+14, hw, sizeof hw);
  fwrite (head, 40, 1, ofp);
  write_raw16 (ofp);
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
    "\n-v        Print verbose messages"
    "\n-c        Write image data to standard output"
    "\n-i        Identify files without decoding them"
    "\n-s        Use secondary pixels if available"
    "\n-b <num>  Set brightness (default = 1.0)"
    "\n-2        Write  8-bit non-linear PGM (default)"
    "\n-4        Write 16-bit linear PGM"
    "\n-3        Write 16-bit linear PSD (Adobe Photoshop)"
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
      case 'v':  verbose           = 1;  break;
      case 'i':  identify_only     = 1;  break;
      case 'c':  write_to_stdout   = 1;  break;
      case 's':  use_secondary     = 1;  break;
      case 'b':  bright = atof(argv[arg++]);  break;
      case '2':  write_fun = write_ppm;    break;
      case '4':  write_fun = write_ppm16;  break;
      case '3':  write_fun = write_psd;  write_ext = ".psd";  break;
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
      fprintf (stderr, "%s: unsupported file format.\n", ifname);
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
