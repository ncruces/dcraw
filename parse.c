/*
   Raw Photo Parser
   Copyright 2004 by Dave Coffin, dcoffin a cybercom o net

   This program extracts thumbnail images (preferably JPEGs)
   from any raw digital camera formats that have them, and
   shows table contents.

   $Revision: 1.12 $
   $Date: 2004/02/20 04:22:08 $
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#ifdef WIN32
#include <winsock2.h>
typedef __int64 INT64;
#else
#include <netinet/in.h>
typedef long long INT64;
#endif

/*
   TIFF and CIFF data blocks can be quite large.
   Display only the first DLEN bytes.
 */
#ifndef DLEN
#define DLEN 768
#endif

typedef unsigned char uchar;
//typedef unsigned short ushort;

FILE *ifp;
short order;
char make[128], model[128], model2[128], thumb_head[128];
int width, height, offset, bps;
int thumb_offset, thumb_length, thumb_layers;

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

void tiff_dump(int base, int tag, int type, int count, int level)
{
  int save, j, num, den;
  uchar c;
  int size[] = { 1,1,1,2,4,8,1,1,2,4,8,4,8 };

  if (count * size[type < 13 ? type:0] > 4)
    fseek (ifp, fget4(ifp)+base, SEEK_SET);
  save = ftell(ifp);
  printf("%*stag=0x%x, type=%d, count=%d, data=",
	level*2, "", tag, type, count);
  if (type==2) putchar('\"');
  for (j = 0; j < count && j < DLEN; j++)
    switch (type) {
      case 1: case 6: case 7:		/* byte values */
	printf ("%c%02x",(j & 31) || count < 17 ? ' ':'\n', fgetc(ifp) & 0xff);
	break;
      case 2:				/* null-terminated ASCII strings */
	c = fgetc(ifp);
	putchar(isprint(c) ? c:'.');
	break;
      case 3: case 8:			/* word values */
	printf ("%c%04x",(j & 15) || count < 9 ? ' ':'\n', fget2(ifp));
	break;
      case 4: case 9:			/* dword values */
	printf ("%c%08x",(j & 7) || count < 5 ? ' ':'\n', fget4(ifp));
	break;
      case 5: case 10:			/* rationals */
	num = fget4(ifp);
	den = fget4(ifp);
	printf (" %d/%d", num, den);
	break;
    }
  if (type==2) putchar('\"');
  putchar('\n');
  fseek (ifp, save, SEEK_SET);
}

void nef_parse_makernote()
{
  int base=0, offset=0, entries, tag, type, count, val, save;
  short sorder;
  char buf[10];

  puts("  Nikon MakerNote:");
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
    save = ftell(ifp);
    tag  = fget2(ifp);
    type = fget2(ifp);
    count= fget4(ifp);
    tiff_dump (base, tag, type, count, 2);
    fseek (ifp, save+12, SEEK_SET);
  }
  order = sorder;
}

void mrw_parse_makernote(int base)
{
  int entries, tag, type, count, val;

  entries = fget2(ifp);
  while (entries--) {
    tag  = fget2(ifp);
    type = fget2(ifp);
    count= fget4(ifp);
    val  = fget4(ifp);
    if (tag == 0x81 || tag == 0x88)
      thumb_offset = base + val;
  }
}

void nef_parse_exif(int base)
{
  int entries, tag, type, count, save;

  puts("Nikon EXIF tag:");
  entries = fget2(ifp);
  while (entries--) {
    save = ftell(ifp);
    tag  = fget2(ifp);
    type = fget2(ifp);
    count= fget4(ifp);
    tiff_dump (base, tag, type, count, 1);
    if (tag == 0x927c)
      if (!strncmp(make,"NIKON",5))
	nef_parse_makernote();
      else if (strstr(make,"Minolta"))
	mrw_parse_makernote(base);
    fseek (ifp, save+12, SEEK_SET);
  }
}

void parse_tiff (int base, int level)
{
  int entries, tag, type, count, slen, save, save2, val, i;

  entries = fget2(ifp);
  while (entries--) {
    save = ftell(ifp);
    tag  = fget2(ifp);
    type = fget2(ifp);
    count= fget4(ifp);
    slen = count;
    if (slen > 128) slen = 128;

    tiff_dump (base, tag, type, count, level);

    save2 = ftell(ifp);
    if (type == 3)			/* short int */
      val = fget2(ifp);
    else
      val = fget4(ifp);
    fseek (ifp, save2, SEEK_SET);

    switch (tag) {
      case 0x100:			/* ImageWidth */
	if (!width)  width = val;
	break;
      case 0x101:			/* ImageHeight */
	if (!height) height = val;
	break;
      case 0x102:			/* Bits per sample */
	if (bps) break;
	bps = val;
	if (count == 1)
	  thumb_layers = 1;
	break;
      case 0x103:			/* Compression */
	break;
      case 0x10f:			/* Make tag */
	fgets (make, slen, ifp);
	break;
      case 0x110:			/* Model tag */
	fgets (model, slen, ifp);
	break;
      case 0x827d:			/* Model2 tag */
	fgets (model2, slen, ifp);
	break;
      case 0x111:			/* StripOffset */
	if (!offset) offset = val;
	break;
      case 0x117:			/* StripByteCounts */
	if (offset > val && !strncmp(make,"KODAK",5))
	  offset -= val;
	break;
      case 0x14a:			/* SubIFD tag */
	save2 = ftell(ifp);
	for (i=0; i < count; i++) {
	  printf ("SubIFD #%d:\n", i+1);
	  fseek (ifp, save2 + i*4, SEEK_SET);
	  fseek (ifp, fget4(ifp)+base, SEEK_SET);
	  parse_tiff (base, level+1);
	}
	break;
      case 0x201:
	thumb_offset = val;
	break;
      case 0x202:
	thumb_length = val;
	break;
      case 0x8769:
	fseek (ifp, fget4(ifp)+base, SEEK_SET);
	nef_parse_exif (base);
	break;
    }
    fseek (ifp, save+12, SEEK_SET);
  }
}

/*
   Parse a TIFF file looking for camera model and decompress offsets.
 */
void parse_tiff_file (int base)
{
  int doff, spp=3;

  width = height = offset = bps = 0;
  fseek (ifp, base, SEEK_SET);
  order = fget2(ifp);
  fget2(ifp);			/* Should be 42 for standard TIFF */
  while ((doff = fget4(ifp))) {
    fseek (ifp, doff+base, SEEK_SET);
    parse_tiff (base, 0);
  }
  if (strncmp(make,"KODAK",5))
    thumb_layers = 0;
  if (!strncmp(model,"DCS460A",7)) {
    spp = 1;
    thumb_layers = 0;
  }
  if (!thumb_length) {
    thumb_offset = offset;
    sprintf (thumb_head, "P%d\n%d %d\n%d\n",
	spp > 1 ? 6:5, width, height, (1 << bps) - 1);
    thumb_length = width * height * spp * ((bps+7)/8);
  }
}

/*
   Parse the CIFF structure.
 */
void parse_ciff (int offset, int length, int level)
{
  int tboff, nrecs, i, j, type, len, dlen, roff, aoff=0, save;
  char c, name[256];

  fseek (ifp, offset+length-4, SEEK_SET);
  tboff = fget4(ifp) + offset;
  fseek (ifp, tboff, SEEK_SET);
  nrecs = fget2(ifp);
  printf ("%*s%d records:\n", level*2, "", nrecs);
  for (i = 0; i < nrecs; i++) {
    save = ftell(ifp);
    type = fget2(ifp);
    printf ("%*stype=0x%04x", level*2, "", type);
    if (type & 0x4000) {
      len = 8;
      type &= 0x3fff;
    } else {
      len  = fget4(ifp);
      roff = fget4(ifp);
      aoff = offset + roff;
      printf (", length=%d, reloff=%d, absoff=%d",
		len, roff, aoff);
      fseek (ifp, aoff, SEEK_SET);
    }
    if ((type & 0xe700) == 0)
      printf (", data=");
    if (type == 0x0032)			/* display as words */
	type |= 0x1000;
    dlen = len < DLEN ? len:DLEN;
    switch (type >> 8) {
      case 0x28:
      case 0x30:
	putchar('\n');
	parse_ciff (aoff, len, level+1);
	fseek (ifp, save+10, SEEK_SET);
	continue;
      case 0x00:			/* byte values */
	for (j = 0; j < dlen; j++)
	  printf ("%c%02x",(j & 31) || dlen < 16 ? ' ':'\n', fgetc(ifp) & 0xff);
	break;
      case 0x08:			/* null-terminated ASCII strings */
	putchar('\"');
	for (j = 0; j < dlen; j++) {
	  c = fgetc(ifp);
	  putchar( isprint(c) ? c:'.');
	}
	putchar('\"');
	break;
      case 0x10:			/* word values */
	for (j = 0; j < dlen; j+=2)
	  printf ("%c%5u",(j & 31) || dlen < 16 ? ' ':'\n', fget2(ifp));
	break;
      case 0x18:			/* dword values */
	for (j = 0; j < dlen; j+=4)
	  printf ("%c%08x",(j & 31) || dlen < 16 ? ' ':'\n', fget4(ifp));
    }
    putchar('\n');
    fseek (ifp, save+10, SEEK_SET);
    if (type == 0x080a) {		/* Get the camera name */
      fseek (ifp, aoff, SEEK_SET);
      fread (name, 256, 1, ifp);
      strcpy (make, name);
      strcpy (model, name + strlen(make)+1);
    }
    if (type == 0x2007) {		/* Found the JPEG thumbnail */
      thumb_offset = aoff;
      thumb_length = len;
    }
  }
}

void parse_rollei()
{
  char line[128], *val;

  fseek (ifp, 0, SEEK_SET);
  do {
    fgets (line, 128, ifp);
    fputs (line, stdout);
    if ((val = strchr(line,'=')))
      *val++ = 0;
    else
      val = line + strlen(line);
    if (!strcmp(line,"HDR"))
      thumb_offset = atoi(val);
    if (!strcmp(line,"TX "))
      width = atoi(val);
    if (!strcmp(line,"TY "))
      height = atoi(val);
  } while (strncmp(line,"EOHD",4));
  strcpy (make, "Rollei");
  strcpy (model, "d530flex");
  thumb_length = width*height*2;
}

void rollei_decode (FILE *tfp)
{
  ushort data;
  int row, col;

  fseek (ifp, thumb_offset, SEEK_SET);
  fprintf (tfp, "P6 %d %d 255\n", width, height);
  for (row=0; row < height; row++)
    for (col=0; col < width; col++) {
      fread (&data, 2, 1, ifp);
      data = ntohs(data);
      putc (data << 3, tfp);
      putc (data >> 5  << 2, tfp);
      putc (data >> 11 << 3, tfp);
    }
}

void parse_foveon()
{
  char *buf, *bp, *np;
  int off1, off2, len, i, wide, high;

  order = 0x4949;			/* Little-endian */
  fseek (ifp, -4, SEEK_END);
  off2 = fget4(ifp);

  fseek (ifp, off2, SEEK_SET);
  while (fget4(ifp) != 0x464d4143)	/* Search for "CAMF" */
    if (feof(ifp)) return;
  off1 = fget4(ifp);
  fseek (ifp, off1+8, SEEK_SET);
  off1 += (fget4(ifp)+3) * 8;
  len = (off2 - off1)/2;
  fseek (ifp, off1, SEEK_SET);
  buf = malloc(len);
  if (!buf) {
    perror("parse_foveon() malloc failed");
    exit(1);
  }
  for (i=0; i < len; i++)		/* Convert Unicode to ASCII */
    buf[i] = fget2(ifp);
  for (bp=buf; bp < buf+len && *bp; ) {
    np = bp + strlen(bp) + 1;
    printf ("%-16s%s\n", bp, np);
    if (!strcmp(bp,"CAMMANUF"))
      strcpy (make, np);
    if (!strcmp(bp,"CAMMODEL"))
      strcpy (model, np);
    bp = np + strlen(np) + 1;
  }
  free(buf);

  fseek (ifp, off2, SEEK_SET);
  while (fget4(ifp) != 0x47414d49)	/* Search for "IMAG" */
    if (feof(ifp)) return;
  fseek (ifp, fget4(ifp)+16, SEEK_SET);
  wide = fget4(ifp);			/* Should crop to this width */
  high = fget4(ifp);
  wide = fget4(ifp) / 3;		/* Includes one garbage column */
  sprintf (thumb_head, "P6 %d %d 255\n", wide, high);
  thumb_offset = ftell(ifp);
  thumb_length = wide * high * 3;
}

void kodak_yuv_decode (FILE *tfp)
{
  uchar c, blen[384];
  unsigned row, col, len, bits=0;
  INT64 bitbuf=0;
  int i, li=0, si, diff, six[6], y[4], cb=0, cr=0, rgb[3];
  ushort *out, *op;

  fseek (ifp, thumb_offset, SEEK_SET);
  width = (width+1) & -2;
  fprintf (tfp, "P6 %d %d 65535\n", width, height);
  out = malloc (width * 12);
  if (!out) {
    fprintf (stderr, "kodak_yuv_decode() malloc failed!\n");
    exit(1);
  }

  for (row=0; row < height; row+=2) {
    for (col=0; col < width; col+=2) {
      if ((col & 127) == 0) {
	len = (width - col + 1) * 3 & -4;
	if (len > 384) len = 384;
	for (i=0; i < len; ) {
	  c = fgetc(ifp);
	  blen[i++] = c & 15;
	  blen[i++] = c >> 4;
	}
	li = bitbuf = bits = y[1] = y[3] = cb = cr = 0;
      }
      for (si=0; si < 6; si++) {
	len = blen[li++];
	if (bits < len) {
	  for (i=0; i < 32; i+=8)
	    bitbuf += (INT64) fgetc(ifp) << (bits+(i^8));
	  bits += 32;
	}
	diff = bitbuf & (0xffff >> (16-len));
	bitbuf >>= len;
	bits -= len;
	if ((diff & (1 << (len-1))) == 0)
	  diff -= (1 << len) - 1;
	six[si] = diff;
      }
      y[0] = six[0] + y[1];
      y[1] = six[1] + y[0];
      y[2] = six[2] + y[3];
      y[3] = six[3] + y[2];
      cb  += six[4];
      cr  += six[5];
      for (i=0; i < 4; i++) {
	op = out + ((i >> 1)*width + col+(i & 1)) * 3;
	rgb[0] = y[i] + 1.40200/2 * cr;
	rgb[1] = y[i] - 0.34414/2 * cb - 0.71414/2 * cr;
	rgb[2] = y[i] + 1.77200/2 * cb;
	for (c=0; c < 3; c++)
	  if (rgb[c] > 0) op[c] = htons(rgb[c]);
      }
    }
    fwrite (out, sizeof *out, width*6, tfp);
  }
  free(out);
}

/*
   Identify which camera created this file, and set global variables
   accordingly.	 Return nonzero if the file cannot be decoded.
 */
int identify(char *fname)
{
  char head[32], thumb_name[256], *thumb, *rgb;
  unsigned hlen, fsize, toff, tlen, lsize, i;
  FILE *tfp;

  make[0] = model[0] = model2[0] = 0;
  thumb_head[0] = thumb_offset = thumb_length = thumb_layers = 0;
  order = fget2(ifp);
  hlen = fget4(ifp);
  fseek (ifp, 0, SEEK_SET);
  fread (head, 1, 32, ifp);
  fseek (ifp, 0, SEEK_END);
  fsize = ftell(ifp);
  if (!memcmp (head,"MMMMRawT",8)) {
  } else if (order == 0x4949 || order == 0x4d4d) {
    if (!memcmp(head+6,"HEAPCCDR",8)) {
      parse_ciff (hlen, fsize - hlen, 0);
      fseek (ifp, hlen, SEEK_SET);
    } else
      parse_tiff_file(0);
  } else if (!memcmp (head,"\0MRM",4)) {
    fseek (ifp, 4, SEEK_SET);
    thumb_length = fget4(ifp);
    parse_tiff_file(48);
    strcpy (thumb_head, "\xff");
    thumb_offset++;
    thumb_length -= thumb_offset;
  } else if (!memcmp (head,"FUJIFILM",8)) {
    fseek (ifp, 84, SEEK_SET);
    toff = fget4(ifp);
    tlen = fget4(ifp);
    parse_tiff_file (toff+12);
    thumb_offset = toff;
    thumb_length = tlen;
  } else if (!memcmp (head,"DSC-Image",9))
    parse_rollei();
  else if (!memcmp (head,"FOVb",4))
    parse_foveon();
  if (model[0] == 0) {
    fprintf (stderr, "%s: unsupported file format.\n", fname);
    return 1;
  }
  if (!strncmp(model,"C50",3)) {
    thumb_head[0] = 0;
    thumb_offset = 0x1000;
    thumb_length = 0x2c00;
  }
  if (!strncmp(model,"C80",3)) {
    thumb_head[0] = 0;
    thumb_offset = 0x1584;
    thumb_length = 0x2c00;
  }
  fprintf (stderr, "Findings for %s:\n", fname);
  fprintf (stderr, "Make   is \"%s\"\n", make);
  fprintf (stderr, "Model  is \"%s\"\n", model);
  if (model2[0])
    fprintf (stderr, "Model2 is \"%s\"\n", model2);

  if (!thumb_length) {
    fprintf (stderr, "Thumbnail image not found\n");
    return 0;
  }
  strcpy (thumb_name, fname);
  strcat (thumb_name, ".thumb");
  tfp = fopen (thumb_name, "wb");
  if (!tfp) {
    perror(thumb_name);
    exit(1);
  }
  if (!strcasecmp(model,"DCS Pro 14N")) {
    kodak_yuv_decode (tfp);
    goto done;
  }
  if (!strcmp(make,"Rollei")) {
    rollei_decode (tfp);
    goto done;
  }
  thumb = (char *) malloc(thumb_length);
  if (!thumb) {
    fprintf (stderr, "Cannot allocate %d bytes!!\n", thumb_length);
    exit(1);
  }
  fseek (ifp, thumb_offset, SEEK_SET);
  fread (thumb, 1, thumb_length, ifp);
  if (thumb_layers) {
    rgb = (char *) malloc(thumb_length);
    if (!rgb) {
      fprintf (stderr, "Cannot allocate %d bytes!!\n", thumb_length);
      exit(1);
    }
    lsize = thumb_length/3;
    for (i=0; i < thumb_length; i++)
      rgb[(i%lsize)*3 + i/lsize] = thumb[i];
    free(thumb);
    thumb = rgb;
  }
  fputs (thumb_head, tfp);
  fwrite(thumb, 1, thumb_length, tfp);
  free (thumb);
done:
  fclose (tfp);
  fprintf (stderr, "Thumbnail image written to %s.\n", thumb_name);
  return 0;
}

int main(int argc, char **argv)
{
  int arg;

  if (argc == 1)
  {
    fprintf (stderr,
    "\nRaw Photo Parser and Thumbnail Extracter"
    "\nby Dave Coffin, dcoffin a cybercom o net"
    "\n\nUsage:  %s [options] file1.crw file2.crw ...\n", argv[0]);
    exit(1);
  }

  for (arg=1; arg < argc; arg++)
  {
    ifp = fopen(argv[arg],"rb");
    if (!ifp) {
      perror(argv[arg]);
      continue;
    }
    printf ("\nParsing %s:\n", argv[arg]);
    identify (argv[arg]);
    fclose (ifp);
  }
  return 0;
}
