/*
   Raw Photo Parser
   Copyright 2002 by Dave Coffin <dcoffin@shore.net>

   This program extracts thumbnail images (preferably JPEGs)
   from any raw digital camera formats that have them, and
   shows table contents.

   $Revision: 1.5 $
   $Date: 2002/12/20 22:33:58 $
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

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
char make[128], model[128];
int thumb_offset, thumb_len;

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
  printf("%*stag=0x%x, type=%d, count=%d, data=", level*2, "", tag, type, count);
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

void tiff_parse_subifd(int base)
{
  int entries, tag, type, count, save;

  entries = fget2(ifp);
  while (entries--) {
    save = ftell(ifp);
    tag  = fget2(ifp);
    type = fget2(ifp);
    count= fget4(ifp);
    tiff_dump (base, tag, type, count, 1);
    fseek (ifp, save+12, SEEK_SET);
  }
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
    if (tag == 0x927c && !strncmp(make,"NIKON",5))
      nef_parse_makernote();
    fseek (ifp, save+12, SEEK_SET);
  }
}

/*
   Parse a TIFF file looking for camera model and decompress offsets.
 */
void parse_tiff(int base)
{
  int doff, entries, tag, type, count, save, save2, i;

  fseek (ifp, base, SEEK_SET);
  order = fget2(ifp);
  fget2(ifp);			/* Should be 42 for standard TIFF */
  while ((doff = fget4(ifp))) {
    fseek (ifp, doff+base, SEEK_SET);
    entries = fget2(ifp);
    while (entries--) {
      save = ftell(ifp);
      tag  = fget2(ifp);
      type = fget2(ifp);
      count= fget4(ifp);

      tiff_dump (base, tag, type, count, 0);

      switch (tag) {
	case 271:			/* Make tag */
	  fread (make, 1, 128, ifp);
	  if (count > 127) count=127;
	  make[count]=0;
	  break;
	case 272:			/* Model tag */
	  fread (model, 1, 128, ifp);
	  if (count > 127) count=127;
	  model[count]=0;
	  break;
	case 330:			/* SubIFD tag */
          save2 = ftell(ifp);
	  for (i=0; i < count; i++) {
	    printf ("SubIFD #%d:\n", i+1);
	    fseek (ifp, save2 + i*4, SEEK_SET);
	    fseek (ifp, fget4(ifp)+base, SEEK_SET);
	    tiff_parse_subifd(base);
	  }
	  break;
	case 0x8769:
	  fseek (ifp, fget4(ifp)+base, SEEK_SET);
	  nef_parse_exif(base);
	  break;
      }
      fseek (ifp, save+12, SEEK_SET);
    }
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
      thumb_len = len;
    }
  }
}

/*
   Open a CRW file, identify which camera created it, and set
   global variables accordingly.  Returns nonzero if an error occurs.
 */
int open_and_id(char *fname)
{
  unsigned magic, hlen;
  char head[8], thumb_name[256], *thumb;
  FILE *tfp;

  ifp = fopen(fname,"rb");
  if (!ifp) {
    perror(fname);
    return 1;
  }
  model[0] = thumb_offset = thumb_len = 0;
  order = fget2(ifp);
  if (order == 0x4949 || order == 0x4d4d) {
    hlen = fget4(ifp);
    fread (head, 1, 8, ifp);
    if (!memcmp(head,"HEAPCCDR",8)) {
      fseek (ifp, 0, SEEK_END);
      parse_ciff (hlen, ftell(ifp) - hlen, 0);
      fseek (ifp, hlen, SEEK_SET);
    } else
      parse_tiff(0);
  } else {
    fseek (ifp, 0, SEEK_SET);
    magic = fget4(ifp);
    if (magic == 0x46554a49)		/* "FUJI" */
      parse_tiff(120);
    else if (magic == 0x4d524d) {	/* "\0MRM" */
      parse_tiff(48);
    }
  }
  if (model[0] == 0) {
    printf ("%s: unsupported file format.\n", fname);
    return 1;
  }
  printf ("Findings for %s:\n", fname);
  printf ("Make  is \"%s\"\n", make);
  printf ("Model is \"%s\"\n", model);

  if (!thumb_len) {
    printf ("Thumbnail image not found\n");
    return 0;
  }
  strcpy (thumb_name, fname);
  strcat (thumb_name, ".thumb");
  tfp = fopen (thumb_name, "wb");
  if (!tfp) {
    perror(thumb_name);
    exit(1);
  }
  thumb = (char *) malloc(thumb_len);
  if (!thumb) {
    fprintf (stderr, "Cannot allocate %d bytes!!\n", thumb_len);
    exit(1);
  }
  fseek (ifp, thumb_offset, SEEK_SET);
  fread (thumb, 1, thumb_len, ifp);
  fwrite(thumb, 1, thumb_len, tfp);
  free (thumb);
  fclose (tfp);
  printf ("Thumbnail image written to %s.\n", thumb_name);
  return 0;
}

int main(int argc, char **argv)
{
  int arg;

  if (argc == 1)
  {
    fprintf (stderr,
    "\nRaw Photo Thumbnail Extracter"
    "\nby Dave Coffin (dcoffin@shore.net)"
    "\n\nUsage:  %s [options] file1.crw file2.crw ...\n", argv[0]);
    exit(1);
  }

  for (arg=1; arg < argc; arg++)
  {
    printf("\nParsing %s:\n",argv[arg]);
    open_and_id(argv[arg]);
    if (ifp) fclose(ifp);
  }
  return 0;
}
