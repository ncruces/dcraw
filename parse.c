/*
   Simple ANSI C reference parser for all Canon digital cameras.
   by Dave Coffin  October 14, 2001

   No restrictions on this code -- use and distribute freely.

   $Revision: 1.4 $
   $Date: 2002/11/09 05:40:02 $
*/

#include <stdio.h>
#include <string.h>

typedef unsigned char uchar;

/* Global Variables */

FILE *ifp;
short order;
char jpgname[1024];

/*
   Get a 2-byte integer, making no assumptions about CPU byte order.
   Nor should we assume that the compiler evaluates left-to-right.
 */
short fget2 (FILE *f)
{
  register uchar a, b;

  a = fgetc(f);
  b = fgetc(f);
  if (order == 0x4d4d)		/* "MM" means big-endian */
    return (a << 8) + b;
  else				/* "II" means little-endian */
    return a + (b << 8);
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
  if (order == 0x4d4d)
    return (a << 24) + (b << 16) + (c << 8) + d;
  else
    return a + (b << 8) + (c << 16) + (d << 24);
}

/*
   Parse the CIFF structure.
 */
parse (int offset, int length, int level)
{
  int tboff, nrecs, i, j, type, len, roff, aoff, save;
  FILE *jfp;
  char name[64], *jpg;

  fseek (ifp, offset+length-4, SEEK_SET);
  tboff = fget4(ifp) + offset;
  fseek (ifp, tboff, SEEK_SET);
  nrecs = fget2(ifp);
  printf ("%*s%d records:\n",level*2,"",nrecs);
  for (i = 0; i < nrecs; i++) {
    type = fget2(ifp);
    printf ("%*stype = 0x%04x, ", level*2, "", type);
    if (type < 0x4000) {
      len  = fget4(ifp);
      roff = fget4(ifp);
      aoff = offset + roff;
      printf ("length =%8d, reloff =%8d, absoff = 0x%06x\n",
		len, roff, aoff);
    } else {
      printf ("data =");
      for (j = 0; j < 8; j++)
	printf (" %02x",fgetc(ifp));
      putchar('\n');
    }
    save = ftell(ifp);
    if (type == 0x080a) {		/* Get the camera name */
      fseek (ifp, aoff, SEEK_SET);
      while (fgetc(ifp));
      fread (name, 64, 1, ifp);
      printf ("Camera name is %s\n", name);
    }
    if (type == 0x1031) {		/* Get the width and height */
      fseek (ifp, aoff+2, SEEK_SET);
      printf ("Image width  is %d\n", fget2(ifp));
      printf ("Image height is %d\n", fget2(ifp));
    }
    if (type == 0x1835) {		/* Get the decoder table */
      fseek (ifp, aoff, SEEK_SET);
      printf ("Table value is %d\n", fget4(ifp));
    }
    if (type == 0x2007) {		/* Write the JPEG thumbnail */
      printf ("Writing JPEG thumbnail to %s\n", jpgname);
      if (jpg = (char *)malloc(len)) {
	if (jfp = fopen (jpgname, "wb")) {
	  fseek (ifp, aoff, SEEK_SET);
	  fread (jpg, len, 1, ifp);
	  fwrite(jpg, len, 1, jfp);
	  fclose(jfp);
	} else
	  perror (jpgname);
	free(jpg);
      } else
      printf ("Cannot malloc %d bytes!!\n", len);
    }
    if (type >> 8 == 0x28 || type >> 8 == 0x30)	/* Get sub-tables */
      parse (aoff, len, level+1);
    fseek (ifp, save, SEEK_SET);
  }
}

/*
   Creates a new filename with a different extension
 */
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
  char head[8];
  int arg, hlen;

  if (argc < 2) {
    fprintf(stderr,"Usage:  %s file1.crw file2.crw ...\n",argv[0]);
    exit(1);
  }
  for (arg=1 ; arg < argc; arg++)
  {
    ifp = fopen(argv[arg],"rb");
    if (!ifp) {
      perror(argv[arg]);
      exit(1);
    }
    printf("------------------- %s -------------------\n",argv[arg]);
    order = fget2(ifp);
    hlen  = fget4(ifp);
    fread (head, 1, 8, ifp);
    if (memcmp(head,"HEAPCCDR",8) || (order != 0x4949 && order != 0x4d4d)) {
      printf("%s is not a Canon CRW file.\n",argv[arg]);
      fclose(ifp);
      continue;
    }
    exten (jpgname, argv[arg], ".thumb.jpg");
    fseek (ifp, 0, SEEK_END);
    parse (hlen, ftell(ifp)-hlen, 1);
    fclose (ifp);
  }
}
