/*
   Simple ANSI C reference parser for all Canon digital cameras.
   by Dave Coffin  October 14, 2001

   No restrictions on this code -- use and distribute freely.

   $Revision: 1.1 $
   $Date: 2001/10/15 04:38:25 $
*/

#include <stdio.h>

/* Global Variables */

FILE *ifp;
short order;

/*
   Get a 2-byte integer, making no assumptions about CPU byte order.
 */
short fget2 (FILE *f)
{
  if (order == 0x4949)		/* "II" means little-endian */
    return fgetc(f) + (fgetc(f) << 8);
  else if (order == 0x4d4d)	/* "MM" means big-endian */
    return (fgetc(f) << 8) + fgetc(f);
  else {
    fprintf(stderr,"Unknown byte order!");
    exit(1);
  }
}

/*
   Same for a 4-byte integer.
 */
int fget4 (FILE *f)
{
  if (order == 0x4949)
    return fgetc(f) + (fgetc(f) << 8) + (fgetc(f) << 16) + (fgetc(f) << 24);
  else if (order == 0x4d4d)
    return (fgetc(f) << 24) + (fgetc(f) << 16) + (fgetc(f) << 8) + fgetc(f);
  else {
    fprintf(stderr,"Unknown byte order!");
    exit(1);
  }
}

parse (int offset, int length, int level)
{
  int save, toff, nrecs, i, j, type, len, off;
  char name[64];

  fseek (ifp, offset+length-4, SEEK_SET);
  toff = fget4(ifp) + offset;
  fseek (ifp, toff, SEEK_SET);
  nrecs = fget2(ifp);
  printf ("%*s%d records:\n",level*2,"",nrecs);
  for (i = 0; i < nrecs; i++) {
    type = fget2(ifp);
    printf ("%*stype = 0x%04x, ", level*2, "", type);
    if (type < 0x4000) {
      len  = fget4(ifp);
      off  = fget4(ifp);
      printf ("length =%8d, reloff =%8d, absoff = 0x%06x\n",
		len, off, offset+off);
    } else {
      printf ("data =");
      for (j = 0; j < 8; j++)
        printf (" %02x",fgetc(ifp));
      putchar('\n');
    }
    save = ftell(ifp);
    if (type >> 8 == 0x28 || type >> 8 == 0x30)
      parse (offset+off, len, level+1);
    if (type == 0x080a) {
      fseek (ifp, offset+off, SEEK_SET);
      fread (name, 64, 1, ifp);
      printf ("Camera name is %s\n", name+strlen(name)+1);
    }
    if (type == 0x1835) {
      fseek (ifp, offset+off, SEEK_SET);
      printf ("Table value is %d\n", fget4(ifp));
    }
    fseek (ifp, save, SEEK_SET);
  }
}

main(int argc, char **argv)
{
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
    fread (&order, 2, 1, ifp);
    hlen = fget4(ifp);

    fseek (ifp, 0, SEEK_END);
    parse (hlen, ftell(ifp)-hlen, 1);
    fclose (ifp);
  }
}
