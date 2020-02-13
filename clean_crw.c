/*
   Because they are parsed from the end, Canon CRW files
   become unreadable if garbage data is appended to them, as
   often happens when files are recovered from damaged media.
   This program truncates CRW files to the correct size.

   Copyright 2005 by Dave Coffin, dcoffin a cybercom o net
   Free for all uses.

   $Revision$
   $Date$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned char *buffer;

int get4 (int i)
{
  if (buffer[0] == 'I')
    return buffer[i+3] << 24 | buffer[i+2] << 16 | buffer[i+1] << 8 | buffer[i];
  else
    return buffer[i] << 24 | buffer[i+1] << 16 | buffer[i+2] << 8 | buffer[i+3];
}

int main (int argc, char **argv)
{
  int arg, size, end, diff, status=1;
  unsigned char *fname;
  FILE *fp;

  if (argc == 1)
    fprintf (stderr, "Usage:  %s crw_0001.crw crw_0002.crw ...\n", argv[0]);

  for (arg=1; arg < argc; arg++) {
    status = 1;
    fp = fopen (argv[arg], "rb");
    fseek (fp, 0, SEEK_END);
    size = ftell(fp);
    buffer = malloc (size + strlen(argv[arg]) + 10);
    if (!buffer) {
      fprintf (stderr, "Cannot allocate memory!\n");
      return 2;
    }
    fname = buffer + size;
    sprintf (fname, "%s.clean", argv[arg]);
    fseek (fp, 0, SEEK_SET);
    fread (buffer, 1, size, fp);
    fclose (fp);
    if (strncmp (buffer, "II\x1a\0\0\0HEAPCCDR", 14) &&
	strncmp (buffer, "MM\0\0\0\x1aHEAPCCDR", 14)) {
      fprintf (stderr, "%s is not a CRW file!\n", argv[arg]);
      free (buffer);
      continue;
    }
    for (end=size; end > 0xa0000; end--) {
      diff = end - get4(end-4);
      if (diff > 50 && diff < 120 && diff % 10 == 2) {
	status = 0;
	break;
      }
    }
    if (status)
      fprintf (stderr, "Failed to clean %s\n", argv[arg]);
    else {
      if ((fp = fopen (fname, "wb"))) {
	fprintf (stderr, "Writing %s\n", fname);
	fwrite (buffer, 1, end, fp);
	fclose (fp);
      } else {
	perror (fname);
	status = 1;
      }
    }
    free (buffer);
  }
  return status;
}
