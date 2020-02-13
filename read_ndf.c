#include <stdio.h>
#include <stdlib.h>
#include <string.h>

FILE *ifp;

void find_jpeg()
{
  unsigned short mark=0;
  int c;

  do mark = (mark << 8) + (c = fgetc(ifp));
  while (mark != 0xffd8 && c != EOF);
}

void write_jpeg (char *iname, int index)
{
  char *oname;
  FILE *ofp;
  unsigned short mark=0;
  int c;

  oname = malloc (strlen (iname) + 12);
  if (!oname) return;
  sprintf (oname, "%s.%d.jpg", iname, index);
  if (!(ofp = fopen (oname, "wb"))) {
    perror (oname);
    return;
  }
  putc (0xff, ofp);
  putc (0xd8, ofp);
  do {
    mark = (mark << 8) + (c = fgetc(ifp));
    putc (mark, ofp);
  } while (mark != 0xffd9 && c != EOF);
}

int main (int argc, char **argv)
{
  int arg;

  for (arg=1; arg < argc; arg++) {
    if (!(ifp = fopen (argv[arg], "rb"))) {
      perror (argv[arg]);
      continue;
    }
    find_jpeg();
    write_jpeg (argv[arg], 2);
    find_jpeg();
    write_jpeg (argv[arg], 1);
    if (ftell(ifp) & 1) fgetc(ifp);
    write_jpeg (argv[arg], 3);
    fclose (ifp);
  }
  return 0;
}
