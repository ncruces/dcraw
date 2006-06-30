#include <stdio.h>

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define ushort UshORt
typedef unsigned char uchar;
typedef unsigned short ushort;

short order;
int width, height, bps, data_offset;
FILE *ifp;

ushort sget2 (uchar *s)
{
  if (order == 0x4949)		/* "II" means little-endian */
    return s[0] | s[1] << 8;
  else				/* "MM" means big-endian */
    return s[0] << 8 | s[1];
}

ushort get2()
{
  uchar str[2] = { 0xff,0xff };
  fread (str, 1, 2, ifp);
  return sget2(str);
}

int sget4 (uchar *s)
{
  if (order == 0x4949)
    return s[0] | s[1] << 8 | s[2] << 16 | s[3] << 24;
  else
    return s[0] << 24 | s[1] << 16 | s[2] << 8 | s[3];
}

int get4()
{
  uchar str[4] = { 0xff,0xff,0xff,0xff };
  fread (str, 1, 4, ifp);
  return sget4(str);
}

void parse_tiff_ifd()
{
  unsigned entries, tag, type, len, save;

  entries = get2();
  if (entries > 512) return;
  while (entries--) {
    tag  = get2();
    type = get2();
    len  = get4();
    save = ftell(ifp) + 4;
    if (len * ("1112481124848"[type < 13 ? type:0]-'0') > 4)
      fseek (ifp, get4(), SEEK_SET);
    switch (tag) {
      case 256:  width       = get4();  break;
      case 257:  height      = get4();  break;
      case 258:  bps         = get2();  break;
      case 273:  data_offset = get4();  break;
      case 330:
	fseek (ifp, get4(), SEEK_SET);
	parse_tiff_ifd();
    }
    fseek (ifp, save, SEEK_SET);
  }
}

void parse_tiff()
{
  int doff;

  order = get2();
  if (order != 0x4949 && order != 0x4d4d) return;
  get2();
  while ((doff = get4())) {
    fseek (ifp, doff, SEEK_SET);
    parse_tiff_ifd();
  }
}

int main (int argc, char **argv)
{
  char buf[4096];
  int size, red;

  if (argc == 1) {
    fprintf (stderr, "Usage:  %s [options] file.nef\n", argv[0]);
    return 1;
  }
  if (!(ifp = fopen (argv[1], "rb"))) {
    perror (argv[1]);
    return 1;
  }
  width = height = bps = 0;
  parse_tiff();
  if (!height) {
    fprintf (stderr, "TIFF decode failed.\n");
    return 1;
  }
  printf ("P6 %d %d %d\n", width, height, (1 << bps)-1);
  size = width*height*3*bps/8;
  fseek (ifp, data_offset, SEEK_SET);
  while (size > 0) {
    red = fread (buf, 1, MIN(size,4096), ifp);
    if (bps > 8 && order == 0x4949)
      swab (buf, buf, red);
    fwrite (buf, 1, red, stdout);
    size -= 4096;
  }
  fclose (ifp);
  return 0;
}
