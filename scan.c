#include <stdio.h>

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define ushort UshORt
typedef unsigned char uchar;
typedef unsigned short ushort;

short order;
int width, height, data_offset;
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
  unsigned entries, tag, type, len, val, save;

  entries = get2();
  if (entries > 512) return;
  while (entries--) {
    tag  = get2();
    type = get2();
    len  = get4();
    val  = get4();
    save = ftell(ifp);
    switch (tag) {
      case 0x100:  width       = val;  break;
      case 0x101:  height      = val;  break;
      case 0x111:  data_offset = val;  break;
      case 0x14a:
	save = ftell(ifp);
	fseek (ifp, val, SEEK_SET);
	parse_tiff_ifd();
	fseek (ifp, save, SEEK_SET);
    }
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
  int size;

  if (argc == 1) {
    fprintf (stderr, "Usage:  %s [options] file.nef\n", argv[0]);
    return 1;
  }
  if (!(ifp = fopen (argv[1], "rb"))) {
    perror (argv[1]);
    return 1;
  }
  width = height = 0;
  parse_tiff();
  if (!height) {
    fprintf (stderr, "TIFF decode failed.\n");
    return 1;
  }
  printf ("P6 %d %d 255\n", width, height);
  size = width*height*3;
  fseek (ifp, data_offset, SEEK_SET);
  while (size > 0) {
    fwrite (buf, 1, fread(buf,1,MIN(size,4096),ifp), stdout);
    size -= 4096;
  }
  fclose (ifp);
  return 0;
}
