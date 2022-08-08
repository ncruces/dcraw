/*
   Raw Photo Parser
   Copyright 2004-2015 by Dave Coffin, dcoffin a cybercom o net

   This program displays raw metadata for all raw photo formats.
   It is free for all uses.

   $Revision: 1.78 $
   $Date: 2018/06/01 21:26:34 $
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

#define ushort UshORt
typedef unsigned char uchar;
typedef unsigned short ushort;

FILE *ifp;
short order;
char *fname, make[128], model[128], model2[128];
int is_dng;

void tseek(FILE *stream, long offset, int whence)
{
  char line[81];
  if( fseek(stream,offset,whence) != 0 ) {
    snprintf(line,80,"fseek(%p,%ld,%d)",stream,offset,whence);
    perror(line);
    exit(1);
  }
}

void tread(void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  char line[81];
  if( fread (ptr, size, nmemb, stream) != nmemb ) {
    snprintf(line,80,"fread(%p,%ld,%ld,%p)",ptr,(long)size,(long)nmemb,stream);
    perror(line);
    exit(1);
  }
}

void twrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  char line[81];
  if( fwrite (ptr, size, nmemb, stream) != nmemb ) {
    snprintf(line,80,"fwrite(%p,%ld,%ld,%p)",ptr,(long)size,(long)nmemb,stream);
    perror(line);
    exit(1);
  }
}

ushort sget2 (uchar *s)
{
  if (order == 0x4949)		/* "II" means little-endian */
    return s[0] | s[1] << 8;
  else				/* "MM" means big-endian */
    return s[0] << 8 | s[1];
}
#define sget2(s) sget2((uchar *)s)

ushort get2()
{
  uchar str[2] = { 0xff,0xff };
  tread (str, 1, 2, ifp);
  return sget2(str);
}

int sget4 (uchar *s)
{
  if (order == 0x4949)
    return s[0] | s[1] << 8 | s[2] << 16 | s[3] << 24;
  else
    return s[0] << 24 | s[1] << 16 | s[2] << 8 | s[3];
}
#define sget4(s) sget4((uchar *)s)

int get4()
{
  uchar str[4] = { 0xff,0xff,0xff,0xff };
  tread (str, 1, 4, ifp);
  return sget4(str);
}

float int_to_float (int i)
{
  union { int i; float f; } u;
  u.i = i;
  return u.f;
}

double get_double()
{
  union { char c[8]; double d; } u;
  int i, rev;

  rev = 7 * ((order == 0x4949) == (ntohs(0x1234) == 0x1234));
  for (i=0; i < 8; i++)
    u.c[i ^ rev] = fgetc(ifp);
  return u.d;
}

unsigned getbithuff (int nbits, const ushort *huff)
{
  static unsigned bitbuf=0;
  static int vbits=0, reset=0;
  unsigned c;

  if (nbits == -1)
    return bitbuf = vbits = reset = 0;
  if (nbits == 0 || vbits < 0) return 0;
  while (vbits < nbits && (c = fgetc(ifp)) != EOF) {
    bitbuf = (bitbuf << 8) + (uchar) c;
    vbits += 8;
  }
  c = bitbuf << (32-vbits) >> (32-nbits);
  if (huff) {
    vbits -= huff[c] >> 8;
    c = (uchar) huff[c];
  } else
    vbits -= nbits;
  return c;
}

#define getbits(n) getbithuff(n,0)
#define gethuff(h) getbithuff(*h,h+1)

int ljpeg_diff (ushort *huff)
{
  int len, diff;

  len = gethuff(huff);
  diff = getbits(len);
  if ((diff & (1 << (len-1))) == 0)
    diff -= (1 << len) - 1;
  return diff;
}

void tiff_dump(int base, int tag, int type, int count, int level)
{
  int save, j, num, den;
  uchar c;
  int size[] = { 1,1,1,2,4,8,1,1,2,4,8,4,8 };

  if (count * size[type < 13 ? type:0] > 4)
    tseek (ifp, get4()+base, SEEK_SET);
  save = ftell(ifp);
  printf("%*stag=0x%x %d, type=%d, count=%d, offset=%06x, data=",
	level*2, "", tag, tag, type, count, save);
  if (tag == 34310) goto quit;
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
	printf ("%c%04x",(j & 15) || count < 9 ? ' ':'\n', get2());
	break;
      case 4: case 9:			/* dword values */
	printf ("%c%08x",(j & 7) || count < 5 ? ' ':'\n', get4());
	break;
      case 5: case 10:			/* rationals */
	num = get4();
	den = get4();
	printf (" %d/%d", num, den);
//	printf (" %lf", (double) num/den);
	break;
    }
  if (type==2) putchar('\"');
quit:
  putchar('\n');
  tseek (ifp, save, SEEK_SET);
}

void parse_nikon_capture_note (int length)
{
  unsigned sorder, offset, tag, j, size;

  puts ("    Nikon Capture Note:");
  sorder = order;
  order = 0x4949;
  tseek (ifp, 22, SEEK_CUR);
  for (offset=22; offset+22 < length; offset += 22+size) {
    tag = get4();
    tseek (ifp, 14, SEEK_CUR);
    size = get4()-4;
    printf("      tag=0x%08x, size=%d", tag, size);
    for (j=0; j < size; j++)
      printf ("%s%02x", j & 31 ? " ":"\n\t", fgetc(ifp));
    puts("");
  }
  order = sorder;
}

void nikon_decrypt (uchar ci, uchar cj, int tag, int i, int size, uchar *buf)
{
  static const uchar xlat[2][256] = {
  { 0xc1,0xbf,0x6d,0x0d,0x59,0xc5,0x13,0x9d,0x83,0x61,0x6b,0x4f,0xc7,0x7f,0x3d,0x3d,
    0x53,0x59,0xe3,0xc7,0xe9,0x2f,0x95,0xa7,0x95,0x1f,0xdf,0x7f,0x2b,0x29,0xc7,0x0d,
    0xdf,0x07,0xef,0x71,0x89,0x3d,0x13,0x3d,0x3b,0x13,0xfb,0x0d,0x89,0xc1,0x65,0x1f,
    0xb3,0x0d,0x6b,0x29,0xe3,0xfb,0xef,0xa3,0x6b,0x47,0x7f,0x95,0x35,0xa7,0x47,0x4f,
    0xc7,0xf1,0x59,0x95,0x35,0x11,0x29,0x61,0xf1,0x3d,0xb3,0x2b,0x0d,0x43,0x89,0xc1,
    0x9d,0x9d,0x89,0x65,0xf1,0xe9,0xdf,0xbf,0x3d,0x7f,0x53,0x97,0xe5,0xe9,0x95,0x17,
    0x1d,0x3d,0x8b,0xfb,0xc7,0xe3,0x67,0xa7,0x07,0xf1,0x71,0xa7,0x53,0xb5,0x29,0x89,
    0xe5,0x2b,0xa7,0x17,0x29,0xe9,0x4f,0xc5,0x65,0x6d,0x6b,0xef,0x0d,0x89,0x49,0x2f,
    0xb3,0x43,0x53,0x65,0x1d,0x49,0xa3,0x13,0x89,0x59,0xef,0x6b,0xef,0x65,0x1d,0x0b,
    0x59,0x13,0xe3,0x4f,0x9d,0xb3,0x29,0x43,0x2b,0x07,0x1d,0x95,0x59,0x59,0x47,0xfb,
    0xe5,0xe9,0x61,0x47,0x2f,0x35,0x7f,0x17,0x7f,0xef,0x7f,0x95,0x95,0x71,0xd3,0xa3,
    0x0b,0x71,0xa3,0xad,0x0b,0x3b,0xb5,0xfb,0xa3,0xbf,0x4f,0x83,0x1d,0xad,0xe9,0x2f,
    0x71,0x65,0xa3,0xe5,0x07,0x35,0x3d,0x0d,0xb5,0xe9,0xe5,0x47,0x3b,0x9d,0xef,0x35,
    0xa3,0xbf,0xb3,0xdf,0x53,0xd3,0x97,0x53,0x49,0x71,0x07,0x35,0x61,0x71,0x2f,0x43,
    0x2f,0x11,0xdf,0x17,0x97,0xfb,0x95,0x3b,0x7f,0x6b,0xd3,0x25,0xbf,0xad,0xc7,0xc5,
    0xc5,0xb5,0x8b,0xef,0x2f,0xd3,0x07,0x6b,0x25,0x49,0x95,0x25,0x49,0x6d,0x71,0xc7 },
  { 0xa7,0xbc,0xc9,0xad,0x91,0xdf,0x85,0xe5,0xd4,0x78,0xd5,0x17,0x46,0x7c,0x29,0x4c,
    0x4d,0x03,0xe9,0x25,0x68,0x11,0x86,0xb3,0xbd,0xf7,0x6f,0x61,0x22,0xa2,0x26,0x34,
    0x2a,0xbe,0x1e,0x46,0x14,0x68,0x9d,0x44,0x18,0xc2,0x40,0xf4,0x7e,0x5f,0x1b,0xad,
    0x0b,0x94,0xb6,0x67,0xb4,0x0b,0xe1,0xea,0x95,0x9c,0x66,0xdc,0xe7,0x5d,0x6c,0x05,
    0xda,0xd5,0xdf,0x7a,0xef,0xf6,0xdb,0x1f,0x82,0x4c,0xc0,0x68,0x47,0xa1,0xbd,0xee,
    0x39,0x50,0x56,0x4a,0xdd,0xdf,0xa5,0xf8,0xc6,0xda,0xca,0x90,0xca,0x01,0x42,0x9d,
    0x8b,0x0c,0x73,0x43,0x75,0x05,0x94,0xde,0x24,0xb3,0x80,0x34,0xe5,0x2c,0xdc,0x9b,
    0x3f,0xca,0x33,0x45,0xd0,0xdb,0x5f,0xf5,0x52,0xc3,0x21,0xda,0xe2,0x22,0x72,0x6b,
    0x3e,0xd0,0x5b,0xa8,0x87,0x8c,0x06,0x5d,0x0f,0xdd,0x09,0x19,0x93,0xd0,0xb9,0xfc,
    0x8b,0x0f,0x84,0x60,0x33,0x1c,0x9b,0x45,0xf1,0xf0,0xa3,0x94,0x3a,0x12,0x77,0x33,
    0x4d,0x44,0x78,0x28,0x3c,0x9e,0xfd,0x65,0x57,0x16,0x94,0x6b,0xfb,0x59,0xd0,0xc8,
    0x22,0x36,0xdb,0xd2,0x63,0x98,0x43,0xa1,0x04,0x87,0x86,0xf7,0xa6,0x26,0xbb,0xd6,
    0x59,0x4d,0xbf,0x6a,0x2e,0xaa,0x2b,0xef,0xe6,0x78,0xb6,0x4e,0xe0,0x2f,0xdc,0x7c,
    0xbe,0x57,0x19,0x32,0x7e,0x2a,0xd0,0xb8,0xba,0x29,0x00,0x3c,0x52,0x7d,0xa8,0x49,
    0x3b,0x2d,0xeb,0x25,0x49,0xfa,0xa3,0xaa,0x39,0xa7,0xc5,0xa7,0x50,0x11,0x36,0xfb,
    0xc6,0x67,0x4a,0xf5,0xa5,0x12,0x65,0x7e,0xb0,0xdf,0xaf,0x4e,0xb3,0x61,0x7f,0x2f } };
  uchar ck=0x60;

  if (strncmp ((char *)buf, "02", 2)) return;
  ci = xlat[0][ci];
  cj = xlat[1][cj];
  printf("Decrypted tag 0x%x:\n%*s", tag, (i & 31)*3, "");
  for (; i < size; i++)
    printf("%02x%c", buf[i] ^ (cj += ci * ck++), (i & 31) == 31 ? '\n':' ');
  if (size & 31) puts("");
}

int parse_tiff_ifd (int base, int level);

void parse_makernote (int base, int len, int level)
{
  int offset=0, entries, tag, type, count, val, save;
  unsigned serial=0, key=0;
  uchar buf91[630]="", buf97[608]="", buf98[31]="";
  short sorder;
  char buf[10];

/*
   The MakerNote might have its own TIFF header (possibly with
   its own byte-order!), or it might just be a table.
 */
  sorder = order;
  tread (buf, 1, 10, ifp);
  if (!strcmp (buf,"Nikon")) {	/* starts with "Nikon\0\2\0\0\0" ? */
    base = ftell(ifp);
    order = get2();		/* might differ from file-wide byteorder */
    val = get2();		/* should be 42 decimal */
    offset = get4();
    tseek (ifp, offset-8, SEEK_CUR);
  } else if (!strcmp (buf,"OLYMPUS") ||
	     !strcmp (buf,"PENTAX ")) {
    base = ftell(ifp)-10;
    tseek (ifp, -2, SEEK_CUR);
    order = get2();
    if (buf[0] == 'O') get2();
  } else if (!strncmp (buf,"SONY",4) ||
	     !strcmp  (buf,"Panasonic")) {
    goto nf;
  } else if (!strncmp (buf,"FUJIFILM",8)) {
    base = ftell(ifp)-10;
nf: order = 0x4949;
    tseek (ifp,  2, SEEK_CUR);
  } else if (!strcmp (buf,"OLYMP") ||
	     !strcmp (buf,"LEICA") ||
	     !strcmp (buf,"Ricoh") ||
	     !strcmp (buf,"EPSON"))
    tseek (ifp, -2, SEEK_CUR);
  else if (!strcmp (buf,"AOC") ||
	   !strcmp (buf,"QVC"))
    tseek (ifp, -4, SEEK_CUR);
  else {
    tseek (ifp, -10, SEEK_CUR);
    if (!strncmp(make,"SAMSUNG",7))
      base = ftell(ifp);
    if (!strncmp (buf,"ev=",3)) {
      while (len--) putchar (fgetc(ifp));
      putchar ('\n');
    }
  }

  entries = get2();
  if (entries > 127) return;
  printf ("%*sMakerNote:\n", level*2-2, "");
  while (entries--) {
    save = ftell(ifp);
    tag  = get2();
    type = get2();
    count= get4();
    tiff_dump (base, tag, type, count, level);
    if ((tag      == 0x11 && !strncmp(make,"NIKON",5)) ||
	(tag >> 8 == 0x20 && !strncmp(buf ,"OLYMP",5)) || type == 13) {
      if (count == 1)
	tseek (ifp, get4()+base, SEEK_SET);
      parse_tiff_ifd (base, level+1);
    }
    if (tag == 0x1d)
      while ((val = fgetc(ifp)) && val != EOF)
	serial = serial*10 + (isdigit(val) ? val - '0' : val % 10);
    if (tag == 0x91)
      tread (buf91, sizeof buf91, 1, ifp);
    if (tag == 0x97)
      tread (buf97, sizeof buf97, 1, ifp);
    if (tag == 0x98)
      tread (buf98, sizeof buf98, 1, ifp);
    if (tag == 0xa7)
      key = fgetc(ifp)^fgetc(ifp)^fgetc(ifp)^fgetc(ifp);
    if (tag == 0xe01)
      parse_nikon_capture_note (count);
    if (tag == 0xb028) {
      tseek (ifp, get4(), SEEK_SET);
      parse_tiff_ifd (base, level+1);
    }
    tseek (ifp, save+12, SEEK_SET);
  }
  nikon_decrypt (serial, key, 0x91, 4, sizeof buf91, buf91);
  if (!strncmp ((char *)buf97, "0205", 4))
    nikon_decrypt (serial, key, 0x97, 4, 284, buf97);
  else
    nikon_decrypt (serial, key, 0x97, 284, sizeof buf97, buf97);
  nikon_decrypt (serial, key, 0x98, 4, sizeof buf98, buf98);
  order = sorder;
}

void parse_exif (int base, int level)
{
  int entries, tag, type, count, save;

  printf ("%*sEXIF table:\n", level*2-2, "");
  entries = get2();
  while (entries--) {
    save = ftell(ifp);
    tag  = get2();
    type = get2();
    count= get4();
    tiff_dump (base, tag, type, count, level);
    if (tag == 0x927c)
      parse_makernote (base, count, level+1);
    tseek (ifp, save+12, SEEK_SET);
  }
}

void parse_mos(int level);
void parse_minolta (int base);
void parse_tiff (int base, int level);

void parse_thumb (int base, int level)
{
  int i=order;
  order = 0x4d4d;
  tseek (ifp, base, SEEK_SET);
  if (get4()==0xffd8ffe1 && get2() && get4()==0x45786966 && !get2()) {
    printf ("%*sEmbedded JPEG:\n", level*2, "");
    parse_tiff (ftell(ifp), level+1);
  }
  order = i;
}

void sony_decrypt (unsigned *data, int len, int start, int key)
{
  static unsigned pad[128], p;

  if (start) {
    for (p=0; p < 4; p++)
      pad[p] = key = key * 48828125 + 1;
    pad[3] = pad[3] << 1 | (pad[0]^pad[2]) >> 31;
    for (p=4; p < 127; p++)
      pad[p] = (pad[p-4]^pad[p-2]) << 1 | (pad[p-3]^pad[p-1]) >> 31;
    for (p=0; p < 127; p++)
      pad[p] = htonl(pad[p]);
  }
  while (len-- && p++)
    *data++ ^= pad[(p-1) & 127] = pad[p & 127] ^ pad[(p+64) & 127];
}

int parse_tiff_ifd (int base, int level)
{
  int entries, tag, type, count, slen, save, save2, i;
  unsigned *buf, sony_offset=0, sony_length=0, sony_key=0;
  FILE *sfp;

  entries = get2();
  if (entries > 1024) return 1;
  while (entries--) {
    save = ftell(ifp);
    tag  = get2();
    type = get2();
    count= get4();
    slen = count;
    if (slen > 128) slen = 128;
    tiff_dump (base, tag, type, count, level);
    if (type == 13) tag = 0x14a;
    switch (tag) {
      case 271:				/* Make tag */
	fgets (make, slen, ifp);
	break;
      case 272:				/* Model tag */
	fgets (model, slen, ifp);
	break;
      case 33405:			/* Model2 tag */
	fgets (model2, slen, ifp);
	break;
      case 330:				/* SubIFD tag */
	save2 = ftell(ifp);
	for (i=0; i < count; i++) {
	  printf ("SubIFD #%d:\n", i+1);
	  tseek (ifp, save2 + i*4, SEEK_SET);
	  tseek (ifp, get4()+base, SEEK_SET);
	  parse_tiff_ifd (base, level+1);
	}
	break;
      case 273:				/* StripOffset */
      case 513:				/* JpegIFOffset */
      case 61447:
	tseek (ifp, get4()+base, SEEK_SET);
      case 46:
	parse_thumb (ftell(ifp), level);
	break;
      case 29184: sony_offset = get4();  break;
      case 29185: sony_length = get4();  break;
      case 29217: sony_key    = get4();  break;
      case 29264:
	parse_minolta (ftell(ifp));
	break;
      case 33424:
      case 65024:
	puts("Kodak private data:");
	tseek (ifp, get4()+base, SEEK_SET);
	parse_tiff_ifd (base, level+1);
	break;
      case 34310:
	parse_mos(0);
	break;
      case 34665:
	tseek (ifp, get4()+base, SEEK_SET);
	parse_exif (base, level+1);
	break;
      case 34853:
	puts("GPS data:");
	tseek (ifp, get4()+base, SEEK_SET);
	parse_tiff_ifd (base, level+1);
	break;
      case 50459:
	i = order;
	save2 = ftell(ifp);
	order = get2();
	tseek (ifp, save2 + (get2(),get4()), SEEK_SET);
	parse_tiff_ifd (save2, level+1);
	order = i;
	break;
      case 50706:
	is_dng = 1;
	break;
      case 50740:
	if (is_dng) break;
	parse_minolta (i = get4()+base);
	tseek (ifp, i, SEEK_SET);
	parse_tiff_ifd (base, level+1);
    }
    tseek (ifp, save+12, SEEK_SET);
  }
  if (sony_length && (buf = malloc(sony_length))) {
    tseek (ifp, sony_offset, SEEK_SET);
    tread (buf, sony_length, 1, ifp);
    sony_decrypt (buf, sony_length/4, 1, sony_key);
    sfp = ifp;
    if ((ifp = tmpfile())) {
      twrite (buf, sony_length, 1, ifp);
      tseek (ifp, 0, SEEK_SET);
      puts ("Sony SR2 encrypted IFD:");
      parse_tiff_ifd (-sony_offset, level);
      fclose (ifp);
    }
    ifp = sfp;
    free (buf);
  }
  return 0;
}

/*
   Parse a TIFF file looking for camera model and decompress offsets.
 */
void parse_tiff (int base, int level)
{
  int doff, ifd=0, sorder=order;

  tseek (ifp, base, SEEK_SET);
  order = get2();
  if (order != 0x4949 && order != 0x4d4d) return;
  get2();
  while ((doff = get4())) {
    tseek (ifp, doff+base, SEEK_SET);
    printf ("%*sIFD #%d:\n", level*2, "", ifd++);
    if (parse_tiff_ifd (base, level)) break;
  }
  order = sorder;
}

void parse_minolta (int base)
{
  unsigned offset, save, len, j;
  char tag[4];

  tseek (ifp, base, SEEK_SET);
  if (fgetc(ifp) || fgetc(ifp)-'M' || fgetc(ifp)-'R') return;
  order = fgetc(ifp) * 0x101;
  offset = base + get4() + 8;
  while ((save=ftell(ifp)) < offset) {
    tread (tag, 1, 4, ifp);
    len = get4();
    printf ("Minolta tag %3.3s offset %06x length %06x", tag+1, save, len);
    if (!strncmp (tag+1,"TTW",3)) {
      putchar ('\n');
      parse_tiff (ftell(ifp),0);
    } else {
      for (j = 0; j < len/2 && j < 128; j++)
        printf ("%c%04x",(j & 15) || len < 9 ? ' ':'\n', get2());
      putchar ('\n');
    }
    tseek (ifp, save+len+8, SEEK_SET);
  }
}

/*
   Parse the CIFF structure.
 */
void parse_ciff (int offset, int length, int level)
{
  int tboff, nrecs, i, j, type, len, dlen, roff, aoff=0, save;
  char c, name[256];
  ushort key[2];

  tseek (ifp, offset+length-4, SEEK_SET);
  tboff = get4() + offset;
  tseek (ifp, tboff, SEEK_SET);
  nrecs = get2();
  if (nrecs > 100) return;
  printf ("%*s%d records:\n", level*2, "", nrecs);
  for (i = 0; i < nrecs; i++) {
    save = ftell(ifp);
    type = get2();
    printf ("%*stype=0x%04x", level*2, "", type);
    if (type & 0x4000) {
      len = 8;
      type &= 0x3fff;
    } else {
      len  = get4();
      roff = get4();
      aoff = offset + roff;
      printf (", length=%d, reloff=%d, absoff=%d",
		len, roff, aoff);
      tseek (ifp, aoff, SEEK_SET);
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
	tseek (ifp, save+10, SEEK_SET);
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
	key[0] = get2();
	tseek (ifp, -2, SEEK_CUR);
	if (type == 0x1032 && key[0] == 1040)
	  key[1] = 17907;
	else key[0] = key[1] = 0;
	for (j = 0; j < dlen; j+=2)
	  printf ("%c%5u",(j & 31) || dlen < 16 ? ' ':'\n',
		get2() ^ key[(j >> 1) & 1]);
	break;
      case 0x18:			/* dword values */
	for (j = 0; j < dlen; j+=4)
	  printf ("%c%08x",(j & 31) || dlen < 16 ? ' ':'\n', get4());
    }
    putchar('\n');
    tseek (ifp, save+10, SEEK_SET);
    if (type == 0x080a) {		/* Get the camera name */
      tseek (ifp, aoff, SEEK_SET);
      tread (name, 256, 1, ifp);
      strcpy (make, name);
      strcpy (model, name + strlen(make)+1);
    }
  }
}

int parse_jpeg (int offset)
{
  int len, save, hlen;

  tseek (ifp, offset, SEEK_SET);
  if (fgetc(ifp) != 0xff || fgetc(ifp) != 0xd8) return 0;

  while (fgetc(ifp) == 0xff && fgetc(ifp) >> 4 != 0xd) {
    order = 0x4d4d;
    len   = get2() - 2;
    save  = ftell(ifp);
    order = get2();
    hlen  = get4();
    if (get4() == 0x48454150)		/* "HEAP" */
      parse_ciff (save+hlen, len-hlen, 0);
    parse_tiff (save+6,0);
    tseek (ifp, save+len, SEEK_SET);
  }
  return 1;
}

void parse_riff (int level)
{
  unsigned i, size, end, save;
  char tag[4], type[4], buf[64];

  order = 0x4949;
  tread (tag, 4, 1, ifp);
  size = get4();
  if (isdigit(tag[0])) {
    tseek (ifp, size, SEEK_CUR);
    return;
  }
  printf ("%*.4s size %d", level*4+4, tag, size);
  if (!memcmp(tag,"RIFF",4) || !memcmp(tag,"LIST",4)) {
    end = ftell(ifp) + size;
    tread (type, 4, 1, ifp);
    printf (" type %.4s:\n", type);
    while (ftell(ifp)+7 < end)
      parse_riff (level+1);
  } else {
    save = ftell(ifp);
    tread (buf, 1, 40, ifp);
    printf (": ");
    for (i=0; i < 40 && isprint(buf[i]); i++)
      putchar (buf[i]);
    putchar ('\n');
    tseek (ifp, save+size, SEEK_SET);
  }
}

void parse_mos(int level)
{
  char data[256];
  int i, skip, save;
  char *cp;

  save = ftell(ifp);
  while (1) {
    if (get4() != 0x504b5453) break;
    get4();
    printf ("%*sPKTS ", level, "");
    tread (data, 1, 40, ifp);
    skip = get4();
    printf ("%s %d bytes: ", data, skip);
    if (!strcmp(data,"pattern_ratation_angle")) {
      printf ("%d\n", get2());
      continue;
    }
    if (!strcmp(data,"icc_camera_to_tone_matrix")) {
      for (i=0; i < skip/4; i++)
	printf ("%f ", int_to_float(get4()));
      putchar('\n');
      continue;
    }
    tread (data, 1, sizeof data, ifp);
    tseek (ifp, -sizeof data, SEEK_CUR);
    data[sizeof data - 1] = 0;
    while ((cp=strchr(data,'\n')))
      *cp = ' ';
    printf ("%s\n",data);
    parse_mos(level+2);
    tseek (ifp, skip, SEEK_CUR);
  }
  tseek (ifp, save, SEEK_SET);
}

void parse_rollei()
{
  char line[128], *val;

  tseek (ifp, 0, SEEK_SET);
  do {
    fgets (line, 128, ifp);
    fputs (line, stdout);
    if ((val = strchr(line,'=')))
      *val++ = 0;
    else
      val = line + strlen(line);
  } while (strncmp(line,"EOHD",4));
  strcpy (make, "Rollei");
  strcpy (model, "d530flex");
}

void get_utf8 (int offset, char *buf, int len)
{
  ushort c;
  char *cp;

  tseek (ifp, offset, SEEK_SET);
  for (cp=buf; (c = get2()) && cp+3 < buf+len; ) {
    if (c < 0x80)
      *cp++ = c;
    else if (c < 0x800) {
      *cp++ = 0xc0 + (c >> 6);
      *cp++ = 0x80 + (c & 0x3f);
    } else {
      *cp++ = 0xe0 + (c >> 12);
      *cp++ = 0x80 + (c >> 6 & 0x3f);
      *cp++ = 0x80 + (c & 0x3f);
    }
  }
  *cp = 0;
}

void parse_foveon()
{
  unsigned entries, off, len, tag, save, i, j, k, pent, poff[256][2];
  char name[128], value[128], *camf, *pos, *cp, *dp;
  unsigned val, wide, high, row, col, diff, type, num, ndim, dim[3];
  ushort huff[258], vpred[2][2], hpred[2];

  order = 0x4949;			/* Little-endian */
  tseek (ifp, -4, SEEK_END);
  tseek (ifp, get4(), SEEK_SET);
  if (get4() != 0x64434553) {	/* SECd */
    printf ("Bad Section identifier at %6x\n", (int)ftell(ifp)-4);
    return;
  }
  get4();
  entries = get4();
  while (entries--) {
    off = get4();
    len = get4();
    tag = get4();
    save = ftell(ifp);
    tseek (ifp, off, SEEK_SET);
    printf ("%c%c%c%c at offset %06x, length %06x, ",
	tag, tag >> 8, tag >> 16, tag >> 24, off, len);
    if (get4() != (0x20434553 | (tag << 24))) {
      printf ("Bad Section identifier at %6x\n", off);
      return;
    }
    val = get4();
    printf ("version %d.%d, ",val >> 16, val & 0xffff);
    switch (tag) {
      case 0x32414d49:			/* IMA2 */
      case 0x47414d49:			/* IMAG */
	printf ("type %d, "	, get4());
	printf ("format %2d, "	, get4());
	printf ("columns %4d, "	, get4());
	printf ("rows %4d, "	, get4());
	printf ("rowsize %d\n"	, get4());
	parse_jpeg (off+28);
	order = 0x4949;
	break;
      case 0x464d4143:			/* CAMF */
	type = get4();
	printf ("type %d\n", type);
	get4(); get4();
	wide = get4();
	high = get4();
	if (type == 2) {
	  camf = malloc (len -= 28);
	  tread (camf, 1, len, ifp);
	  for (i=0; i < len; i++) {
	    high = (high * 1597 + 51749) % 244944;
	    val = high * (INT64) 301593171 >> 24;
	    camf[i] ^= ((((high << 8) - val) >> 1) + val) >> 17;
	  }
	} else if (type == 4) {
	  camf = malloc (len = wide*high*3/2);
	  memset (huff, 0xff, sizeof huff);
	  huff[0] = 8;
	  for (i=0; i < 13; i++) {
	    tag = getc(ifp);
	    val = getc(ifp);
	    for (j=0; j < 256 >> tag; )
	      huff[val+ ++j] = tag << 8 | i;
	  }
	  tseek (ifp, 6, SEEK_CUR);
	  getbits(-1);
	  vpred[0][0] = vpred[0][1] =
	  vpred[1][0] = vpred[1][1] = 512;
	  for (j=row=0; row < high; row++) {
	    for (col=0; col < wide; col++) {
	      diff = ljpeg_diff(huff);
	      if (col < 2) hpred[col] = vpred[row & 1][col] += diff;
	      else         hpred[col & 1] += diff;
	      if (col & 1) {
		camf[j++] = hpred[0] >> 4;
		camf[j++] = hpred[0] << 4 | hpred[1] >> 8;
		camf[j++] = hpred[1];
	      }
	    }
	  }
	} else {
	  printf ("Unknown CAMF type %d\n", type);
	  break;
	}
	for (pos=camf; (unsigned) (pos-camf) < len; pos += sget4(pos+8)) {
	  if (strncmp (pos, "CMb", 3)) goto done;
	  val = sget4(pos+4);
	  printf ("  %4.4s version %d.%d: ", pos, val >> 16, val & 0xffff);
	  switch (pos[3]) {
	    case 'M':
	      cp = pos + sget4(pos+16);
	      type = sget4(cp);
	      ndim = sget4(cp+4);
	      dim[0] = dim[1] = dim[2] = 1;
	      printf ("%d-dimensonal array %s of type %d:\n    Key: (",
		ndim, pos+sget4(pos+12), sget4(cp));
	      dp = pos + sget4(cp+8);
	      for (i=ndim; i--; ) {
		cp += 12;
		dim[i] = sget4(cp);
		printf ("%s %d%s", pos+sget4(cp+4), dim[i], i ? ", ":")\n");
	      }
	      for (i=0; i < dim[2]; i++) {
		for (j=0; j < dim[1]; j++) {
		  printf ("    ");
		  for (k=0; k < dim[0]; k++)
		    switch (type) {
		      case 5:
			printf ("%7d", *(uchar *)dp++);
			break;
		      case 0:
		      case 6:
			printf ("%7d", (short) sget2(dp));
			dp += 2;
			break;
		      case 1:
		      case 2:
			printf (" %d", sget4(dp));
			dp += 4;
			break;
		      case 3:
			val = sget4(dp);
			printf (" %9f", int_to_float(val));
			dp += 4;
		    }
		  printf ("\n");
		}
		printf ("\n");
	      }
	      break;
	    case 'P':
	      val = sget4(pos+16);
	      num = sget4(pos+val);
	      printf ("%s, %d parameters:\n", pos+sget4(pos+12), num);
	      cp = pos+val+8 + num*8;
	      for (i=0; i < num; i++) {
		val += 8;
		printf ("    %s = %s\n", cp+sget4(pos+val), cp+sget4(pos+val+4));
	      }
	      break;
	    case 'T':
	      cp = pos + sget4(pos+16);
	      printf ("%s = %.*s\n", pos+sget4(pos+12), sget4(cp), cp+4);
	      break;
	    default:
	      printf ("\n");
	  }
	}
done:	free (camf);
	break;
      case 0x504f5250:			/* PROP */
	printf ("entries %d, ", pent=get4());
	printf ("charset %d, ", get4());
	get4();
	printf ("nchars %d\n", get4());
	off += pent*8 + 24;
	if ((unsigned) pent > 256) pent=256;
	for (i=0; i < pent*2; i++)
	  poff[0][i] = off + get4()*2;
	for (i=0; i < pent; i++) {
	  get_utf8 (poff[i][0], name, 128);
	  get_utf8 (poff[i][1], value, 128);
	  printf ("  %s = %s\n", name, value);
	  if (!strcmp (name,"CAMMANUF"))
	    strcpy (make, value);
	  if (!strcmp (name,"CAMMODEL"))
	    strcpy (model, value);
	}
    }
    tseek (ifp, save, SEEK_SET);
  }
}

void parse_fuji (int offset)
{
  int entries, tag, len;

  tseek (ifp, offset, SEEK_SET);
  if (!(len = get4())) return;
  printf ("Fuji Image %c:\n", offset < 100 ? 'S':'R');
  tseek (ifp, len, SEEK_SET);
  entries = get4();
  if (entries > 255) return;
  while (entries--) {
    tag = get2();
    len = get2();
    printf ("Fuji tag=0x%x, len=%d, data =",tag,len);
    while (len--)
      printf (" %02x",fgetc(ifp));
    putchar ('\n');
  }
}

void parse_phase_one (int base)
{
  unsigned entries, tag, type, len, data, save;
  unsigned meta=0, wide=0, high=0, i, j;
  char str[256];

  tseek (ifp, base, SEEK_SET);
  order = get4() & 0xffff;
  if (get4() >> 8 != 0x526177) return;
  tseek (ifp, base+get4(), SEEK_SET);
  entries = get4();
  get4();
  while (entries--) {
    tag  = get4();
    type = get4();
    len  = get4();
    data = get4();
    save = ftell(ifp);
    printf ("Phase One tag=0x%x, type=%d, len=%2d, data = 0x%x",
		tag, type, len, data);
    if (type == 4 && len == 4 && data > 0xfffffff)
      printf (" = %f", int_to_float(data));
    putchar ('\n');
    if (tag == 0x110) meta = base+data;
    if (len > 4)
      tseek (ifp, base+data, SEEK_SET);
    if (type == 1 && len < 256) {
      tread (str, 256, 1, ifp);
      puts (str);
    }
    if (tag != 0x21c && type == 4 && len > 4) {
      for ( ; len > 0; len -= 4)
	printf ("%f ", int_to_float(get4()));
      puts ("");
    }
    tseek (ifp, save, SEEK_SET);
  }
  strcpy (make, "Phase One");
  strcpy (model, "unknown");
  if (!meta) return;
  tseek (ifp, meta, SEEK_SET);
  order = get2();
  tseek (ifp, 6, SEEK_CUR);
  tseek (ifp, meta+get4(), SEEK_SET);
  entries = get4();
  get4();
  while (entries--) {
    tag  = get4();
    len  = get4();
    data = get4();
    save = ftell(ifp);
    printf ("Phase One meta tag=0x%x, len=%2d, offset = 0x%x, data = ",
		tag, len, data);
    if (!((0x000801f4 >> (tag-0x400)) & 1)) putchar ('\n');
    tseek (ifp, meta+data, SEEK_SET);
    switch (tag) {
      case 0x400:
	for (i=0; i < len; i+=2)
	  printf ("%5u%c", get2(), (i & 6) == 6 || i == len-2 ? '\n':' ');
	break;
      case 0x401:
	for (i=0; i < 16; i+=2)
	  printf ("%6u%c", get2(), (i & 14) == 14 || i == len-2 ? '\n':' ');
	for (; i < len; i+=4)
	  printf ("%9.6f%c", int_to_float(get4()),
		(i & 28) == 12 || i == len-4 ? '\n':' ');
	break;
      case 0x402:
	printf ("%f\n", int_to_float (data));
	break;
      case 0x404: case 0x405: case 0x406: case 0x407:
	tread (str, 256, 1, ifp);
	puts (str);
	break;
      case 0x408: case 0x413:
	printf ("%lf\n", get_double());
	break;
      case 0x40b: case 0x410: case 0x416:
	for (i=0; i < len; i+=2)
	  printf ("%6u%c", get2(), (i & 14) == 14 || i == len-2 ? '\n':' ');
	break;
      case 0x40f: case 0x418: case 0x419: case 0x41a:
	for (i=0; i < 4; i++)
	  printf ("%02X%c", fgetc(ifp), i == 3 ? '\n':' ');
	for (; i < len; i+=4)
	  printf ("%e%c", int_to_float(get4()), i == len-4 ? '\n':' ');
	break;
      case 0x412:
	for (i=0; i < 36; i+=4) {
	  printf ("%u ", j=get4());
	  if (i ==  4) wide = j;
	  if (i == 12) high = j*2;
	}
	printf ("%u\n", get2());
	for (i=0; i < wide*high; i++)
	  printf ("%9.6f%c", int_to_float(get4()),
			i % wide == wide-1 ? '\n':' ');
	for (i=0; i < wide*high; i++)
	  printf ("%5u%c", get2(), i % wide == wide-1 ? '\n':' ');
	break;
      default:
	for (i=0; i < len; i++)
	  printf ("%02X%c", fgetc(ifp),
		(i & 15) == 15 || i == len-1 ? '\n':' ');
    }
    tseek (ifp, save, SEEK_SET);
  }
}

void parse_uuid (int level)
{
  unsigned i, len, tag;
  char buf[0x8000];

  for (;;) {
    len = get2();
    tag = get2();
    if (!len) break;
    printf ("%*stag = 0x%x, len=%d, ", level*2, "", tag, len);
    switch (tag >> 12) {
      case 1:
	if (len-4 < sizeof buf) {
	  tread (buf, 1,len-4, ifp);
	  printf ("\"%.*s\"", len-4, buf);
	}
	break;
      case 2:
	for (i=4; i < len; i+=4)
	  printf ("%f ",int_to_float(get4()));
	break;
      default:
	for (i=4; i < len; i++)
	  printf ("%02x", getc(ifp));
    }
    putchar ('\n');
  }
}

void parse_redcine (off_t base, int level)
{
  unsigned i, len, tag, ulen, utag;
  char c, ctag[4], buf[0x8000];

  do {
    fseeko (ifp, base, SEEK_SET);
    len = get4();
    tag = get4();
    if (feof(ifp)) break;
    for (i=0; i < 4; i++) {
      ctag[i] = tag >> ((3-i) << 3);
      if (!isprint(ctag[i])) ctag[i] = '.';
    }
    printf ("%*soff=0x%llx, len=%d, tag=0x%x \"%.4s\"\n",
      level*2, "", (INT64) base, len, tag, ctag);
    switch (tag) {
      case 0x52454431:			/* RED1 */
	tseek (ifp, 59, SEEK_CUR);
	tread (buf, 1, 256, ifp);
	printf ("  Original name: %s\n", buf);
	break;
      case 0x52454432:			/* RED2 */
	tseek (ifp, 18, SEEK_CUR);
      case 0x52444901:			/* RDI */
	tseek (ifp, 88, SEEK_CUR);
	parse_uuid (level+1);
	base = -(-(base+len) & -4096);
	continue;
#if 0
      case 0x52454441:			/* REDA */
	tread (buf, 1, sizeof buf, ifp);
	twrite (buf+24, 1, len-32, stdout);
	break;
#endif
      case 0x52454456:			/* REDV */
	printf ("  seq = %d, time = %d\n", get4(), get4());
	parse_redcine (base+20, level+1);
	break;
      case 0x75756964:			/* uuid */
	tseek (ifp, 16, SEEK_CUR);
	parse_uuid (level+1);
    }
    base += len;
  } while (len);
}

void parse_crx (int level, int end)
{
  int i, uuid[4], size, save;
  char tag[4], buf[400];

  while ((save = ftell(ifp)) < end) {
    order = 0x4d4d;
    size = get4();
    if (size < 8 || save+size > end) {
      tseek (ifp, -4, SEEK_CUR);
      tread (buf, 1, 400, ifp);
      printf (" =");
      for (i=0; i < 400 && i < end-save; i++)
        printf ("%s%02x",i & 3 ? "":" ",buf[i] & 255);
      tseek (ifp, end, SEEK_SET);
      return;
    }
    tread (tag, 4, 1, ifp);
    printf ("\n%*.4s size %d", level*2+4, tag, size);
    memset (uuid, 0, 16);
    if (!memcmp(tag,"uuid",4)) {
      for (i=0; i < 4; i++) uuid[i] = get4();
      tseek (ifp, -16, SEEK_CUR);
      printf(" = ");
      for (i=0; i < 16; i++)
	printf ("%s%02x",(0x550 >> i) & 1 ? "-":"", fgetc(ifp));
    }
    if (!memcmp(tag,"stsd",4))
      tseek (ifp, 8, SEEK_CUR);
    if (!memcmp(tag,"CMT",3)) {
      putchar ('\n');
      parse_tiff (ftell(ifp),level+1);
    } else parse_crx (level+1, save+size);
    tseek (ifp, save+size, SEEK_SET);
  }
  if (!level) {
    printf ("Finished parsing at offset 0x%lx, ",ftell(ifp));
    printf ("mdat %sfound\n", get4() == 0x6d646174 ? "":"not ");
  }
}

void parse_qt (int level, int end)
{
  unsigned i, lcase, size, save;
  char tag[4], buf[64];

  order = 0x4d4d;
  while (ftell(ifp)+7 < end) {
    save = ftell(ifp);
    if ((size = get4()) < 8) return;
    tread (tag, 4, 1, ifp);
    printf ("%*.4s size %d", level*4+4, tag, size);
    for (lcase=1, i=0; i < 4; i++)
      if (!islower(tag[i])) lcase = 0;
    if (lcase && memcmp(tag,"ftyp",4) && memcmp(tag,"tkhd",4)
	      && memcmp(tag,"mdat",4)
	|| !memcmp(tag,"CNOP",4) || !memcmp(tag,"CNTH",4)) {
      putchar ('\n');
      parse_qt (level+1, save+size);
    } else if (!memcmp(tag,"CNDA",4)) {
      puts (" *** parsing JPEG thumbnail ...");
      parse_jpeg (ftell(ifp));
    } else {
      tread (buf, 1, 40, ifp);
      printf (" : ");
      for (i=0; i < 40 && i < size-8; i++)
	putchar (isprint(buf[i]) ? buf[i] : '.');
      putchar ('\n');
    }
    tseek (ifp, save+size, SEEK_SET);
  }
}

char *memmem (char *haystack, size_t haystacklen,
              char *needle, size_t needlelen)
{
  char *c;
  for (c = haystack; c <= haystack + haystacklen - needlelen; c++)
    if (!memcmp (c, needle, needlelen))
      return c;
  return NULL;
}

/*
   Identify which camera created this file, and set global variables
   accordingly.	 Return nonzero if the file cannot be decoded.
 */
void identify()
{
  char head[32], *cp;
  unsigned hlen, fsize, toff, tlen;

  make[0] = model[0] = model2[0] = is_dng = 0;
  order = get2();
  hlen = get4();
  tseek (ifp, 0, SEEK_SET);
  tread (head, 1, 32, ifp);
  tseek (ifp, 0, SEEK_END);
  fsize = ftell(ifp);
  if ((cp = memmem (head, 32, "MMMM", 4)) ||
      (cp = memmem (head, 32, "IIII", 4))) {
    parse_phase_one (cp-head);
    if (cp-head) parse_tiff (0,0);
  } else if (order == 0x4949 || order == 0x4d4d) {
    if (!memcmp(head+6,"HEAPCCDR",8)) {
      parse_ciff (hlen, fsize - hlen, 0);
      tseek (ifp, hlen, SEEK_SET);
    } else
      parse_tiff (0,0);
  } else if (!memcmp (head,"NDF0",4)) {
    parse_tiff (12,0);
  } else if (!memcmp (head,"\0MRM",4)) {
    parse_minolta (0);
  } else if (!memcmp (head,"FUJIFILM",8)) {
    tseek (ifp, 84, SEEK_SET);
    toff = get4();
    tlen = get4();
    parse_fuji (92);
    tseek (ifp, 100, SEEK_SET);
    parse_tiff (get4(),0);
    if (toff > 120) {
      parse_fuji (120);
      tseek (ifp, 128, SEEK_SET);
      parse_tiff (get4(),0);
    }
    parse_thumb (toff,0);
  } else if (!memcmp (head,"RIFF",4)) {
    tseek (ifp, 0, SEEK_SET);
    parse_riff(0);
  } else if (!memcmp (head+4,"ftypcrx ",8)) {
    tseek (ifp, 0, SEEK_SET);
    parse_crx (0, fsize);
  } else if (!memcmp (head+4,"ftypqt   ",9)) {
    tseek (ifp, 0, SEEK_SET);
    parse_qt (0, fsize);
  } else if (!memcmp (head+4,"RED",3))
    parse_redcine(0,0);
  else if (!memcmp (head,"DSC-Image",9))
    parse_rollei();
  else if (!memcmp (head,"FOVb",4))
    parse_foveon();
  parse_jpeg(0);
}

int main(int argc, char **argv)
{
  int arg;

  if (argc == 1)
  {
    fprintf (stderr,
    "\nRaw Photo Parser"
    "\nby Dave Coffin, dcoffin a cybercom o net"
    "\n\nUsage:  %s file1.crw file2.crw ...\n", argv[0]);
    return 1;
  }
  for (arg=1; arg < argc; arg++)
  {
    fname = argv[arg];
    ifp = fopen (fname,"rb");
    if (!ifp) {
      perror (fname);
      continue;
    }
    printf ("\nParsing %s:\n", fname);
    identify();
    fclose (ifp);
  }
  return 0;
}
