/*
   Completely decrypt a Sony DSC-F828 raw file.
   by Dave Coffin  1/4/2004
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netinet/in.h>

typedef unsigned char uchar;

void sony_decrypt (void *buf, int len, int key)
{
  unsigned pad[128], *data=buf;
  int i;

  for (i=0; i < 4; i++)
    pad[i] = key = key * 48828125 + 1;
  pad[3] = pad[3] << 1 | (pad[0]^pad[2]) >> 31;
  for (i=4; i < 127; i++)
    pad[i] = (pad[i-4]^pad[i-2]) << 1 | (pad[i-3]^pad[i-1]) >> 31;
  for (i=0; i < 127; i++)
    pad[i] = htonl(pad[i]);
  for ( ; i < len+127; i++,data++)
    *data ^= pad[i & 127] = pad[(i+1) & 127] ^ pad[(i+65) & 127];
}

void sony_clear (uchar *buffer, int length)
{
  unsigned *ip, key0, key1=0, key2=0, i;
  uchar *cp;

  ip = (void *) cp = buffer+200896;
  key0 = ntohl(ip[*cp]);
  sony_decrypt (buffer+164600, 9074, key0);
  for (i=4; i--; ) {
    key1 = key1 << 8 | buffer[164610+i];
    key2 = key2 << 8 | buffer[164622+i];
  }
  sony_decrypt (buffer+164640, 174376, key1);
  sony_decrypt (buffer+862144, (length-862144)/4, key2);
}

int main (int argc, char **argv)
{
  FILE *fp;
  char name[512], *buffer;
  int arg, length;

  for (arg=1; arg < argc; arg++) {
    fp = fopen (argv[arg], "rb");
    if (!fp) {
      perror (argv[arg]);
      continue;
    }
    fseek (fp, 0, SEEK_END);
    length = ftell (fp);
    if (length < 0x100000) {
      fprintf (stderr, "%s is too small!\n", argv[arg]);
      fclose(fp);
      continue;
    }
    buffer = malloc (length);
    if (!buffer) {
      fprintf (stderr, "%s is too big!\n", argv[arg]);
      fclose(fp);
      continue;
    }
    fseek (fp, 0, SEEK_SET);
    fread (buffer, 1, length, fp);
    fclose (fp);
    sony_clear (buffer, length);
    strcpy (name, argv[arg]);
    strcat (name, ".clear");
    fp = fopen (name, "wb");
    fwrite (buffer, 1, length, fp);
    free (buffer);
    fclose(fp);
  }
  return 0;
}
