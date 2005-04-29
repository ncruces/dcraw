/*
   fujiturn.c by Dave Coffin

   UNIX filter to correct the 45-degree rotation in images from
   Fuji digital cameras.  Compile with -D_16BIT to rotate 48-bit
   PPM images.  Sample usage:

   dcraw -c -j dscf0000.raf | fujiturn | pnmscale 0.70710678 > dscf0000.ppm

   $Revision: 1.6 $
   $Date: 2005/04/29 16:35:42 $

 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#ifdef _16BIT
typedef unsigned short value;
#else
typedef unsigned char value;
#define ntohs(x) (x)
#define htons(x) (x)
#endif

void merror (void *ptr, char *what)
{
  if (ptr) return;
  fprintf (stderr, "Not enough memory for %s\n", what);
  exit(1);
}

int main()
{
  FILE *ifp, *ofp;
  value (*in)[3], (*mid)[3], (*pix)[3], (*out)[3];
  char nl;
  int maxval, i, j, iwide, ihigh, owide, ohigh;
  unsigned irow, icol, orow, ocol;

#if defined(WIN32) || defined(DJGPP)
  if (setmode(0,O_BINARY) < 0) perror("setmode(0)");
  if (setmode(1,O_BINARY) < 0) perror("setmode(1)");
#endif
  ifp = stdin;
  ofp = stdout;
  if (fscanf (ifp, "P6 %d %d %d%c", &iwide, &ihigh, &maxval, &nl) != 4
	|| abs(iwide - ihigh) > 1) {
    fprintf (stderr, "Input is not a Fuji image processed by dcraw.\n");
    exit(1);
  }
  i = (maxval > 255) ? 16 : 8;
  j = 8 * sizeof (value);
  if (i != j) {
    fprintf (stderr, "Input is %d-bit, fujiturn is %d-bit\n", i, j);
    exit(1);
  }
  in = calloc (iwide, sizeof *in);
  merror (in, "input array");
  fread (in, iwide, sizeof *in, ifp);
  for (i = 0; i < iwide; i++)
    if (in[i][0] || in[i][1] || in[i][2]) break;
  ohigh = (iwide - i) * 2 - 4;
  for (i = iwide; --i;)
    if (in[i][0] || in[i][1] || in[i][2]) break;
  owide = i;
  mid = calloc (ohigh * owide, sizeof *mid);
  merror (mid, "middle array");
  for (irow = 0; irow < ihigh; irow++) {
    for (icol = 0; icol < iwide; icol++) {
      orow =  irow + icol - owide + 5;
      ocol = (icol - irow + owide - 1)/2;
      if (orow < ohigh && ocol < owide)
	for (i = 0; i < 3; i++)
	  mid[orow*owide+ocol][i] = ntohs(in[icol][i]);
    }
    fread (in, iwide, sizeof *in, ifp);
  }
  free(in);
  out = calloc (2*owide, sizeof *out);
  merror (out, "output array");
  fprintf (ofp, "P6\n%d %d\n%d\n", owide*2, ohigh, maxval);
  for (orow = 0; orow < ohigh; orow++) {
    for (ocol = 0; ocol < owide*2; ocol++) {
      pix = mid + orow*owide + ocol/2;
      if ((orow+ocol) & 1) {
        if (orow-1 < ohigh-2 && ocol-1 < owide*2-2)
	  for (i = 0; i < 3; i++)
	    out[ocol][i] = htons (
	    ( pix[-owide][i] + pix[0-(orow&1)][i] +
	      pix[ owide][i] + pix[1-(orow&1)][i] ) >> 2);
      } else
	for (i = 0; i < 3; i++)
	  out[ocol][i] = htons(pix[0][i]);
    }
    fwrite (out, 2*owide, 3*sizeof (value), ofp);
  }
  free(mid);
  free(out);
  return 0;
}
