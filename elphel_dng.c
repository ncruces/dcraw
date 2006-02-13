/*
   Converts Elphel camera output from Elphel-JPEG to Adobe DNG.
   gcc -o elphel_dng elphel_dng.c -O4 -Wall -lm -ljpeg -ltiff
   Requires LibTIFF 3.8.0 plus a patch.

   Written by Dave Coffin for Berkeley Engineering and Research.

   Free for all uses.

   $Revision: 1.1 $
   $Date: 2006/02/13 23:25:39 $
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <time.h>
#include <math.h>
#include <jpeglib.h>
#include <tiffio.h>

#ifdef MODE1
#define ROT(x) (x)
#else
#define ROT(x) ((x) >> 1 | ((x) & 1) << 3)
#endif

int main (int argc, char **argv)
{
  static const short CFARepeatPatternDim[] = { 2,2 };
  static const float cam_xyz[] =
  { 2.005,-0.771,-0.269, -0.752,1.688,0.064, -0.149,0.283,0.745 };
  struct jpeg_error_mgr jerr;
  struct jpeg_decompress_struct cinfo;
  float gam;
  int status=1, i, r, c, row, col;
  unsigned short curve[256];
  struct stat st;
  struct tm tm;
  char datetime[64];
  JSAMPARRAY buf;
  FILE *ifp;
  TIFF *tif;

  if (argc != 4) {
    fprintf (stderr, "Usage: %s gamma infile outfile\n"
	"Example: %s 100 cgi.jpg output.dng\n", argv[0],argv[0]);
    return 1;
  }
  if ((gam = atof(argv[1])) <= 0) {
    fprintf (stderr, "Gamma must be positive!\n");
    return 1;
  }
  for (i=0; i < 256; i++)
    curve[i] = 0x3fff * pow (i/255.0, 100/gam) + 0.5;

  if (!(ifp = fopen (argv[2], "rb"))) {
    perror (argv[2]);
    return 1;
  }
  stat (argv[2], &st);
  gmtime_r (&st.st_mtime, &tm);
  sprintf (datetime, "%04d:%02d:%02d %02d:%02d:%02d",
	tm.tm_year+1900,tm.tm_mon+1,tm.tm_mday,tm.tm_hour,tm.tm_min,tm.tm_sec);

  cinfo.err = jpeg_std_error (&jerr);
  jpeg_create_decompress (&cinfo);
  jpeg_stdio_src (&cinfo, ifp);
  jpeg_read_header (&cinfo, TRUE);
  if ((cinfo.image_width | cinfo.image_height) & 15) {
    fprintf (stderr, "Dimensions must be multiples of 16!\n");
    goto fail;
  }
  cinfo.dct_method = JDCT_FLOAT;
  cinfo.out_color_space = JCS_GRAYSCALE;
  jpeg_start_decompress (&cinfo);
  buf = (*cinfo.mem->alloc_sarray)
	((j_common_ptr) &cinfo, JPOOL_IMAGE, cinfo.image_width, 17);
  if (!(tif = TIFFOpen (argv[3], "w"))) goto fail;

  TIFFSetField (tif, TIFFTAG_IMAGEWIDTH, cinfo.image_width);
  TIFFSetField (tif, TIFFTAG_IMAGELENGTH, cinfo.image_height);
  TIFFSetField (tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
  TIFFSetField (tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField (tif, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField (tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField (tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
  TIFFSetField (tif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  TIFFSetField (tif, TIFFTAG_MAKE, "Elphel");
  TIFFSetField (tif, TIFFTAG_MODEL, "Model 323");
  TIFFSetField (tif, TIFFTAG_SOFTWARE, "elphel_dng");
  TIFFSetField (tif, TIFFTAG_DATETIME, datetime);
  TIFFSetField (tif, TIFFTAG_CFAREPEATPATTERNDIM, CFARepeatPatternDim);
  TIFFSetField (tif, TIFFTAG_CFAPATTERN, 4, "\001\0\002\001");
  TIFFSetField (tif, TIFFTAG_DNGVERSION, "\001\001\0\0");
  TIFFSetField (tif, TIFFTAG_DNGBACKWARDVERSION, "\001\0\0\0");
  TIFFSetField (tif, TIFFTAG_CALIBRATIONILLUMINANT1, 15);
  TIFFSetField (tif, TIFFTAG_COLORMATRIX1, 9, cam_xyz);
  TIFFSetField (tif, TIFFTAG_ORIGINALRAWFILENAME, argv[2]);
  if (gam != 100)
    TIFFSetField (tif, TIFFTAG_LINEARIZATIONTABLE, 256, curve);

  for (row=0; row < cinfo.image_height; row += 16) {
    for (r=0; r < 16; )
      r += jpeg_read_scanlines (&cinfo, buf+r, 16-r);
    for (r=0; r < 16; r++) {
      for (col=0; col < cinfo.image_width; col += 16)
	for (c=0; c < 16; c++)
	  buf[16][col+c] = buf[ROT(r)][col+ROT(c)];
      TIFFWriteScanline (tif, buf[16], row+r, 0);
    }
  }
  TIFFClose (tif);
  jpeg_finish_decompress (&cinfo);
  status = 0;
fail:
  jpeg_destroy_decompress (&cinfo);
  fclose (ifp);
  return status;
}
