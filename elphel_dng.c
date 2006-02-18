/*
   Converts Elphel camera output from Elphel-JPEG to Adobe DNG.
   gcc -o elphel_dng elphel_dng.c -O4 -Wall -lm -ljpeg -ltiff
   Requires LibTIFF 3.8.0 plus a patch.

   Written by Dave Coffin for Berkeley Engineering and Research.

   Free for all uses.

   $Revision: 1.4 $
   $Date: 2006/02/18 22:50:53 $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
  static const float neutral[] = { 0.807133, 1.0, 0.913289 };
  long sub_offset=0, white=0x3fff;
  struct jpeg_error_mgr jerr;
  struct jpeg_decompress_struct cinfo;
  JSAMPARRAY buf;
  float gam;
  int status=1, i, r, c, row, col;
  unsigned short curve[256], *out;
  struct stat st;
  struct tm tm;
  char datetime[64];
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
	((j_common_ptr) &cinfo, JPOOL_IMAGE, cinfo.image_width, 16);
  if (!(tif = TIFFOpen (argv[3], "w"))) goto fail;
  out = calloc (cinfo.image_width, sizeof *out);

  TIFFSetField (tif, TIFFTAG_SUBFILETYPE, 1);
  TIFFSetField (tif, TIFFTAG_IMAGEWIDTH, cinfo.image_width >> 4);
  TIFFSetField (tif, TIFFTAG_IMAGELENGTH, cinfo.image_height >> 4);
  TIFFSetField (tif, TIFFTAG_BITSPERSAMPLE, 8);
  TIFFSetField (tif, TIFFTAG_COMPRESSION, COMPRESSION_NONE);
  TIFFSetField (tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
  TIFFSetField (tif, TIFFTAG_MAKE, "Elphel");
  TIFFSetField (tif, TIFFTAG_MODEL, "Model 323");
  TIFFSetField (tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
  TIFFSetField (tif, TIFFTAG_SAMPLESPERPIXEL, 3);
  TIFFSetField (tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField (tif, TIFFTAG_SOFTWARE, "elphel_dng");
  TIFFSetField (tif, TIFFTAG_DATETIME, datetime);
  TIFFSetField (tif, TIFFTAG_SUBIFD, 1, &sub_offset);
  TIFFSetField (tif, TIFFTAG_DNGVERSION, "\001\001\0\0");
  TIFFSetField (tif, TIFFTAG_DNGBACKWARDVERSION, "\001\0\0\0");
  TIFFSetField (tif, TIFFTAG_UNIQUECAMERAMODEL, "Elphel Model 323");
  TIFFSetField (tif, TIFFTAG_COLORMATRIX1, 9, cam_xyz);
  TIFFSetField (tif, TIFFTAG_ASSHOTNEUTRAL, 3, neutral);
  TIFFSetField (tif, TIFFTAG_CALIBRATIONILLUMINANT1, 21);
  TIFFSetField (tif, TIFFTAG_ORIGINALRAWFILENAME, argv[2]);
  memset (buf[0], 0, cinfo.image_width);	// all-black thumbnail
  for (row=0; row < cinfo.image_height >> 4; row++)
    TIFFWriteScanline (tif, buf[0], row, 0);
  TIFFWriteDirectory (tif);

  TIFFSetField (tif, TIFFTAG_SUBFILETYPE, 0);
  TIFFSetField (tif, TIFFTAG_IMAGEWIDTH, cinfo.image_width);
  TIFFSetField (tif, TIFFTAG_IMAGELENGTH, cinfo.image_height);
  TIFFSetField (tif, TIFFTAG_BITSPERSAMPLE, 16);
  TIFFSetField (tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_CFA);
  TIFFSetField (tif, TIFFTAG_SAMPLESPERPIXEL, 1);
  TIFFSetField (tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField (tif, TIFFTAG_CFAREPEATPATTERNDIM, CFARepeatPatternDim);
  TIFFSetField (tif, TIFFTAG_CFAPATTERN, 4, "\001\0\002\001");
  TIFFSetField (tif, TIFFTAG_LINEARIZATIONTABLE, 256, curve);
  TIFFSetField (tif, TIFFTAG_WHITELEVEL, 1, &white);

  for (row=0; row < cinfo.image_height; row += 16) {
    for (r=0; r < 16; )
      r += jpeg_read_scanlines (&cinfo, buf+r, 16-r);
    for (r=0; r < 16; r++) {
      for (col=0; col < cinfo.image_width; col += 16)
	for (c=0; c < 16; c++)
	  out[col+c] = buf[ROT(r)][col+ROT(c)];
      TIFFWriteScanline (tif, out, row+r, 0);
    }
  }
  free (out);
  TIFFClose (tif);
  jpeg_finish_decompress (&cinfo);
  status = 0;
fail:
  jpeg_destroy_decompress (&cinfo);
  fclose (ifp);
  return status;
}
