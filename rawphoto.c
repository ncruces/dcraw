/*
   Raw photo loader plugin for The GIMP
   by Dave Coffin at cybercom dot net, user dcoffin
   http://www.cybercom.net/~dcoffin/

   $Revision: 1.7 $
   $Date: 2003/10/30 19:15:41 $

   This code is licensed under the same terms as The GIMP.
   To simplify maintenance, it calls my command-line "dcraw"
   program to do the actual decoding.

   To install locally:
	gimptool --install rawphoto.c

   To install globally:
	gimptool --install-admin rawphoto.c

   To build without installing:
	gcc -o rawphoto rawphoto.c `gtk-config --cflags --libs` -lgimp -lgimpui
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gtk/gtk.h>

#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>
#include <libgimp/gimpintl.h>

#define PLUG_IN_VERSION  "1.0.5 - 30 October 2003"

static void query(void);
static void run(gchar *name,
		gint nparams,
		GimpParam *param,
		gint *nreturn_vals,
		GimpParam **return_vals);

static gint  load_dialog (gchar *name);
static gint32 load_image (gchar *filename);

GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,  /* init_procedure */
  NULL,  /* quit_procedure */
  query, /* query_procedure */
  run,   /* run_procedure */
};

static struct {
  gboolean check_val[4];
  gfloat    spin_val[4];
} cfg = {
  { FALSE, FALSE, FALSE, FALSE },
  { 0.8, 1, 1, 1 }
};

MAIN ()

static void query (void)
{
  static GimpParamDef load_args[] =
  {
    { GIMP_PDB_INT32,      "run_mode",     "Interactive, non-interactive" },
    { GIMP_PDB_STRING,     "filename",     "The name of the file to load" },
    { GIMP_PDB_STRING,     "raw_filename", "The name of the file to load" },
  };
  static GimpParamDef load_return_vals[] =
  {
    { GIMP_PDB_IMAGE,      "image",        "Output image" },
  };

  static gint num_load_args =
	sizeof load_args / sizeof load_args[0];
  static gint num_load_return_vals =
	sizeof load_return_vals / sizeof load_return_vals[0];

  gimp_install_procedure ("file_rawphoto_load",
			  "Loads raw digital camera files",
			  "This plug-in loads raw digital camera files.",
			  "Dave Coffin at cybercom dot net, user dcoffin",
			  "Copyright 2003 by Dave Coffin",
			  PLUG_IN_VERSION,
			  "<Load>/rawphoto",
			  NULL,
			  GIMP_PLUGIN,
			  num_load_args,
			  num_load_return_vals,
			  load_args,
			  load_return_vals);

  gimp_register_load_handler ("file_rawphoto_load",
		"bay,bmq,crw,dcr,jpg,mrw,nef,orf,pef,raf,raw,rdc,tif,x3f", "");
}

static void run (gchar *name,
		gint nparams,
		GimpParam *param,
		gint *nreturn_vals,
		GimpParam **return_vals)
{
  static GimpParam values[2];
  GimpRunModeType run_mode;
  GimpPDBStatusType status;
  gint32 image_id = -1;
  gchar *command, *fname;
  int stat;

  *nreturn_vals = 1;
  *return_vals = values;

  status = GIMP_PDB_CALLING_ERROR;
  if (strcmp (name, "file_rawphoto_load")) goto done;

  status = GIMP_PDB_EXECUTION_ERROR;
  fname = param[1].data.d_string;
  command = g_malloc (strlen(fname)+20);
  if (!command) goto done;
/*
   Is the file really a raw photo?  If not, try loading it
   as a regular JPEG or TIFF.
 */
  sprintf (command, "dcraw -i \"%s\"\n",fname);
  fputs (command, stderr);
  stat = system (command);
  g_free (command);
  if (stat) {
    if (stat > 0x200)
      g_message (_("The \"rawphoto\" plugin won't work because "
	"there is no \"dcraw\" executable in your path."));
    if (!strcasecmp (fname + strlen(fname) - 4, ".jpg"))
      *return_vals = gimp_run_procedure2
	("file_jpeg_load", nreturn_vals, nparams, param);
    else
      *return_vals = gimp_run_procedure2
	("file_tiff_load", nreturn_vals, nparams, param);
    return;
  }
  gimp_get_data ("plug_in_rawphoto", &cfg);
  status = GIMP_PDB_CANCEL;
  run_mode = param[0].data.d_int32;
  if (run_mode == GIMP_RUN_INTERACTIVE)
    if (!load_dialog (param[1].data.d_string)) goto done;

  status = GIMP_PDB_EXECUTION_ERROR;
  image_id = load_image (param[1].data.d_string);
  if (image_id == -1) goto done;

  *nreturn_vals = 2;
  values[1].type = GIMP_PDB_IMAGE;
  values[1].data.d_image = image_id;
  status = GIMP_PDB_SUCCESS;
  gimp_set_data ("plug_in_rawphoto", &cfg, sizeof cfg);

done:
  values[0].type = GIMP_PDB_STATUS;
  values[0].data.d_status = status;
}

static gint32 load_image (gchar *filename)
{
  int		tile_height, width, height, row, nrows;
  FILE		*pfp;
  gint32	image, layer;
  GimpDrawable	*drawable;
  GimpPixelRgn	pixel_region;
  guchar	*pixel;
  char		*command, nl;

  command = g_malloc (strlen(filename)+100);
  if (!command) return -1;
  sprintf (command,
	"dcraw -c%s%s%s%s -g %0.2f -b %0.2f -r %0.2f -l %0.2f \"%s\"\n",
	cfg.check_val[0] ? " -f":"",
	cfg.check_val[1] ? " -w":"",
	cfg.check_val[2] ? " -d":"",
	cfg.check_val[3] ? " -q":"",
	cfg.spin_val[0], cfg.spin_val[1], cfg.spin_val[2], cfg.spin_val[3],
	filename );
  fputs (command, stderr);
  pfp = popen (command, "r");
  g_free (command);
  if (!pfp) {
    perror ("dcraw");
    return -1;
  }

  if (fscanf (pfp, "P6 %d %d 255%c", &width, &height, &nl) != 3) {
    pclose (pfp);
    g_message ("Not a raw digital camera image.\n");
    return -1;
  }

  image = gimp_image_new (width, height, GIMP_RGB);
  if (image == -1) {
    pclose (pfp);
    g_message ("Can't allocate new image.\n");
    return -1;
  }

  gimp_image_set_filename (image, filename);

  /* Create the "background" layer to hold the image... */
  layer = gimp_layer_new (image, "Background", width, height,
			  GIMP_RGB_IMAGE, 100, GIMP_NORMAL_MODE);
  gimp_image_add_layer (image, layer, 0);

  /* Get the drawable and set the pixel region for our load... */
  drawable = gimp_drawable_get (layer);
  gimp_pixel_rgn_init (&pixel_region, drawable, 0, 0, drawable->width,
			drawable->height, TRUE, FALSE);

  /* Temporary buffers... */
  tile_height = gimp_tile_height();
  pixel = g_new (guchar, tile_height * width * 3);

  /* Load the image... */
  for (row = 0; row < height; row += tile_height) {
    nrows = height - row;
    if (nrows > tile_height)
	nrows = tile_height;
    fread (pixel, width * 3, nrows, pfp);
    gimp_pixel_rgn_set_rect (&pixel_region, pixel, 0, row, width, nrows);
  }

  pclose (pfp);
  g_free (pixel);

  gimp_drawable_flush (drawable);
  gimp_drawable_detach (drawable);

  return image;
}

/* this is set to true after OK click in any dialog */
gboolean result = FALSE;

static void callback_ok (GtkWidget * widget, gpointer data)
{
  result = TRUE;
  gtk_widget_destroy (GTK_WIDGET (data));
}

gint load_dialog (gchar * name)
{
  GtkWidget *dialog;
  GtkWidget *table;
  GtkObject *adj;
  GtkWidget *widget;
  int i;
  static const char *label[8] =
  { "Interpolate RGB as four colors", "Use camera white balance",
    "Grayscale document mode", "Quick interpolation",
    "Gamma", "Brightness", "Red Multiplier", "Blue Multiplier" };

  gimp_ui_init ("rawphoto", TRUE);

  dialog = gimp_dialog_new (_("Raw Photo Loader 1.0"), "rawphoto",
			gimp_standard_help_func, "",
			GTK_WIN_POS_MOUSE,
			FALSE, TRUE, FALSE,
			_("OK"), callback_ok, NULL, NULL, NULL, TRUE,
			FALSE, _("Cancel"), gtk_widget_destroy, NULL,
			1, NULL, FALSE, TRUE, NULL);
  gtk_signal_connect
	(GTK_OBJECT(dialog), "destroy", GTK_SIGNAL_FUNC(gtk_main_quit), NULL);

  table = gtk_table_new (8, 2, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER(table), 6);
  gtk_box_pack_start
	(GTK_BOX(GTK_DIALOG(dialog)->vbox), table, FALSE, FALSE, 0);
  gtk_widget_show (table);

  for (i=0; i < 4; i++) {
    widget = gtk_check_button_new_with_label
	(_(label[i]));
    gtk_toggle_button_set_active
	(GTK_TOGGLE_BUTTON (widget), cfg.check_val[i]);
    gtk_table_attach
	(GTK_TABLE(table), widget, 0, 2, i, i+1, GTK_FILL, GTK_FILL, 0, 0);
    gtk_signal_connect (GTK_OBJECT (widget), "toggled",
			GTK_SIGNAL_FUNC (gimp_toggle_button_update),
			&cfg.check_val[i]);
    gtk_widget_show (widget);
  }

  for (i=4; i < 8; i++) {
    widget = gtk_label_new (_(label[i]));
    gtk_misc_set_alignment (GTK_MISC (widget), 1.0, 0.5);
    gtk_misc_set_padding   (GTK_MISC (widget), 10, 0);
    gtk_table_attach
	(GTK_TABLE(table), widget, 0, 1, i, i+1, GTK_FILL, GTK_FILL, 0, 0);
    gtk_widget_show (widget);
    widget = gimp_spin_button_new
	(&adj, cfg.spin_val[i-4], 0.01, 4.0, 0.01, 0.1, 0.1, 0.1, 2);
    gtk_table_attach
	(GTK_TABLE(table), widget, 1, 2, i, i+1, GTK_FILL, GTK_FILL, 0, 0);
    gtk_signal_connect (GTK_OBJECT (adj), "value_changed",
			GTK_SIGNAL_FUNC (gimp_float_adjustment_update),
			&cfg.spin_val[i-4]);
    gtk_widget_show (widget);
  }

  gtk_widget_show (dialog);

  gtk_main();
  gdk_flush();

  return result;
}
