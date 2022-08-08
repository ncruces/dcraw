Mirror of Dave Coffin's dcraw
=============================

This repository contains an unofficial mirror of Dave Coffin's dcraw with full version history.

Branches:
- **master**: Dave Coffin's code, this [README.md](README.md), and [LICENSE](LICENSE). No other changes.
- **debian**: adds Debian testing patches.
- **ncruces-dev**: improves DNG support and avoids creating temporary files.
- **ncruces-win**: adds various hacks to make unicode paths work on Windows.
- **ncruces-wasm**: adds various hacks to run on WASI-compliant runtimes.

The rest of this [README.md](README.md) is a markdown translation of Dave Coffin's dcraw [website](https://dechifro.org/dcraw/).


Decoding raw digital photos in Linux
====================================

Welcome! If you are wondering how to connect your digital camera and download images to a Linux PC, go to the [gPhoto homepage](http://gphoto.sourceforge.net/). My software is for processing those images _after_ downloading them.

If you're downloading JPEG files, you don't need my software at all. The image has already been processed inside the camera. Almost all digital cameras made since 1997 produce JPEG images, so why would you want to do it any other way?

Well, despite the convenience and ubiquity of JPEG, there are some disadvantages. JPEG is a lossy format -- to fit a big image into a small file, a lot of information is thrown away. That's why midrange and high-end digital cameras offer an alternative: Raw, unprocessed CCD data files, for which the camera manufacturer provides special decoding software.

Of course this software is for Windows and Macintosh only, with no source code. So it's useless to users of other operating systems, programmers hoping to design a better interpolation algorithm, and historians not yet born in an era when the only Windows machines will be in museums.

So here is my mission: Write and maintain an ANSI C program that decodes any raw image from any digital camera on any computer running any operating system.

That program is called [dcraw](dcraw.c) (pronounced "dee-see-raw"), and it's become a standard tool within and without the Open Source world. It's small (about 9000 lines), portable (standard C libraries only), free (both "gratis" and "libre"), and when used skillfully, produces better quality output than the tools provided by the camera vendor.

[Here's my resume](https://dechifro.org/resume.html). I do freelance consulting related to dcraw, and I'm also available for full-time software work in the Northeast USA.

I can be reached by sending e-mail to cybercom dot net with the username "dcoffin".

### News and Interviews

[Essay for Digital Outback Photo, 25 April 2003](http://www.outbackphoto.com/artofraw/raw_07/essay.html)  
[Article in News.com, 21 April 2005](http://news.cnet.com/Nikons-photo-encryption-reported-broken/2100-1030_3-5679848.html)  
[Interview with Digital Photography Review, 27 April 2005](http://www.dpreview.com/news/0504/05042701davecoffininterview.asp)  
[Dcraw mentioned in Editors Guild magazine, July/August 2005](http://www.editorsguild.com/v2/magazine/archives/0705/cover_story.htm)  
[Interview with Thorsten Schnebeck, 10 June 2006](http://archive.is/0FyJw)  
[Interview with Ladinamo, 16 June 2006](https://web.archive.org/web/20121015035554/http://www.ladinamo.org/english/raw-format-the-captive-photo.php)

### My Code

Unless otherwise noted in the source code, these programs are free for all uses, although I would like to receive credit for them. Donations are welcome too, if you're making money from my code.  

[![Donate](https://www.paypal.com/en_US/i/btn/btn_donate_SM.gif)](https://www.paypal.com/donate/?business=dcoffin@cybercom.net&item_name=Support%20for%20dcraw)

**Note to Linux distributors:** The only executable files that should be installed by a dcraw package are "dcraw", "clean\_crw", and maybe "fuji\_green", "fujiturn", and "fujiturn16". My shell scripts are dangerous and should only be installed in a "doc" directory without execute permission.

*   [dcraw.c -- decodes raw photos, extracts thumbnails, and displays metadata](dcraw.c)  
    Supports 731 cameras at last count. Compile with "**gcc -o dcraw -O4 dcraw.c -lm -ljasper -ljpeg -llcms2**" or "**gcc -o dcraw -O4 dcraw.c -lm -DNODEPS**". Run with no arguments to see a usage message. Don't complain that 16-bit output is too dark -- read the [FAQ](#faq)!
*   [UNIX manpage for dcraw](https://dechifro.org/dcraw/dcraw.1.html)  
    This is dcraw's official user documentation, updated in lockstep with the source code.
*   [rawphoto.c -- basic plugin for GIMP 1.2 & 2.0](rawphoto.c)  
    After installing "**dcraw**", do "**gimptool --install rawphoto.c**". My plugin provides a simple dialog box for loading raw files into the Gimp. [Udi Fuchs](http://ufraw.sourceforge.net/) and [Joseph Heled](http://homepages.ihug.co.nz/~peps/) have written much nicer plugins, with live preview, histograms, and color curves.
*   [.badpixels -- my camera's "hot pixels"](.badpixels)  
    This file tells dcraw which pixels have died and when, so that it can interpolate around them.
*   [dcraw.c,v -- complete unabridged RCS file](https://dechifro.org/dcraw/RCS/dcraw.c,v)  
    This file contains the entire history of dcraw.c since its conception on February 23, 1997. If you don't have the RCS toolkit, [download it here](http://www.cs.purdue.edu/homes/trinkle/RCS/).
*   [parse.c -- read image data structures](parse.c)  
    This program displays CIFF and TIFF data structures in a very cryptic format.
*   [clean\_crw.c -- clean Canon CRW files](clean_crw.c)  
    Recovered or undeleted CRW files often have junk appended to them that makes them unreadable. This program safely cleans CRW files.
*   [fujiturn.c -- rotate Fuji Super CCD images](fujiturn.c)  
    An alternative to dcraw's built-in Fuji rotation.
*   [fuji\_green.c -- convert Fuji green pixels to PGM](fuji_green.c)  
    A side benefit of the Fuji Super CCD design is that its green pixels make nice greyscale images.

For hackers only:

*   [decompress.c](decompress.c) is a simple reference decompressor for CRW files.
*   [sony\_clear.c](sony_clear.c) decrypts SRF files from the Sony DSC-F828.

### Internationalization

To build and install multilingual dcraw in Linux, download the latest tarball [from this directory](https://dechifro.org/dcraw/archive/) and run the "install" script. The currently supported languages are [Esperanto](https://dechifro.org/dcraw/dcraw_eo.1.html), [Russian](https://dechifro.org/dcraw/dcraw_ru.1.html), [French](https://dechifro.org/dcraw/dcraw_fr.1.html), [Italian](https://dechifro.org/dcraw/dcraw_it.1.html), [German](https://dechifro.org/dcraw/dcraw_de.1.html), [Portuguese](https://dechifro.org/dcraw/dcraw_pt.1.html), [Spanish](https://dechifro.org/dcraw/dcraw_es.1.html), [Dutch](https://dechifro.org/dcraw/dcraw_nl.1.html), [Polish](https://dechifro.org/dcraw/dcraw_pl.1.html), [Hungarian](https://dechifro.org/dcraw/dcraw_hu.1.html), [Czech](https://dechifro.org/dcraw/dcraw_cs.1.html), [Swedish](https://dechifro.org/dcraw/dcraw_sv.1.html), [Catalan](https://dechifro.org/dcraw/dcraw_ca.1.html), [Danish](https://dechifro.org/dcraw/dcraw_da.1.html), [Romanian](https://dechifro.org/dcraw/dcraw_ro.1.html), [Japanese](https://dechifro.org/dcraw/dcraw_ja.1.html), and Chinese (both [Traditional](https://dechifro.org/dcraw/dcraw_zh_TW.1.html) and [Simplified](https://dechifro.org/dcraw/dcraw_zh_CN.1.html)).

To build a unilingual, self-contained DCRAW.EXE for DOS/Windows, use a source file [from this directory](https://dechifro.org/dcraw/msdos/) instead.

To add another language, send me translations of [this manpage](dcraw.1) and [this message table](dcraw_eo.po) in UTF-8 encoding. Translate only from my original English and Esperanto texts -- other languages may contain factual errors invisible to me.

Do not translate "Cannot do X" as "It is impossible to do X". Dcraw is not perfect, so if it cannot do something, that does not mean that the task is impossible. Computers must never use the pronoun "I", so write "dcraw cannot do X".

When in doubt, translate everything. I proofread these texts before releasing them, and it's much easier for me to correct over-translation than under-translation.

Dcraw decodes raw _photos_, not raw _files_. No digital camera generates raw files in normal usage, there's always a header with useful metadata. (For abnormal usage, see CHDK and DIAG RAW below)

"raw" is an English word, not an acronym or file format. "raw photo" should be translated with the same adjective that you would use for "crude oil" or "raw materials".

There are dozens of raw photo formats: CRW, CR2, MRW, NEF, RAF, etc. "RAW Format" does not exist; it is an illusion created by dcraw's ability to read all raw formats.

### Other Raw Photo Decoders

Dcraw has made it far easier for developers to support a wide range of digital cameras in their applications. They can call dcraw from a graphical interface, paste pieces of dcraw.c into their code, or just use dcraw.c as the documentation that camera makers refuse to provide:

*   [ACDSee](http://www.acdsystems.com/)
*   [Adobe Photoshop](http://www.adobe.com/products/photoshop/cameraraw.html)
*   [BR's PhotoArchiver by Baard Riiber](http://www.br-software.com/)
*   [BreezeBrowser by Chris Breeze](http://www.breezesys.com/)
*   [Conceiva Lightbox](http://www.conceiva.com/)
*   [cPicture by Jürgen Eidt](http://cpicture.net/en/t_raw.html)
*   [Cumulus by Canto](http://www.canto.com/)
*   [dcRAW-X by Bryan Chang](http://frostyplace.com/dcraw/)
*   [DCRawUI by Sune Trudslev](http://www.tanis.dk/wiki/index.php/DCRawUI)
*   [DDRoom by Mykhailo Malyshko](https://github.com/ddroom/DDRoom/releases)
*   [Directory Opus Plugin by Leo Davidson](http://www.pretentiousname.com/jp2raw/)(with C++ source code)
*   [DeepSkyStacker by Luc Coiffier](http://deepskystacker.free.fr/)
*   [DF Studio by DigitalFusion](http://www.dfstudio.com/)
*   [dpMagic by Mikhail Stolpner](http://www.dpmagic.com/)
*   [GraphicConverter by Thorsten Lemke](http://www.lemkesoft.com/)
*   [GTKRawGallery by Daniele Isca](http://sourceforge.net/projects/gtkrawgallery/)
*   [GVBox from JCO Consulting](http://jcoconsulting.com/index.asp?Section=GVOCX)
*   [HDR Shop](http://www.hdrshop.com/)
*   [ImageLab from Aragon System](http://www.aragonsystem.com/)
*   [Imagina by Rob Baker](http://www.planetimagina.com/)
*   [IrfanView by Irfan Skiljan](http://www.irfanview.com/)
*   [IRIS image processor for astronomers](http://www.astrosurf.com/buil/us/iris/iris.htm)
*   [Lightbox by Josh Anon](http://www.lightboxsoftware.com/)
*   [LightZone by LightZone project](http://lightzoneproject.org/)
*   [LRViewer by Marc Rochkind](http://imageingester.com/)
*   [MediaRECOVER File Recovery Software](http://www.mediarecover.com/)
*   [Mixpo by Mixpo Portfolio Broadcasting Inc.](http://www.mixpo.com/)
*   [Photo Acute by Almalence](http://www.photoacute.com/)
*   [Photo Companion by Jeff Moore](http://www.wildcape.com/)
*   [Photo Jockey by Davie Lee Reed](http://photojockey.com/) who also wrote a [dcraw interface for Delphi programmers](http://smatters.com/dcraw/).
*   [Photo Organizer by Balint Kis](http://www.k-i-s.net/)
*   [PhotoRescue from DataRescue](http://www.datarescue.com/photorescue/)
*   [PhotoReviewer by Ben Haller](http://www.sticksoftware.com/software/PhotoReviewer.html)
*   [Photovault by Harri Kaimio](http://www.photovault.org/)
*   [Picasa from Google](http://www.picasa.com/)
*   [Picture Arena by Felix Schwarz](http://www.picturearena.com/)
*   [PixInsight by Pleiades Software](http://pleiades-astrophoto.com/)
*   [PolyView by Polybytes](http://www.polybytes.com/)
*   [PRISM by Alcor System](http://www.prism-america.com/)
*   [RAW Developer by Iridient Digital](http://www.iridientdigital.com/)
*   [Raw Magick](http://www.rawmagick.com/)
*   [RawDrop by Frank Siegert](http://www.wizards.de/rawdrop)
*   [RawTherapee by RT Team](http://www.rawtherapee.com/)
*   [Serif PhotoPlus, PanoramaPlus, and AlbumPlus](http://www.serif.com/)
*   [SharpRaw by Duane DeSieno](http://www.logicaldesigns.com/)
*   [SilverFast DCPro by LaserSoft Imaging](http://www.silverfast.com/)
*   [StudioLine Photo by H&M Software](http://www.studioline.net/)
*   [ViewIt by Zdzislaw Losvik](http://www.hexcat.com/viewit/)
*   [Viewer n5 by Dmitry Fedorov](http://www.dimin.net/software/viewer/)
*   [VueScan by Ed Hamrick](http://www.hamrick.com/)
*   [Xara Xtreme Pro](http://www.xara.com/products/xtreme/)

### Frequently Asked Questions

**I don't have a C compiler. Could you send me an executable?**

Sergio Namias has built [some current Windows EXE files here](http://www.centrostudiprogressofotografico.it/en/dcraw/). Dcraw has also been ported to [Amiga](http://os4depot.net/index.php?function=showfile&file=graphics/convert/dcraw.lha), [MorphOS](http://morphos-files.ppa.pl/find.php?find=dcraw), [BeOS](http://www.pidcock.co.uk/beos/index.html), [OS/2](http://hobbes.nmsu.edu/h-search.php?key=dcraw), and [RISC OS](http://www.riscos.info/packages/GraphicsDetails.html).

**Why does dcraw say "Out of memory" in Windows Vista?**

Ostensibly to stop memory leaks, Microsoft decided that programs using the old MS-DOS API, including anything compiled with [DJGPP](http://www.delorie.com/djgpp/), shall be confined to 32MB of memory. This limitation can be removed by some combination of service packs and registry hacks, or you can compile dcraw to use the newer Win32 API. Thomas Nicely (of Pentium FDIV fame) has a [page describing the problem and various workarounds](http://www.trnicely.net/misc/vista.html).

**How can I read the EXIF data (shutter speed, aperture, etc.)?**

[Phil Harvey's ExifTool](http://www.sno.phy.queensu.ca/~phil/exiftool/) provides a unified Perl-based EXIF reader (and editor!) for all cameras and file formats. "dcraw -i -v" is much faster, but provides less information.

**How can I read NEF files from Nikon scanners?**

Dcraw only supports cameras. Try [this simple program](scan.c) for scanners.

**How can I read Nikon Dust Off images (NDF files)?**

[Use this program](read_ndf.c).

**Do you have any specifications describing raw photo formats?**

Yes, but they tend to omit important details, like how to decompress the raw image or decrypt private metadata. See the [TIFF spec](http://partners.adobe.com/asn/developer/PDFS/TN/TIFF6.pdf), the [TIFF/EP spec](https://dechifro.org/N4378.pdf), the [Adobe DNG spec](http://www.adobe.com/products/dng/pdfs/dng_spec.pdf), the [CIFF (CRW) spec](http://xyrion.org/ciff/), and the [X3F spec](http://web.archive.org/web/20070317042320/http://www.x3f.info/technotes/FileDocs/X3F_Format.pdf).

**Where can I get an assortment of raw photos to test my software?**

For the latest cameras, I get samples from [Photography Blog](http://www.photographyblog.com/). A "Full Review" at [Imaging Resource](http://www.imaging-resource.com/MFR1.HTM) usually includes a few raw shots. [www.rawsamples.ch](http://www.rawsamples.ch/) is no longer updated, but it has samples from older cameras. For $800, I sell a complete test suite on six DVDs containing every camera supported by dcraw, and provide web-based updates for $300/year.

**I'm designing a digital camera. How do I convert its raw photos into something that dcraw and Adobe Photoshop can open?**

Download [LibTIFF v3.8.2](http://dl.maptools.org/dl/libtiff/tiff-3.8.2.tar.gz) and apply [this patch](libtiff.patch). Then use [this C program](elphel_dng.c) as a template for converting your photos to valid [Adobe DNG](http://www.adobe.com/products/dng/main.html) files.

**Why are dcraw output images larger than camera JPEGs?**

Any algorithm that combines each pixel with its neighbors is going to have problems near the edges. C code is cheap, so dcraw applies a different algorithm to edge pixels. Hardware logic is expensive, so cameras crop off the edge pixels after processing.

**I shot a raw photo with no light. Why does it appear all noisy, when it should be solid black?**

No matter how dark an image is, dcraw's auto-exposure stretches it so that one percent of its pixels appear white. The "-W" option avoids this behavior.

**I bracket plus/minus two stops, but all five shots look almost the same in dcraw. Why?**

See the previous question.

**Why is 16-bit output dark / unreadable?**

If you want pretty pictures straight out of dcraw, stay with 8-bit output. 16-bit linear output is the best raw material for professional image editors such as [Photoshop](http://www.adobe.com/products/photoshop/main.html) and [CinePaint](http://cinepaint.sourceforge.net/), but it's no good for most image viewers.

**What does the "-f" (four color RGB) option do?**

If you see patterns like [this](ahd_maze.png) or [this](vng_grid.png) in your output images, first try "dcraw -a". If these patterns persist, use "dcraw -f" to get rid of them.

**Could you please add an option for TIFF / FITS / PNG / BMP / JPEG output?**

In versions 8.25 and later, "dcraw -T" writes TIFF output with metadata. To write other formats:

dcraw -c crw\_0001.crw | pnmtofits > crw\_0001.fits
dcraw -c crw\_0001.crw | pnmtopng > crw\_0001.png
dcraw -c crw\_0001.crw | ppmtobmp > crw\_0001.bmp
dcraw -c crw\_0001.crw | cjpeg > crw\_0001.jpeg

I used the [Netpbm toolkit](http://netpbm.sourceforge.net/) in these examples. [ImageMagick](http://www.imagemagick.org/) also does command-line format conversions. Both are free.

**Why don't you implement dcraw as a library?**

I have decided that dcraw shall be a command-line program written in C, and that any further abstraction layers must be added around this core, not inside it.

Library code is ugly because it cannot use global variables. Libraries are more difficult to modify, build, install, and test than standalone programs, and so are inappropriate for file formats that change every day.

There's a simpler way to make dcraw modular and thread-safe: Run it as a separate process. Eric Raymond [explains this technique here](http://www.faqs.org/docs/artu/multiprogramchapter.html).

**Why are there false colors along edges within the image?**

Because of interpolation. This is a hard problem, easily defined:

1.  Take a three-color RGB image. At each pixel, set two color values to zero.
2.  Reconstruct the original three-color image as best you can from the remaining one color per pixel.

Dcraw currently gives a choice of four methods: Bilinear, Variable Number of Gradients (VNG), Patterned Pixel Grouping (PPG), and Adaptive Homogeneity-Directed (AHD).

[The Foveon X3 Capture chip](http://www.dpreview.com/news/0202/02021101foveonx3.asp) requires a different kind of interpolation. Unlike CCD arrays, it captures three colors at every pixel location. But the colors are not well separated, so the raw data looks very gray. Much processing is needed to enhance color while suppressing noise.

**How do I get my camera to take raw photos?**

For Canon PowerShots that don't output CRW or CR2, you need the [CHDK hack](http://digicanon.narod.ru/).  
For some Nikon Coolpix cameras, you need to enable a [special "DIAG RAW" mode](http://e2500.narod.ru/raw_format_e.htm).  
For Casio cameras, see [Maurice Delaney's website](http://www.inweb.ch/foto/rawformat.html) or read [this discussion on dpreview](http://forums.dpreview.com/forums/read.asp?forum=1015&message=4961779).  
For the Minolta DiMAGE G400, G500, G530, or G600, go [here (in Russian)](http://myfototest.narod.ru/) or [here (in English)](http://forums.dpreview.com/forums/read.asp?forum=1024&message=11773287).  
For the Minolta DiMAGE Z2 and Nikon Coolpix 2100/3100/3700, [go here](http://tester13.nm.ru/nikon/).  
For SMaL cameras, see the [camerahacking Forum](http://camerahacks.10.forumer.com/).  
For Agfa and Samsung cameras, [go here](http://forums.dpreview.com/forums/read.asp?forum=1001&message=28484239).

For other cameras, refer to the User's Manual.

**Does dcraw work with my camera?**

Most likely, yes. If your camera is not on the Supported list, try dcraw anyway. If it doesn't work perfectly, don't just sit quietly waiting for my next version. Ask me if I need any raw photos, or go ahead and post a photo to your website and e-mail me the URL. If you don't have a website, use [YouSendIt](http://yousendit.com/), [RapidShare](http://rapidshare.com/), [Sendshack](http://sendshack.com/), [ShareFile](http://www.sharefile.com/), [sendspace](http://www.sendspace.com/), or [Megaupload](http://www.megaupload.com/).

Before choosing a photo to send, read the next question:

**Why does dcraw output have a green tint and weak color?**

Because dcraw doesn't have a color matrix for your camera model, it outputs raw color instead of sRGB. To fix this, I need a photo of a [Wolf Faust](http://www.targets.coloraid.de/), [ColorChecker](http://xritephoto.com/ph_product_overview.aspx?ID=1192), [CMP](http://www.cmp-color.fr/eng%20digital%20target.html), or other calibrated color chart. Follow this checklist:

*   Use a real chart, not a printout or screen image.
*   Wait for sunny weather, local noon ± two hours.
*   Carefully brush any dust off the chart.
*   Tilt the chart 90° to the camera and 45° to the sun to avoid specular reflections.
*   Set the camera two meters away and use telephoto zoom.
*   Use the lowest ISO setting.

### Supported Cameras

*   [Adobe Digital Negative (DNG)](http://www.adobe.com/products/dng/)
*   AgfaPhoto DC-833m
*   Alcatel 5035D
*   Apple QuickTake 100
*   Apple QuickTake 150
*   Apple QuickTake 200
*   [ARRIRAW format](http://www.arri.com/DE/camera/alexa/learn/arriraw_faq/)
*   AVT F-080C
*   AVT F-145C
*   AVT F-201C
*   AVT F-510C
*   AVT F-810C
*   Baumer TXG14
*   Blackmagic URSA
*   Canon PowerShot 600
*   Canon PowerShot A5
*   Canon PowerShot A5 Zoom
*   Canon PowerShot A50
*   Canon PowerShot A460 (CHDK hack)
*   Canon PowerShot A470 (CHDK hack)
*   Canon PowerShot A530 (CHDK hack)
*   Canon PowerShot A570 (CHDK hack)
*   Canon PowerShot A590 (CHDK hack)
*   Canon PowerShot A610 (CHDK hack)
*   Canon PowerShot A620 (CHDK hack)
*   Canon PowerShot A630 (CHDK hack)
*   Canon PowerShot A640 (CHDK hack)
*   Canon PowerShot A650 (CHDK hack)
*   Canon PowerShot A710 IS (CHDK hack)
*   Canon PowerShot A720 IS (CHDK hack)
*   Canon PowerShot A3300 IS (CHDK hack)
*   Canon PowerShot Pro70
*   Canon PowerShot Pro90 IS
*   Canon PowerShot Pro1
*   Canon PowerShot G1
*   Canon PowerShot G1 X
*   Canon PowerShot G1 X Mark II
*   Canon PowerShot G2
*   Canon PowerShot G3
*   Canon PowerShot G3 X
*   Canon PowerShot G5
*   Canon PowerShot G5 X
*   Canon PowerShot G6
*   Canon PowerShot G7 (CHDK hack)
*   Canon PowerShot G7 X
*   Canon PowerShot G7 X Mark II
*   Canon PowerShot G9
*   Canon PowerShot G9 X
*   Canon PowerShot G10
*   Canon PowerShot G11
*   Canon PowerShot G12
*   Canon PowerShot G15
*   Canon PowerShot G16
*   Canon PowerShot S2 IS (CHDK hack)
*   Canon PowerShot S3 IS (CHDK hack)
*   Canon PowerShot S5 IS (CHDK hack)
*   Canon PowerShot SD300 (CHDK hack)
*   Canon PowerShot S30
*   Canon PowerShot S40
*   Canon PowerShot S45
*   Canon PowerShot S50
*   Canon PowerShot S60
*   Canon PowerShot S70
*   Canon PowerShot S90
*   Canon PowerShot S95
*   Canon PowerShot S100
*   Canon PowerShot S110
*   Canon PowerShot S120
*   Canon PowerShot SX1 IS
*   Canon PowerShot SX110 IS (CHDK hack)
*   Canon PowerShot SX120 IS (CHDK hack)
*   Canon PowerShot SX220 HS (CHDK hack)
*   Canon PowerShot SX20 IS (CHDK hack)
*   Canon PowerShot SX30 IS (CHDK hack)
*   Canon PowerShot SX50 HS
*   Canon PowerShot SX60 HS
*   Canon IXUS 160 (CHDK hack)
*   Canon EOS D30
*   Canon EOS D60
*   Canon EOS 5D
*   Canon EOS 5D Mark II
*   Canon EOS 5D Mark III
*   Canon EOS 5DS
*   Canon EOS 5DS R
*   Canon EOS 6D
*   Canon EOS 7D
*   Canon EOS 7D Mark II
*   Canon EOS 10D
*   Canon EOS 20D
*   Canon EOS 30D
*   Canon EOS 40D
*   Canon EOS 50D
*   Canon EOS 60D
*   Canon EOS 70D
*   Canon EOS 77D / 9000D
*   Canon EOS 80D
*   Canon EOS 300D / Digital Rebel / Kiss Digital
*   Canon EOS 350D / Digital Rebel XT / Kiss Digital N
*   Canon EOS 400D / Digital Rebel XTi / Kiss Digital X
*   Canon EOS 450D / Digital Rebel XSi / Kiss Digital X2
*   Canon EOS 500D / Digital Rebel T1i / Kiss Digital X3
*   Canon EOS 550D / Digital Rebel T2i / Kiss Digital X4
*   Canon EOS 600D / Digital Rebel T3i / Kiss Digital X5
*   Canon EOS 650D / Digital Rebel T4i / Kiss Digital X6i
*   Canon EOS 700D / Digital Rebel T5i / Kiss Digital X7i
*   Canon EOS 750D / Digital Rebel T6i / Kiss Digital X8i
*   Canon EOS 760D / Digital Rebel T6s / Kiss Digital X9
*   Canon EOS 800D / Digital Rebel T7i / Kiss Digital X9i
*   Canon EOS 100D / Digital Rebel SL1 / Kiss Digital X7
*   Canon EOS 1000D / Digital Rebel XS / Kiss Digital F
*   Canon EOS 1100D / Digital Rebel T3 / Kiss Digital X50
*   Canon EOS 1200D / Digital Rebel T5 / Kiss Digital X70
*   Canon EOS 1300D / Digital Rebel T6 / Kiss Digital X80
*   Canon EOS 1500D / Digital Rebel T7 / Kiss Digital X90 / EOS 2000D
*   Canon EOS 3000D / Digital Rebel T100 / EOS 4000D
*   Canon EOS C500
*   Canon EOS D2000C
*   Canon EOS M
*   Canon EOS M3
*   Canon EOS M10
*   Canon EOS-1D
*   Canon EOS-1DS
*   Canon EOS-1D X
*   Canon EOS-1D X Mark II
*   Canon EOS-1D Mark II
*   Canon EOS-1D Mark II N
*   Canon EOS-1D Mark III
*   Canon EOS-1D Mark IV
*   Canon EOS-1Ds Mark II
*   Canon EOS-1Ds Mark III
*   Casio QV-2000UX
*   Casio QV-3000EX
*   Casio QV-3500EX
*   Casio QV-4000
*   Casio QV-5700
*   Casio QV-R41
*   Casio QV-R51
*   Casio QV-R61
*   Casio EX-FH100
*   Casio EX-S20
*   Casio EX-S100
*   Casio EX-Z4
*   Casio EX-Z50
*   Casio EX-Z500
*   Casio EX-Z55
*   Casio EX-Z60
*   Casio EX-Z75
*   Casio EX-Z750
*   Casio EX-Z8
*   Casio EX-Z850
*   Casio EX-Z1050
*   Casio EX-Z1080
*   Casio EX-ZR100
*   Casio Exlim Pro 505
*   Casio Exlim Pro 600
*   Casio Exlim Pro 700
*   Contax N Digital
*   Creative PC-CAM 600
*   DJI 4384x3288
*   DxO ONE
*   Epson R-D1
*   Foculus 531C
*   Fuji E550
*   Fuji E900
*   Fuji F700
*   Fuji F710
*   Fuji S1
*   Fuji S2Pro
*   Fuji S3Pro
*   Fuji S5Pro
*   Fuji S20Pro
*   Fuji S100FS
*   Fuji S5000
*   Fuji S5100/S5500
*   Fuji S5200/S5600
*   Fuji S6000fd
*   Fuji S7000
*   Fuji S9000/S9500
*   Fuji S9100/S9600
*   Fuji S200EXR
*   Fuji SL1000
*   Fuji HS10/HS11
*   Fuji HS20EXR
*   Fuji HS30EXR
*   Fuji HS50EXR
*   Fuji F550EXR
*   Fuji F600EXR
*   Fuji F770EXR
*   Fuji F800EXR
*   Fuji F900EXR
*   Fuji X-Pro1
*   Fuji X-Pro2
*   Fuji X-A1
*   Fuji X-A2
*   Fuji X-E1
*   Fuji X-E2
*   Fuji X-E2S
*   Fuji X-H1
*   Fuji X-M1
*   Fuji X-S1
*   Fuji X-T1
*   Fuji X-T2
*   Fuji X-T10
*   Fuji X-T20
*   Fuji XF1
*   Fuji XQ1
*   Fuji XQ2
*   Fuji X100
*   Fuji X100F
*   Fuji X100S
*   Fuji X100T
*   Fuji X10
*   Fuji X20
*   Fuji X30
*   Fuji X70
*   Fuji IS-1
*   Hasselblad CFV
*   Hasselblad CFV-2
*   Hasselblad H3D
*   Hasselblad H4D
*   Hasselblad V96C
*   Hasselblad X1D
*   Imacon Ixpress 16-megapixel
*   Imacon Ixpress 22-megapixel
*   Imacon Ixpress 39-megapixel
*   ISG 2020x1520
*   Kodak DC20
*   Kodak DC25
*   Kodak DC40
*   Kodak DC50
*   Kodak DC120 (also try [kdc2tiff](http://kdc2tiff.sourceforge.net/))
*   Kodak DCS200
*   Kodak DCS315C
*   Kodak DCS330C
*   Kodak DCS420
*   Kodak DCS460
*   Kodak DCS460A
*   Kodak DCS460D
*   Kodak DCS520C
*   Kodak DCS560C
*   Kodak DCS620C
*   Kodak DCS620X
*   Kodak DCS660C
*   Kodak DCS660M
*   Kodak DCS720X
*   Kodak DCS760C
*   Kodak DCS760M
*   Kodak EOSDCS1
*   Kodak EOSDCS3B
*   Kodak NC2000F
*   Kodak ProBack
*   Kodak PB645C
*   Kodak PB645H
*   Kodak PB645M
*   Kodak DCS Pro 14n
*   Kodak DCS Pro 14nx
*   Kodak DCS Pro SLR/c
*   Kodak DCS Pro SLR/n
*   Kodak C330
*   Kodak C603
*   Kodak P850
*   Kodak P880
*   Kodak Z980
*   Kodak Z981
*   Kodak Z990
*   Kodak Z1015
*   Kodak KAI-0340
*   Konica KD-400Z
*   Konica KD-510Z
*   Leaf AFi 7
*   Leaf AFi-II 12
*   Leaf Aptus 17
*   Leaf Aptus 22
*   Leaf Aptus 54S
*   Leaf Aptus 65
*   Leaf Aptus 75
*   Leaf Aptus 75S
*   Leaf Cantare
*   Leaf CatchLight
*   Leaf CMost
*   Leaf DCB2
*   Leaf Valeo 6
*   Leaf Valeo 11
*   Leaf Valeo 17
*   Leaf Valeo 22
*   Leaf Volare
*   Leica C (Typ 112)
*   Leica CL
*   Leica Digilux 2
*   Leica Digilux 3
*   Leica D-LUX2
*   Leica D-LUX3
*   Leica D-LUX4
*   Leica D-LUX5
*   Leica D-LUX6
*   Leica D-LUX (Typ 109)
*   Leica M (Typ 240)
*   Leica M (Typ 262)
*   Leica M Monochrom (Typ 246)
*   Leica M8
*   Leica M9
*   Leica M10
*   Leica Q (Typ 116)
*   Leica R8
*   Leica S (Typ 007)
*   Leica SL (Typ 601)
*   Leica T (Typ 701)
*   Leica TL
*   Leica TL2
*   Leica V-LUX1
*   Leica V-LUX2
*   Leica V-LUX3
*   Leica V-LUX4
*   Leica V-LUX (Typ 114)
*   Leica X VARIO (Typ 107)
*   Leica X1
*   Leica X2
*   Leica X (Typ 113)
*   Leica X-E (Typ 102)
*   Leica X-U (Typ 113)
*   Lenovo A820
*   Logitech Fotoman Pixtura
*   Mamiya ZD
*   Matrix 4608x3288
*   Micron 2010
*   Minolta RD175
*   Minolta DiMAGE 5
*   Minolta DiMAGE 7
*   Minolta DiMAGE 7i
*   Minolta DiMAGE 7Hi
*   Minolta DiMAGE A1
*   Minolta DiMAGE A2
*   Minolta DiMAGE A200
*   Minolta DiMAGE G400
*   Minolta DiMAGE G500
*   Minolta DiMAGE G530
*   Minolta DiMAGE G600
*   Minolta DiMAGE Z2
*   Minolta Alpha/Dynax/Maxxum 5D
*   Minolta Alpha/Dynax/Maxxum 7D
*   Motorola PIXL
*   Nikon D1
*   Nikon D1H
*   Nikon D1X
*   Nikon D2H
*   Nikon D2Hs
*   Nikon D2X
*   Nikon D2Xs
*   Nikon D3
*   Nikon D3s
*   Nikon D3X
*   Nikon D4
*   Nikon D4s
*   Nikon Df
*   Nikon D40
*   Nikon D40X
*   Nikon D5
*   Nikon D50
*   Nikon D60
*   Nikon D70
*   Nikon D70s
*   Nikon D80
*   Nikon D90
*   Nikon D100
*   Nikon D200
*   Nikon D300
*   Nikon D300s
*   Nikon D500
*   Nikon D600
*   Nikon D610
*   Nikon D700
*   Nikon D750
*   Nikon D800
*   Nikon D800E
*   Nikon D810
*   Nikon D850
*   Nikon D3000
*   Nikon D3100
*   Nikon D3200
*   Nikon D3300
*   Nikon D3400
*   Nikon D5000
*   Nikon D5100
*   Nikon D5200
*   Nikon D5300
*   Nikon D5500
*   Nikon D7000
*   Nikon D7100
*   Nikon D7200
*   Nikon D7500
*   Nikon 1 AW1
*   Nikon 1 J1
*   Nikon 1 J2
*   Nikon 1 J3
*   Nikon 1 J4
*   Nikon 1 J5
*   Nikon 1 S1
*   Nikon 1 V1
*   Nikon 1 V2
*   Nikon 1 V3
*   Nikon E700 ("DIAG RAW" hack)
*   Nikon E800 ("DIAG RAW" hack)
*   Nikon E880 ("DIAG RAW" hack)
*   Nikon E900 ("DIAG RAW" hack)
*   Nikon E950 ("DIAG RAW" hack)
*   Nikon E990 ("DIAG RAW" hack)
*   Nikon E995 ("DIAG RAW" hack)
*   Nikon E2100 ("DIAG RAW" hack)
*   Nikon E2500 ("DIAG RAW" hack)
*   Nikon E3200 ("DIAG RAW" hack)
*   Nikon E3700 ("DIAG RAW" hack)
*   Nikon E4300 ("DIAG RAW" hack)
*   Nikon E4500 ("DIAG RAW" hack)
*   Nikon E5000
*   Nikon E5400
*   Nikon E5700
*   Nikon E8400
*   Nikon E8700
*   Nikon E8800
*   Nikon Coolpix A
*   Nikon Coolpix P330
*   Nikon Coolpix P340
*   Nikon Coolpix P6000
*   Nikon Coolpix P7000
*   Nikon Coolpix P7100
*   Nikon Coolpix P7700
*   Nikon Coolpix P7800
*   Nikon Coolpix S6 ("DIAG RAW" hack)
*   Nokia N9
*   Nokia N95
*   Nokia X2
*   Nokia 1200x1600
*   Nokia Lumia 1020
*   Olympus AIR-A01
*   Olympus C3030Z
*   Olympus C5050Z
*   Olympus C5060WZ
*   Olympus C7070WZ
*   Olympus C70Z,C7000Z
*   Olympus C740UZ
*   Olympus C770UZ
*   Olympus C8080WZ
*   Olympus X200,D560Z,C350Z
*   Olympus E-1
*   Olympus E-3
*   Olympus E-5
*   Olympus E-10
*   Olympus E-20
*   Olympus E-30
*   Olympus E-300
*   Olympus E-330
*   Olympus E-400
*   Olympus E-410
*   Olympus E-420
*   Olympus E-500
*   Olympus E-510
*   Olympus E-520
*   Olympus E-620
*   Olympus E-M1
*   Olympus E-M1 Mark II
*   Olympus E-M5
*   Olympus E-M5 Mark II
*   Olympus E-M10
*   Olympus E-M10 Mark II
*   Olympus E-M10 Mark III
*   Olympus E-P1
*   Olympus E-P2
*   Olympus E-P3
*   Olympus E-P5
*   Olympus E-PL1
*   Olympus E-PL1s
*   Olympus E-PL2
*   Olympus E-PL3
*   Olympus E-PL5
*   Olympus E-PL7
*   Olympus E-PL8
*   Olympus E-PL9
*   Olympus E-PM1
*   Olympus E-PM2
*   Olympus PEN-F
*   Olympus SH-2
*   Olympus SP310
*   Olympus SP320
*   Olympus SP350
*   Olympus SP500UZ
*   Olympus SP510UZ
*   Olympus SP550UZ
*   Olympus SP560UZ
*   Olympus SP570UZ
*   Olympus STYLUS1
*   Olympus TG-4
*   Olympus TG-5
*   Olympus XZ-1
*   Olympus XZ-2
*   Olympus XZ-10
*   OmniVision OV5647 (Raspberry Pi)
*   Panasonic DMC-CM1
*   Panasonic DMC-FZ8
*   Panasonic DMC-FZ18
*   Panasonic DMC-FZ28
*   Panasonic DMC-FZ30
*   Panasonic DMC-FZ35/FZ38
*   Panasonic DMC-FZ40
*   Panasonic DMC-FZ50
*   Panasonic DMC-FZ70
*   Panasonic DC-FZ80
*   Panasonic DMC-FZ100
*   Panasonic DMC-FZ150
*   Panasonic DMC-FZ200
*   Panasonic DMC-FZ300
*   Panasonic DMC-FZ330
*   Panasonic DMC-FZ1000
*   Panasonic DMC-FZ2000
*   Panasonic DMC-FX150
*   Panasonic DMC-G1
*   Panasonic DMC-G2
*   Panasonic DMC-G3
*   Panasonic DMC-G5
*   Panasonic DMC-G6
*   Panasonic DMC-G7
*   Panasonic DC-G9
*   Panasonic DMC-G80
*   Panasonic DMC-GF1
*   Panasonic DMC-GF2
*   Panasonic DMC-GF3
*   Panasonic DMC-GF5
*   Panasonic DMC-GF6
*   Panasonic DMC-GF7
*   Panasonic DMC-GH1
*   Panasonic DMC-GH2
*   Panasonic DMC-GH3
*   Panasonic DMC-GH4
*   Panasonic DC-GH5
*   Panasonic DMC-GM1
*   Panasonic DMC-GM5
*   Panasonic DMC-GX1
*   Panasonic DMC-GX7
*   Panasonic DMC-GX8
*   Panasonic DC-GX9
*   Panasonic DMC-GX80
*   Panasonic DC-GX800
*   Panasonic DMC-L1
*   Panasonic DMC-L10
*   Panasonic DMC-LC1
*   Panasonic DMC-LF1
*   Panasonic DMC-LX1
*   Panasonic DMC-LX2
*   Panasonic DMC-LX3
*   Panasonic DMC-LX5
*   Panasonic DMC-LX7
*   Panasonic DMC-LX15
*   Panasonic DMC-LX100
*   Panasonic DMC-TZ61
*   Panasonic DMC-TZ80
*   Panasonic DC-TZ90
*   Panasonic DMC-TZ100
*   Panasonic DC-TZ200
*   Panasonic DMC-ZS40
*   Pentax \*ist D
*   Pentax \*ist DL
*   Pentax \*ist DL2
*   Pentax \*ist DS
*   Pentax \*ist DS2
*   Pentax K10D
*   Pentax K20D
*   Pentax K100D
*   Pentax K100D Super
*   Pentax K200D
*   Pentax K2000/K-m
*   Pentax K-x
*   Pentax K-r
*   Pentax K-1
*   Pentax K-3
*   Pentax K-3 II
*   Pentax K-5
*   Pentax K-5 II
*   Pentax K-5 II s
*   Pentax K-50
*   Pentax K-70
*   Pentax K-500
*   Pentax K-7
*   Pentax K-S1
*   Pentax K-S2
*   Pentax KP
*   Pentax Optio S
*   Pentax Optio S4
*   Pentax Optio 33WR
*   Pentax Optio 750Z
*   Pentax Q-S1
*   Pentax Q7
*   Pentax 645D
*   Pentax 645Z
*   Phase One LightPhase
*   Phase One H 10
*   Phase One H 20
*   Phase One H 25
*   Phase One P 20
*   Phase One P 25
*   Phase One P 30
*   Phase One P 45
*   Phase One P 45+
*   Photron BC2-HD
*   Pixelink A782
*   Polaroid x530
*   Redcode R3D format
*   Ricoh GR
*   Ricoh GR II
*   Ricoh GX200
*   Ricoh GXR MOUNT A12
*   Ricoh GXR A16
*   Rollei d530flex
*   RoverShot 3320af
*   Samsung EK-GN120
*   Samsung EX1
*   Samsung EX2F
*   Samsung GX-1S
*   Samsung GX10
*   Samsung GX20
*   Samsung NX1
*   Samsung NX10
*   Samsung NX11
*   Samsung NX100
*   Samsung NX20
*   Samsung NX200
*   Samsung NX210
*   Samsung NX30
*   Samsung NX300
*   Samsung NX300M
*   Samsung NX500
*   Samsung NX1000
*   Samsung NX1100
*   Samsung NX2000
*   Samsung NX3000
*   Samsung NX mini
*   Samsung WB550
*   Samsung WB2000
*   Samsung S85 (hacked)
*   Samsung S850 (hacked)
*   Sarnoff 4096x5440
*   Sigma SD9
*   Sigma SD10
*   Sigma SD14
*   Sigma SD15
*   Sigma SD1
*   Sigma SD1 Merill
*   Sigma DP1
*   Sigma DP1 Merill
*   Sigma DP1S
*   Sigma DP1X
*   Sigma DP2
*   Sigma DP2 Merill
*   Sigma DP2S
*   Sigma DP2X
*   Sinar 3072x2048
*   Sinar 4080x4080
*   Sinar 4080x5440
*   Sinar STI format
*   SMaL Ultra-Pocket 3
*   SMaL Ultra-Pocket 4
*   SMaL Ultra-Pocket 5
*   Sony DSC-F828
*   Sony DSC-R1
*   Sony DSC-RX0
*   Sony DSC-RX1
*   Sony DSC-RX1R
*   Sony DSC-RX1RM2
*   Sony DSC-RX10
*   Sony DSC-RX10M2
*   Sony DSC-RX10M3
*   Sony DSC-RX10M4
*   Sony DSC-RX100
*   Sony DSC-RX100M2
*   Sony DSC-RX100M3
*   Sony DSC-RX100M4
*   Sony DSC-RX100M5
*   Sony DSC-V3
*   Sony DSLR-A100
*   Sony DSLR-A200
*   Sony DSLR-A230
*   Sony DSLR-A290
*   Sony DSLR-A300
*   Sony DSLR-A330
*   Sony DSLR-A350
*   Sony DSLR-A380
*   Sony DSLR-A450
*   Sony DSLR-A500
*   Sony DSLR-A550
*   Sony DSLR-A580
*   Sony DSLR-A700
*   Sony DSLR-A850
*   Sony DSLR-A900
*   Sony ILCA-68
*   Sony ILCA-77M2
*   Sony ILCA-99M2
*   Sony ILCE-7
*   Sony ILCE-7M2
*   Sony ILCE-7M3
*   Sony ILCE-7R
*   Sony ILCE-7RM2
*   Sony ILCE-7RM3
*   Sony ILCE-7S
*   Sony ILCE-7SM2
*   Sony ILCE-9
*   Sony ILCE-3000
*   Sony ILCE-5000
*   Sony ILCE-5100
*   Sony ILCE-6000
*   Sony ILCE-6300
*   Sony ILCE-6500
*   Sony ILCE-QX1
*   Sony NEX-3
*   Sony NEX-3N
*   Sony NEX-5
*   Sony NEX-5N
*   Sony NEX-5R
*   Sony NEX-5T
*   Sony NEX-6
*   Sony NEX-7
*   Sony NEX-C3
*   Sony NEX-F3
*   Sony SLT-A33
*   Sony SLT-A35
*   Sony SLT-A37
*   Sony SLT-A55V
*   Sony SLT-A57
*   Sony SLT-A58
*   Sony SLT-A65V
*   Sony SLT-A77V
*   Sony SLT-A99V
*   Sony XCD-SX910CR
*   STV680 VGA
*   Xiro Xplorer V
*   YI M1
