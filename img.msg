/*   Messages that map to IMG library error codes.
/*
msg err1                               /* driver name
  lan english -def
    Bad image file type.  The image I/O driver of
    name "%p1s" does not exist.
msg err2                               /* filter name
  lan english -def
    Bad image filter name.  The image filter of name
    "%p1s" does not exist.
msg err3                               /* routine name, image width, buf width
  lan english -def
    Image is too wide for internal scan line buffer in routine %p1s.
    Image is %p2i pixels wide, internal buffer is %p3i pixels wide.
msg err4                               /* header format ID number
  lan english -def
    Bad header format ID encountered in IMG image file.
    Format ID was %p1i.
msg err5                               /* data format ID number
  lan english -def
    Bad data format ID encountered in IMG image file.
    Format ID was %p1.
msg err6                               /* bad option name, image file name
  lan english -def
    Unrecognized option "%p1s" encountered in image file format string.
msg err7                               /* bad parm, option name, image file name
  lan english -def
    Bad parameter "%p1s" encountered for option "%p2s" in image file
    format string.
msg err8                               /* bad opt name, filter name
  lan english -def
    Bad option "%p1s" in parameter string on attempt to open
    image filter "%p2s".
msg err9                               /* bad parm, option name, filter name
  lan english -def
    Bad parameter "%p1s" to option "%p2s" in parameter string
    on attempt to open image filter "%p3s".
msg err10                              /*
  lan english -def
    Illegal or inconsitant data encountered in image file "%p1s".
    The file may not be an image file, or it may be corrupted.
    Unexpected data value was %p2i at file offset %p3i.
msg err11
  lan english -def
    The requested operation is not possible or not supported for this
    image file type.
msg err12
  lan english -def
    No additional image is available in the image file.
msg err13
  lan english -def
    Missing parameter encountered for option %p1s in image file
    format string.
msg err14
  lan english -def
    Paremeter error encountered for option %p1s in image file format string.
msg err15
  lan english -def
    Syntax error in image file format string.  The first unexpected token
    was "%p1s".
msg err16
  lan english -def
    Syntax error encountered on line %p1i of file %p2s.
msg err17
  lan english -def
    Unrecognized command "%p1s" encountered on line %p2i of file %p3s.
msg err18
  lan english -def
    Internal error %p1i in routine %p2s.  Please report this error at once.
msg err19
  lan english -def
    Bad, illegal, or inconsitant data encountered on attempt to open or
    read an image file.  Error %p1i in routine %p2s.
msg err20
  lan english -def
    Attempt to add additional image to an image file where the new image was
    a different size than the first image.  This is not supported for this
    image file type.
msg err21
  lan english -def
    An attempt was made to create a new image in an image file before all the
    scan lines were written for the current image.  The current image must be
    fully written before a new image can be created in an image file.
msg err22
  lan english -def
    Writing more than one image to an image file is not supported for this
    image file type.
msg err23
  lan english -def
    Unexpected data type ID of %p1i encountered at offset %p2i of image file %p3s.
/*
/*   Error messages used by IMG library routines.
/*
msg close
  lan english -def                     /* image file name
    Error on close image file.  Image file name is "%p1s".
msg end_unexpected
  lan english -def
    Unexpected end of image file encountered.
msg open_read                          /* image file name
  lan english -def
    Error on open image file for read.  File name was "%p1s".
msg open_write                         /* image file name
  lan english -def
    Error on open image file for write.  File name was "%p1s".
msg read_scan_line
  lan english -def
    Error on reading scan line from image file.
msg rewind
  lan english -def
    Error on rewind image file.
msg write_last_buf
  lan english -def
    Error on writing last buffer to IMG file.
msg write_scan_line
  lan english -def
    Error on writing scan line to image file.
/*
/*   General messages used by application programs.
/*
msg bad_input_fnam
  lan english -def
    Illegal or missing input file name on command line.
msg image_too_tall
  lan english -def
    Input image is too tall for internal data buffer size.
msg image_too_tall_n
  lan english -def
    The input image contains too many scan lines for this program
    to handle.  Up to %p1i scan lines are allowed, but the image
    contains %p2i.
msg image_too_wide
  lan english -def
    Input image is too wide for internal data buffer size.
msg input_fnam_missing
  lan english -def
    No input file was specified.
msg open_input_image
  lan english -def
    Error on opening input image file for read.
msg open_output_image
  lan english -def
    Error on opening output image file for write.
msg output_fnam_missing
  lan english -def
    No output file was specified.
/*
/*   Message used by the TIFF file driver.
/*
msg tif_buf_create
  lan english -def
    Error occurred on attempt to create new TIFF file output buffer.
msg tif_buf_not_end
  lan english -def
    INTERNAL ERROR: A TIFF file write buffer overflowed, was tagged
    for automatic extension, but was not at the end of the file.
msg tif_buf_overflow
  lan english -def
    INTERNAL ERROR: A TIFF file write buffer overflowed and was not
    tagged for automatic extension.
msg tif_byte_order_bad
  lan english -def
    INTERNAL ERROR: Bad byte order ID of %p1i encountered.
msg tif_compress_ccitt
  lan english -def
    CCITT compression schemes not supported in TIFF files.
    TIFF file compression ID was %p1i.
msg tif_compress_bad
  lan english -def
    TIFF image file compression type %p1i is not supported.
msg tif_bits_per_comp_cnt_bad
  lan english -def
    Bad count value encountered in TIFF file entry BITS PER SAMPLE.
    Value was %p1i.  Only values of 1 or 3 are allowed.
msg tif_dtype_not_int
  lan english -def
    Data type not INTEGER.
msg tif_dtype_not_fp
  lan english -def
    Data type can not be converted to a floating point value.
msg tif_dtype_not_string
  lan english -def
    Data type is not STRING.
msg tif_dtype_unexpected
  lan english -def
    Unexpected data type encountered in TIFF file entry %p1i in
    directory %p2i.  Entry tag was %p3i, data type ID was %p4i.
msg tif_dtype_unexpected2
  lan english -def
    INTERNAL ERROR:  Unexpected tag data type encountered by
    subroutine WRITE_TAG in TIFF file image output driver.
    Tag = %p1i, data type = %p2i, count = %p3i.
msg tif_err_map_pixels
  lan english -def
    Error encountered on attempt to map additional TIFF file pixel
    data into memory.
msg tif_err_write
  lan english -def
    Error on attempt to write to TIFF image output file.
msg tif_ncomp_mismatch_photo
  lan english -def
    TIFF image file contains conflicting information about the
    number of components per pixel.  A BITS PER SAMPLE entry
    indicated %p1i, while a PHOTOMETRIC INTERPRETATION entry
    indicates %p2i.
msg tif_ncomp_mismatch_sampix
  len english -def
    TIFF image file contains conflicting information about the
    number of components per pixel.  A previous BITS PER SAMPLE
    entry implied %p1i, whereas a SAMPLES PER PIXEL entry indicates %p2i.
msg tif_photo_interp_unexpected
  lan english -def
    Unexpected TIFF file PHOTOMETRIC INTERPRETATION value of %p1i
    encountered.
msg tif_planar_unexpected
  lan english -def
    Unexpected TIFF file PLANAR CONFIGURATION value of %p1i encountered.
msg tif_predictor_unsupp
  lan english -def
    TIFF file predictor type %p1i is not supported.
msg tif_samp_pix_unexpected
  lan english -def
    Unexpected TIFF file SAMPLES PER PIXEL value of %pi1 encountered.
    Only value of 1 and 3 are supported.
msg tif_strip_byte_cnt_bad
  lan english -def
    Inconsitant TIFF file STRIP BYTE COUNTS entry length word value %p1i.
    It does not match an earlier STRIP OFFSETS length word value of %p2i.
msg tif_subfield_type_bad
  lan english -def
    Unexpected value of %p1i in TIFF file entry SUBFIELD TYPE.
msg tif_tag_write_bad_order
  lan english -def
    INTERNAL ERROR: Attempt to write TIFF file entries with tags
    in decending order.  ID of last tag was %p1i.  ID of new tag was %p2i.
/*
/*   Messages used by TGA file driver.
/*
msg tga_bad_image_type                 /* bad value
  lan english -def
    Illegal value of %p1i for IMAGE TYPE field in .tga file.
msg tga_bad_lut_type                   /* bad value
  lan english -def
    Illegal value of %p1i for COLOR MAP TYPE field in .tga file.
msg tga_no_image
  lan english -def
    No image data is specified in .tga image file.
msg tga_no_lut
  lan english -def
    The .tga file IMAGE DATA TYPE field specified color table mapping,
    but the COLOR MAP TYPE field indicates no color is present.
/*
/*   Messages used by GIF file driver.
/*
msg gif_write
  lan english -def
    Error on attempt to write to GIF output file.
/*
/*   Messages used by BMF file driver.
/*
msg bmf_bad_pixel_size
  lan english -def
    BMF image file contains %p1i bits per pixel.  Only BMF files with
    8 or fewer, or 24 bits per pixel are supported.
msg bmf_bad_lut_type
  lan english -def
    BMF file contains a LUT type flag value of %p1i.  Only value 0 and
    1 are supported.
/*
/*   Messages used by program IMAGE_INFO.
/*
msg image_info_fnam
  lan english -def
    Image file is %p1s.
msg image_info_data
  lan english -def
    X,Y image size = %p1i,%p2i pixels.
    DX/DY aspect ratio = %p3f10.3.
    ALPHA, RED, GREEN, BLUE precision = %p4i,%p5i,%p6i,%p7i bits/pixel.
    File type is "%p8s".
/*
/*   Messages used by program BMF_HEADER.
/*
msg bmf_header_map_file
  lan english -def
    Error on attempt to map bitmap file for read.
msg bmf_header_unmap_file
  lan english -def
    Error on attempt to unmap bitmap file.
msg bmf_header_info_file
  lan english -def
    .nfill
    Info for Apollo bitmap file "%p1s":
      Major version = %p2i
      Minor version = %p3i
      X size = %p4i
      Y size = %p5i
      Number of groups = %p6i
      Bits per inch = %p7i
      Total bytes in file = %p8i
      Color map type flag = %p9i
      Color map entries = %p10i
      Color map byte offset = %p11i
msg bmf_header_info_group
  lan english -def
    .nfill
      Info for group %p1i:
        Number of sections = %p2i
        Data bits per pixel in each section = %p3i
        Allocated bits per pixel in each section = %p4i
        Allocated bytes per scan line in each section = %p5i
        Byte offset for first section = %p6i
/*
/*   Message used by program IMAGE_FILTER.
/*
msg image_filter_read_filt_file
  lan english -def
    Error on read line from filter profile file.
msg image_filter_bad_shrink
  lan english -def
    SHRINK or DSHRINK value out of range.  Must be an integer greater than
    zero.
/*
/*   Messages used by program IMAGE_BLEND.
/*
msg image_blend_different_sizes
  lan english -def
    The two input images are of different sizes.  IMAGE_BLEND can only
    blend images of the same size.
/*
/*   Messages used by program IMAGE_HIST.
/*
msg image_hist_averages
  lan english -def
    .nfill
    Average red   = %p1f5.3
    Average green = %p2f5.3
    Average blue  = %p3f5.3
    Average alpha = %p4f5.3
/*
/*   Messages used by program IMAGE_DITHER.
/*
msg image_dither_bits_bad
  lan english -def
    The -BITS value of %p1i is out of range.  The value must be at least 1.
msg image_dither_eof
  lan english -def
    Unexpected end of dither pattern file encountered.
msg image_dither_levels_bad
  lan english -def
    The -LEVELS value of %p1i is out of range.
    The value must be at least 2.
msg image_dither_lnum_fnam
  lan english -def
    Error was encountered on line %p1i of file "%p2s".
msg image_dither_size_not_found
  lan english -def
    No SIZE command found in dither pattern file.
msg image_dither_too_many_lines
  lan english -def
    Too many lines found in dither pattern file.  There should be no more
    non-comment lines after the THRESHOLDS command.
msg image_dither_too_many_tokens
  lan english -def
    Too many tokens found in dither pattern file.  The first unexpected
    token was "%p1s".
/*
/*   Messages used by IMAGE_ANIM program.
/*
msg image_anim_frame
  lan english -def
    Frame %p1i, %p2f6.3 sec, %p3s
msg image_anim_lline_extra
  lan english -def
    Unexptected token "%p1s" found after file name in input images list.
msg image_anim_no_in
  lan english -def
    No input frames specified.
msg image_anim_no_out
  lan english -def
    No output image file name specified.
msg image_anim_size_change
  lan english -def
    Image "%p1s" contains %p2i x %p3i pixels, whereas previous images contained
    %p4i x %p5i pixels.  The image size must be the same for all frames in
    the animation.
/*
/*   Messages used by IMG library local programs.
/*
msg wsat_nsquare
  lan english -def
    The byte size of satellite data file "%p1s" is %p2i, which is not the
    square of an integer.  Image size can not be determined.
/*
/*   Message used by the IMAGE_EDIT program.
/*
msg iedit_ent_colors_black
  lan english -def
    Red, green, and blue components of black level, 0-1 scale:
msg iedit_ent_colors_brighten
  lan english -def
    Relative amount to brighten intermediate shades, usually 0 to 1.  May
    be negative to darken.
msg iedit_ent_colors_saturate
  lan english -def
    Relative amount to saturate colors more, usually 0 to 1.  May be
    negative to desaturate.
msg iedit_ent_colors_whbal
  lan english -def
    Red, green, and blue components of gray, 0-1 scale:
msg iedit_ent_colors_white
  lan english -def
    Red, green, and blue components of white level, 0-1 scale:
msg iedit_ent_crop_aspect
  lan english -def
    Width/height aspect ratio or relative width and height values:
msg iedit_ent_crop_bottom
  lan english -def
    Bottom Y included in crop region:
msg iedit_ent_crop_center
  lan english -def
    Crop region center coordinate:
msg iedit_ent_crop_height
  lan english -def
    Source image crop region height:
msg iedit_ent_crop_left
  lan english -def
    Left X included in crop region:
msg iedit_ent_crop_move
  lan english -def
    Amount to move crop region center in X and Y:
msg iedit_ent_crop_right
  lan english -def
    Right X included in crop region:
msg iedit_ent_crop_size
  lan english -def
    Source image crop region size:
msg iedit_ent_crop_top
  lan english -def
    Top Y included in crop region:
msg iedit_ent_crop_width
  lan english -def
    Source image crop region width:
msg iedit_ent_log_ratio
  lan english -def
    White to black ratio on F-stops:
msg iedit_ent_log_offset
  lan english -def
    Log range offset, 0-1 is black-white:
msg iedit_ent_out_bits
  lan english -def
    Minimum bits per color per pixel:
msg iedit_ent_out_qual
  lan english -def
    Percent quality versus compression:
msg iedit_ent_whbal_save
  lan english -def
    Name of this white balance:
msg iedit_err_whbal_save
  lan english -def
    The name must not be blank:
msg iedit_menu
  lan english -def
    .nfill
    0 File 1
    4 Display 1
    1 Crop 1
    2 Colors 3
    3 Out 1
msg iedit_menu_colors
  lan english -def
    .nfill
    9 "Auto blk/wht" 1
    1 "Auto black" 6
    2 "Auto white" 6
    4 "Black in" 2
    7 "Black out" 5
    5 "White in" 2
    8 "White out" 4
    12 "White balance ..." 3
    3 Brighten 2
    10 "Log ratio" 3
    11 "Log offset" 6
    6 Saturate 1
msg iedit_menu_col_whbal
  lan english -def
    .nfill
    1 Average 1
    3 Brightest 1
    2 Darkest 1
    4 "From file ..." 6
    7 Enter 1
    5 Save 1
    6 Reset 1
msg iedit_menu_crop
  lan english -def
    .nfill
    1 Size 1
    9 Center 1
    10 Move 1
    2 Width 1
    3 Height 1
    4 Left 1
    5 Right 1
    6 Top 1
    7 Bottom 1
    8 Aspect 1
msg iedit_menu_display
  lan english -def
    .nfill
    1 Fast 1
    2 Smooth 1
    3 "Rot right" 5
    4 "Rot left" 5
msg iedit_menu_file
  lan english -def
    .nfill
    2 Open 1
    1 Exit 2
msg iedit_menu_out
  lan english -def
    .nfill
    1 "Image type ..." 7
    2 "Bits/color" 1
    3 "Quality" 1
    4 "Save" 1
msg iedit_img_open
  lan english -def
    Opening %p1s
msg iedit_open_err
  lan english -def
    Error opening image file "p1s"
msg iedit_open_fnam
  lan english -def
    New input image file name:
msg iedit_open_nfound
  lan english -def
    Image file "%p1s" not found
msg iedit_out_fnam
  lan english -def
    Output image file name:
msg iedit_stat_err_out_close
  lan english -def
    Error on closing output image file.
msg iedit_stat_err_out_open
  lan english -def
    Error on attempt to open output image file.
msg iedit_stat_noinput
  lan english -def
    No input image is open.
