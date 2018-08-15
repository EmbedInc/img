{   This file declares raw information about TIFF files.
}
const
{
*   Mnemonics for each of the field types.  These are always stored in I16
*   format.  The mnemonic names are taken from the manual "Tag Image File
*   Format Specification Revision 5.0 Final", 8 August 1988.  The mnemonic
*   names in the manual use upper case letters to distiguish the start of
*   a new "word".  The mnemonics here use exactly the same words, with
*   start of words preceeded with an underscore, and all letters in lower
*   case.  The whole mnemonic is then preceeded by "f_" to give the
*   mnemonics their own name space, and "_k" is appended because that is
*   our convention for identifying constants.  For example the manual's
*   name "BitsPerSample" becomes our symbol "f_bits_per_sample_k".
}
  f_new_subfile_type_k =       254;
  f_subfile_type_k =           255;    {not for new files}
  f_image_width_k =            256;
  f_image_length_k =           257;
  f_bits_per_sample_k =        258;
  f_compression_k =            259;
  f_photometric_interpretation_k = 262;
  f_thresholding_k =           263;    {not for new files}
  f_cell_width_k =             264;    {not for new files}
  f_cell_length_k =            265;    {not for new files}
  f_fill_order_k =             266;    {not for new files}
  f_document_name_k =          269;
  f_image_description_k =      270;
  f_make_k =                   271;
  f_model_k =                  272;
  f_strip_offsets_k =          273;
  f_orientation_k =            274;    {not for new files}
  f_samples_per_pixel_k =      277;
  f_rows_per_strip_k =         278;
  f_strip_byte_counts_k =      279;
  f_min_sample_value_k =       280;    {not for new files}
  f_max_sample_value_k =       281;    {not for new files}
  f_x_resolution_k =           282;
  f_y_resolution_k =           283;
  f_planar_configuration_k =   284;
  f_page_name_k =              285;
  f_x_position_k =             286;
  f_y_position_k =             287;
  f_free_offsets_k =           288;    {not for new files}
  f_free_byte_counts_k =       289;    {not for new files}
  f_gray_response_unit_k =     290;
  f_gray_response_curve_k =    291;
  f_group3_options_k =         292;
  f_group4_options_k =         293;
  f_resolution_unit_k =        296;
  f_page_number_k =            297;
  f_color_repsonse_curves_k =  301;
  f_software_k =               305;
  f_date_time_k =              306;
  f_artist_k =                 315;
  f_host_computer_k =          316;
  f_predictor_k =              317;
  f_white_point_k =            318;
  f_primary_chromaticities_k = 319;
  f_color_map_k =              320;
  f_halftone_hints_k =         321;
  f_tile_width_k =             322;
  f_tile_length_k =            323;
  f_tile_offsets_k =           324;
  f_tile_byte_counts_k =       325;
  f_subifd_k =                 330;
  f_ink_sets_k =               332;
  f_ink_names_k =              333;
  f_number_of_inks_k =         334;
  f_dot_range_k =              336;
  f_target_printer_k =         337;
  f_extra_samples_k =          338;
  f_sample_format_k =          339;
  f_smin_sample_value_k =      340;
  f_smax_sample_value_k =      341;
  f_transfer_range_k =         342;
  f_jpeg_proc_k =              512;
  f_jpeg_interchange_fmt_k =   513;
  f_jpeg_interchange_fmt_len_k = 514;
  f_jpeg_restart_interval_k =  515;
  f_jpeg_lossless_predictors_k = 517;
  f_jpeg_point_transforms_k =  518;
  f_jpeg_q_tables_k =          519;
  f_jpeg_dc_tables_k =         520;
  f_jpeg_ac_tables_k =         521;
  f_ycbcr_coeficients_k =      529;
  f_ycbcr_sub_sampling_k =     530;
  f_ycbcr_positioning_k =      531;
  f_reference_black_white_k =  532;
  f_xmp_k =                    700;
  f_cfadim_k =                 33421;
  f_cfapatt_k =                33422;
  f_copyright_k =              33432;
  f_exif_ifd_k =               34665;
  f_iccprofile_k =             34675;
  f_gps_ifd_k =                34853;
  f_dtm_orig_k =               36867;
  f_ep_stdid_k =               37398;
  f_senmeth_k =                37399;
{
*   Constants that identify the byte ordering in this TIFF file.
*   These are the values of the first two bytes.
}
  order_fwd_k = 16#4D4D;               {forwards, high byte stored first}
  order_bkw_k = 16#4949;               {backwards, low byte stored first}
{
*   The only TIFF file "version" ID we support.
}
  version_id_k = 42;
{
*   Mnemonics for all the possible TIFF file data types.  We should skip over
*   any fields that use an unrecognized data type.
}
  type_i8_k = 1;                       {8 bit unsigned integer}
  type_ascii_k = 2;                    {string of 8-bit ASCII codes, terminting null}
  type_i16_k = 3;                      {16 bit unsigned integer}
  type_i32_k = 4;                      {32 bit unsigned integer}
  type_rat_k = 5;                      {numerator then denominator in I32 format}
  type_i8s_k = 6;                      {8 bit signed integer}
  type_u8_k = 7;                       {undefined 8 bit data}
  type_i16s_k = 8;                     {16 bit signed integer}
  type_i32s_k = 9;                     {32 bit signed integer}
  type_rats_k = 10;                    {numerator then denominator in I32S format}
  type_fp4_k = 11;                     {32 bit IEEE floating point}
  type_fp8_k = 12;                     {64 bit IEEE floating point}
{
*   Constants for enumerated values that appear in the tag field data area.
*
*   For field type COMPRESSION:
}
  compress_none_k = 1;                 {data tightly packed in each row}
  compress_ccitt3s_k = 2;              {CCITT-3 subset as defined in TIFF manual}
  compress_ccitt3_k = 3;               {raw CCITT group 3 encoding}
  compress_ccitt4_k = 4;               {raw CCITT group 4 encoding}
  compress_lzw_k = 5;                  {LZW adaptive compression}
  compress_runlen_k = 32773;           {run-length encoding}
{
*   For field type GROUP3_OPTIONS.  These are 1-bit masks that provide more
*   detail about compression method CCITT3.
}
  ccitt3_2d_k = 1;                     {2D encoding, default is 1D}
  ccitt3_none_k = 2;                   {uncompressed mode}
  ccitt3_fill_k = 4;                   {fill bits added to align EOL on bytes}
{
*   For field type NEW_SUBFILE_TYPE.  These are 1-bit masks.
}
  subtype_reduced_k = 1;               {image is reduced version of another image}
  subtype_page_k = 2;                  {image is page of multi-page document}
  subtype_mask_k = 4;                  {image is 1-bit alpha values for another img}
{
*   For field type PHOTOMETRIC_INTERPRETATION.
}
  photo_wb_k = 0;                      {gray scale, 0 = white, max = black}
  photo_bw_k = 1;                      {gray scale, 0 = black, max = white}
  photo_rgb_k = 2;                     {separate RGB color values}
  photo_pcolor_k = 3;                  {psuedo color}
  photo_mask_k = 4;                    {image is 1-bit alpha values for another img}
  photo_cmyk_k = 5;                    {separated color, usually CMYK}
  photo_ycbcr_k = 6;                   {video Y, Cb, Cr color space}
  photo_cielab_k = 7;                  {CIE L, A, B color space}
  photo_cfa_k = 32803;                 {color filter array (CFA)}
{
*   For field type PLANAR_CONFIGURATION.
}
  planar_pix_k = 1;                    {whole pixels stored consecutively}
  planar_samp_k = 2;                   {each strip has just one pixel component}
{
*   For field type SUBFILE_TYPES.  This field type should not be used
*   for new files.
}
  oldtype_full_k = 1;                  {this is full resolution image}
  oldtype_reduced_k = 2;               {reduced version of another image this file}
  oldtype_page_k = 3;                  {image is a page of multi-page dosument}
{
*   For field type ORIENTATION.  This file type should not be used for
*   new files.  The mnemonics encode in what order the image data is stored.
*   The first "word" indicates the direction within a scan line.  The second
*   "word" indicates the direction from one scan line to the next.
*   the default is RIGHT_DOWN, meaning that pixels go right-to-left within
*   a scan line, and scan lines are stored in top-to-bottom order.
}
  orient_right_down_k = 1;
  orient_left_down_k = 2;
  orient_left_up_k = 3;
  orient_right_up_k = 4;
  orient_down_right_k = 5;
  orient_down_left_k = 6;
  orient_up_left_k = 7;
  orient_up_right_k = 8;
{
*   Special code values used in LZW compression.
}
  lzw_code_clear_k = 256;              {re-init table}
  lzw_code_eoi_k = 257;                {end of information}
  lzw_code_first_var_k = 258;          {first variable (not pre-defined) code}
  lzw_code_last_k = 4094;              {last possible code value}
  lzw_bits_first_code_k = 9;           {number of bits in first code after clear}
{
*   For field type PREDICTOR.
}
  pred_none_k = 1;                     {no special prediction algorithm}
  pred_hdiff_k = 2;                    {new vals are delta from previous within line}
{
*   Values for EXTRA_SAMPLES.
}
  extra_unspec_k = 0;                  {unspecified}
  extra_alpha_k = 1;                   {alpha, color is pre-multiplied}
  extra_alpha_sep_k = 2;               {alpha, color not pre-multiplied}
{
*   Tag values used in GPS IFDs.
}
  gps_version_k = 0;                   {spec version for this GPS IFD}
  gps_lat_ref_k = 1;                   {latitude reference (north or south)}
  gps_lat_k = 2;                       {latitude, deg min sec}
  gps_lon_ref_k = 3;                   {longitude reference (east or west)}
  gps_lon_k = 4;                       {longitude, deg min sec}
  gps_alt_ref_k = 5;                   {altitude reference (above/below sea level)}
  gps_alt_k = 6;                       {altitude, meters}
  gps_time_k = 7;                      {time within day, hour min sec}
  gps_sat_k = 8;                       {satellite info}
  gps_stat_k = 9;                      {receiver status}
  gps_measmode_k = 10;                 {measure mode (2D, 3D)}
  gps_dop_k = 11;                      {degree of precision}
  gps_speed_ref_k = 12;                {speed reference (which units used)}
  gps_speed_k = 13;                    {speed, units defined by SPEED_REF}
  gps_track_ref_k = 14;                {track reference (true or magnetic north)}
  gps_track_k = 15;                    {direction of movement}
  gps_dir_ref_k = 16;                  {direction reference (true or mag north)}
  gps_dir_k = 17;                      {direction picture taken in}
  gps_mapdat_k = 18;                   {map datum}
  gps_latdest_ref_k = 19;              {destination point lat ref (N or S)}
  gps_latdest_k = 20;                  {destination point latitude}
  gps_londest_ref_k = 21;              {destination point lon ref (E or W)}
  gps_londest_k = 22;                  {destination point longitude}
  gps_bear_ref_k = 23;                 {reference for bearing to dest point}
  gps_bear_k = 24;                     {bearing to destination point}
  gps_dist_ref_k = 25;                 {distance reference (units)}
  gps_dis_k = 26;                      {distance to destination point}
  gps_procmeth_k = 27;                 {processing method string}
  gps_area_k = 28;                     {area name string}
  gps_date_k = 29;                     {date, year month day}
  gps_diff_k = 30;                     {differential correction used flag}
{
*   Tag values used in EXIF IFDs.
}
  exif_exptime_k = 33434;              {exposure time, seconds}
  exif_fstop_k = 33437;                {F number}
  exif_expprog_k = 34850;              {exposure program type}
  exif_specsens_k = 34852;             {spectral sesitivity}
  exif_iso_k = 34855;                  {ISO "film" speed sensitivity}
  exif_oecf_k = 34856;                 {opto-electronic conversion function}
  exif_ver_k = 36864;                  {EXIF version}
  exif_dtm_k = 36867;                  {date/time originally created}
  exif_dtmdig_k = 36868;               {date/time digitized}
  exif_cconfig_k = 37121;              {components configuration}
  exif_cpbpp_k = 37122;                {compressed bits per pixel}
  exif_shutt_k = 37377;                {shutter speed}
  exif_aper_k = 37378;                 {lens aperture}
  exif_bright_k = 37379;               {brightness}
  exif_expbias_k = 37380;              {exposure bias}
  exif_maxf_k = 37381;                 {smallest F number of the lens (max aperture)}
  exif_dist_k = 37382;                 {distance to subject, meters}
  exif_metmode_k = 37383;              {metering mode}
  exif_lsource_k = 37384;              {the kind of light source}
  exif_flash_k = 37385;                {status of the flash when picture taken}
  exif_flen_k = 37386;                 {actual lens focal length, mm}
  exif_subarea_k = 37396;              {location of main subject in the overall scene}
  exif_maknote_k = 37500;              {maker note, manufacturer specific information}
  exif_comm_k = 37510;                 {user comment}
  exif_subsect_k = 37520;              {fractional seconds to add to date/time}
  exif_subsecor_k = 37521;             {fractional seconds to add to original date/time}
  exif_subsecdig_k = 37522;            {fractional seconds to add to digitized time}
  exif_flashpix_k = 40960;             {Flashpix format version supported by FPXR file}
  exif_colspace_k = 40961;             {color space information}
  exif_pixx_k = 40962;                 {width of the meaningful image}
  exif_pixy_k = 40963;                 {height of the meaningful image}
  exif_sound_k = 40964;                {name of related sound file}
  exif_flashe_k = 41483;               {flash energy, BCPS}
  exif_freq_k = 41484;                 {camera spacial frequency horiz, vert, and diagonal}
  exif_focpx_k = 41486;                {focal plane X pixel resolution}
  exif_focpy_k = 41487;                {focal plane Y pixel resolution}
  exif_focres_k = 41488;               {resolution unit used for focal plane}
  exif_subloc_k = 41492;               {location of main subject in the scene}
  exif_expind_k = 41493;               {exposure index}
  exif_sentype_k = 41495;              {sensor type}
  exif_fsource_k = 41728;              {type of device that created file}
  exif_scetype_k = 41729;              {scene type}
  exif_cfapatt_k = 41730;              {sensor color pattern}
  exif_custrend_k = 41985;             {custom rendering}
  exif_expmode_k = 41986;              {exposure mode}
  exif_whibal_k = 41987;               {white ballance mode}
  exif_zoomd_k = 41988;                {digital zoom ratio}
  exif_flen35_k = 41989;               {35mm equivalent focal length}
  exif_scaptype_k = 41990;             {scene capture type}
  exif_gain_k = 41991;                 {image gain adjustment}
  exif_contr_k = 41992;                {image contrast adjusment}
  exif_sat_k = 41993;                  {image saturation adjustment}
  exif_sharp_k = 41994;                {sharpness processing applied}
  exif_devsett_k = 41995;              {device settings when picture taken}
  exif_subdist_k = 41996;              {distance to the subject}
  exif_unique_k = 42016;               {unique identifier for this image}
{
*   CFA pattern color IDs.
}
  cfa_red_k = 0;                       {red}
  cfa_grn_k = 1;                       {green}
  cfa_blu_k = 2;                       {blue}
  cfa_cya_k = 3;                       {cyan}
  cfa_mag_k = 4;                       {magenta}
  cfa_yel_k = 5;                       {yellow}
  cfa_wht_k = 6;                       {white}
