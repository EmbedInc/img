{   Public insert file for the IMG library.
*
*   This library provides utilities for accessing and manipulating
*   image files.
}
%natural_alignment;

const
  img_subsys_k = -5;                   {our subsystem ID}

  img_err_no_driver_k = 1;             {no image file driver of this name exists}
  img_err_no_filter_k = 2;             {no conversion filter of this name exists}
  img_err_too_wide_k = 3;              {image too wide for internal buffer}
  img_err_img_head_fmt_k = 4;          {bad IMG file header format ID}
  img_err_img_dform_k = 5;             {bad IMG file data format ID}
  img_err_bad_fmt_opt_k = 6;           {bad option token in FILE_FMT parameter}
  img_err_bad_fmt_parm_k = 7;          {bad parm to option in FILE_FMT string}
  img_err_bad_opt_filt_k = 8;          {bad option name in PARMS on open filter}
  img_err_bad_parm_filt_k = 9;         {bad option parm in PARMS on open filter}
  img_err_file_bad_k = 10;             {illegal or inconsistant data in image file}
  img_err_not_poss_k = 11;             {operation not possible on this img file type}
  img_err_end_images_k = 12;           {no "next" image available in image file}
  img_err_fmt_parm_miss_k = 13;        {missing parm to option in FILE_FMT string}
  img_err_fmt_parm_k = 14;             {FILE_FMT string parameter error}
  img_err_fmt_syntax_k = 15;           {syntax error in FILE_FMT string}
  img_err_file_syntax_k = 16;          {syntax error in a text file related to image}
  img_err_cmd_bad_k = 17;              {bad command on line xx of file yy}
  img_err_internal_k = 18;             {internal error}
  img_err_file_data_k = 19;            {bad or inconsistant data in file}
  img_err_size_change_k = 20;          {diff size for additional image in same file}
  img_err_old_ndone_k = 21;            {can't create new image, old not done}
  img_err_no_nextw_k = 22;             {NEXT_WRITE_IMG not supported}
  img_err_dtype_k = 23;                {unexpected data type ID in image file}

type
  img_col_k_t = (                      {mnemonics for pixel COL array indicies}
    img_col_alpha_k,                   {alpha COL index}
    img_col_red_k,                     {red COL index}
    img_col_grn_k,                     {green COL index}
    img_col_blu_k);                    {blue COL index}

  img_pix_fmt_k_t = (                  {menmonics for all the supported pixel types}
    img_pix_fmt_1_k,                   {pixel format is IMG_PIXEL1_T}
    img_pix_fmt_2_k);                  {pixel format is IMG_PIXEL2_T}

  img_head_field_k = (                 {IDs for comment header fields can't tell if filled in}
    imghead_time_k,                    {TIME}
    imghead_heast_k,                   {HOURS_EAST}
    imghead_alt_k,                     {ALTITUDE}
    imghead_latlon_k,                  {LAT and LON}
    imghead_locrad_k);                 {LOCRAD}
  img_head_field_k_t = set of img_head_field_k;

  img_head_t = record                  {machine readable info from header comments}
    fieldset: img_head_field_k_t;      {fields that are set that can't tell by value}
    time: sys_clock_t;                 {absolute CUT time picture was taken}
    hours_east: real;                  {hours east of local timezone when taken}
    iso: real;                         {ISO film speed or equivalent sensitivity}
    exptime: real;                     {exposure time, seconds}
    fstop: real;                       {F-stop number (aperture relative to focal length)}
    focal: real;                       {actual physical focal length, mm}
    focal35: real;                     {35mm equivalent focal length}
    altitude: real;                    {altitude above sea level, meters}
    lat: double;                       {degrees latitude, positive north}
    lon: double;                       {degrees longitude, positive east}
    locrad: real;                      {location confidence radius, meters}
    src_manuf: string_var80_t;         {source equipment manufacturer}
    src_model: string_var80_t;         {source equipment model name}
    src_softw: string_var80_t;         {source software description}
    src_host: string_var80_t;          {name of host computer originally created on}
    src_user: string_var80_t;          {user name on host computer}
    creator: string_var80_t;           {name of person that created image}
    copyright: string_var80_t;         {copyright string}
    list: boolean;                     {COMM list is allocated}
    comm: string_list_t;               {remaining image comment lines}
    end;
{
*   Data types related to a format 1 pixel.  This type of pixel contains 0-255
*   alpha, red, green, and blue values packed into a 32 bit integer.  This is
*   generally the resolution stored in most image files.  This format is the
*   "normal" format when image values are used directly without calculations,
*   and therefore require no additional precision than what is contained in
*   an image file.
}
  img_pixel1_t = packed record case integer of {format 1 ARGB pixel template}
    1:(                                {explicit names for each component}
      alpha: 0..255;
      red: 0..255;
      grn: 0..255;
      blu: 0..255);
    3:(                                {32 bit integer overlay}
      all: integer32);
    end;

  img_pixel1_p_t =                     {pointer to a format 1 pixel}
    ^img_pixel1_t;

  img_scan1_arg_t =                    {arbitrary length array of format 1 pixels}
    array[0..0] of img_pixel1_t;

  img_scan1_arg_p_t =                  {pointer to arbitrary length fmt 1 scan line}
    ^img_scan1_arg_t;
{
*   Data types related to a format 2 pixel.  This type of pixel contains 16 bits
*   for each of alpha, red, green, and blue.
}
  img_pixel2_t = packed record         {format 2 ARGB pixel}
    alpha: 0..65535;
    red: 0..65535;
    grn: 0..65535;
    blu: 0..65535;
    end;

  img_pixel2_p_t =                     {pointer to a format 2 pixel}
    ^img_pixel2_t;

  img_scan2_arg_t =                    {arbitrary length array of format 2 pixels}
    array[0..0] of img_pixel2_t;

  img_scan2_arg_p_t =                  {pointer to arbitrary length fmt 2 scan line}
    ^img_scan2_arg_t;
{
*   Data types related to an image connection or stream.
}
  img_conn_t = record                  {user image stream data connection handle}
    x_size, y_size: sys_int_machine_t; {number of pixels in image}
    aspect: real;                      {width/height of properly  displayed image}
    next_y: sys_int_machine_t;         {scan line coor of next transfer, top = 0}
    bits_alpha: sys_int_machine_t;     {max component precisions in file}
    bits_red: sys_int_machine_t;
    bits_grn: sys_int_machine_t;
    bits_blu: sys_int_machine_t;
    bits_max: sys_int_machine_t;       {max of all the component precisions}
    file_type: string_var32_t;         {lower case image file type, (IMG, TGA, etc)}
    read_write: file_rw_k_t;           {indicates open for read or write}
    gnam: string_leafname_t;           {generic image file leafname}
    tnam: string_treename_t;           {image file treename}
    comm: string_list_t;               {handle to list of comment lines}
    data_p: univ_ptr;                  {points to internal IMG library data block}
    end;

  img_conn_p_t =                       {pointer to image stream connection handle}
    ^img_conn_t;
{
*   Public entry point definitions.
}
procedure img_close (                  {close an image file}
  in out  img: img_conn_t;             {handle to open image file or connection}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_conn_fmt (               {make format string with resolution values}
  in      img: img_conn_t;             {image stream connection handle}
  out     file_fmt: univ string_var_arg_t); {will have ARGB resolution values}
  extern;

function img_end (                     {TRUE if no more images available in file}
  in out  stat: sys_err_t)             {status code, reset to no err image file end}
  :boolean;                            {TRUE on end of file status}
  val_param; extern;

procedure img_head_close (             {end use of header desc, dealloc resources}
  in out  head: img_head_t);           {descriptor to dealloc resources of}
  val_param; extern;

procedure img_head_get (               {read img comments and interpret header fields}
  in out  comm: string_list_t;         {raw list of image comment lines}
  out     head: img_head_t);           {interpreted header fields and remaining comments}
  val_param; extern;

procedure img_list_filts (             {get list of supported filters}
  in      rw: file_rw_t;               {read/write mode asking about}
  in out  names: univ string_var_arg_t); {list of filter name tokens}
  val_param; extern;

procedure img_list_types (             {get list of supported image file types}
  in      rw: file_rw_t;               {read/write mode asking about}
  in out  names: univ string_var_arg_t); {list of file suffix tokens}
  val_param; extern;

procedure img_mem_alloc (              {allocate memory associated with connection}
  in out  img: img_conn_t;             {handle to established image data stream}
  in      size: sys_int_adr_t;         {amount of memory to allocate}
  out     p: univ_ptr);                {pointer to start of new memory}
  val_param; extern;

procedure img_next_read_img (          {open next image in already-open file}
  in out  img: img_conn_t;             {connection handle to image open for read}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_next_write_img (         {open next image in already-open file}
  in out  img: img_conn_t;             {connection handle to image open for write}
  in      x_size, y_size: sys_int_machine_t; {size of image in pixels}
  in      parms: univ string_var_arg_t; {additional format commands}
  out     stat: sys_err_t);            {completion status code}
  val_param; extern;

procedure img_open_read_img (          {open an image file for read}
  in      fnam: univ string_var_arg_t; {name of image file to open}
  out     img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_open_read_filt (         {open a filter connection for read}
  in      img_old: img_conn_t;         {image stream filter will use as input}
  in      filt_name: string;           {filter name string}
  in      parms: univ string_var_arg_t; {parameter string passed to filter}
  out     img: img_conn_t;             {connection to output of filter}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_open_write_img (         {open an image file for write}
  in      fnam: univ string_var_arg_t; {name of image file to open}
  in      aspect: real;                {width/height of properly displayed image}
  in      x_size, y_size: sys_int_machine_t; {size of image in pixels}
  in      file_type: string;           {driver name (IMG, TGA, etc.)}
  in      file_fmt: univ string_var_arg_t; {format string, unique to each FILE_TYPE}
  in      comm: string_list_t;         {descriptor for chain of comment lines}
  out     img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}
  val_param; extern;

procedure img_read_scan1 (             {read next scan line as format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_read_scan2 (             {read next scan line as format 2 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_write_scan1 (            {write next scan line given format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_write_scan2 (            {write next scan line given format 2 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan2_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_rewind (                 {rewind image file to just before first pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}
  extern;
