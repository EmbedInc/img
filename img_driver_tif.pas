{   I/O driver for handling TIFF image files.
}
module img_driver_tif;
define img_d_tif_open_read;
define img_d_tif_open_write;
define img_d_tif_read_scan2;
define img_d_tif_write_scan1;
define img_d_tif_rewind;
define img_d_tif_close_read;
define img_d_tif_close_write;
%include 'img2.ins.pas';
%include 'tiff.ins.pas';

const
  lut_val_max_k = 65535;               {max value that can be stored in LUT entry}
  strip_len_unknown_k = 1024;          {amount to map for unknown strip length}
  adr_unk_k = lastof(sys_int_adr_t);   {flag to indicate address or length unknown}
  cfa_dim_max_k = 8;                   {max supported dimensions of CFA pattern}
  cr_k = 13;                           {carriage return character code}
  lf_k = 10;                           {line feed character code}
  {
  *   Derived constants.
  }
  cfapatt_maxind_k = cfa_dim_max_k - 1; {max allowed CFA pattern array indexes}
  ncfalines_max_k = ((cfa_dim_max_k div 2) * 2) + 1; {max scan lines need to buffer}
  cfaline_max_k = ncfalines_max_k - 1; {0-N index of last buffered scan line}
{
*   Declare globally visible entry points that are defined here but not declared
*   in any include file.
}
procedure img_d_tif_read_scan2 (       {read next scan line as format 2 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_tif_write_scan1 (      {write next scan line from format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_tif_rewind (           {reposition to start of image file}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}
  extern;

procedure img_d_tif_close_read (       {close TIFF file that was open for reading}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_tif_close_write (      {close TIFF file that was open for writing}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  extern;

type
  data_read_p_t = ^data_read_t;        {pointer to our private TIFF file read data}

  read_fmt_k_t = (                     {format for reading uncompressed values}
    read_fmt_8_k,                      {read as 8-bit words}
    read_fmt_16_k,                     {read as 16-bit words}
    read_fmt_32_k);                    {read as 32-bit words}

  lut_ent_p_t = ^lut_ent_t;
  lut_ent_t = record                   {data stored in one LUT entry}
    red: 0..lut_val_max_k;
    grn: 0..lut_val_max_k;
    blu: 0..lut_val_max_k;
    end;

  strip_p_t = ^strip_t;
  strip_t = record                     {info about one "strip" in TIFF file}
    ofs_red: sys_int_adr_t;            {strip offset for RED or when not RGB_SEP}
    ofs_grn: sys_int_adr_t;
    ofs_blu: sys_int_adr_t;
    ofs_comp4, ofs_comp5, ofs_comp6, ofs_comp7, ofs_comp8: sys_int_adr_t;
    len_red: sys_int_adr_t;            {strip size for RED or when not RGB_SEP}
    len_grn: sys_int_adr_t;
    len_blu: sys_int_adr_t;
    len_comp4, len_comp5, len_comp6, len_comp7, len_comp8: sys_int_adr_t;
    end;

  lzw_table_read_t =                   {LZW code table used for decompression}
    array[lzw_code_first_var_k..lzw_code_last_k] of string_var_p_t;

  pix_value_pred_p_t = ^pix_value_pred_t;
  pix_value_pred_t = 0..65535;         {read pix value after predictor applied}

  strip_state_p_t = ^strip_state_t;
  strip_state_t = record               {data for reading one strip}
    bits_n: sys_int_machine_t;         {number of not-yet-used bits in BITS}
    bits: sys_int_conv32_t;            {not-used-yet bits in low bits}
    read_p: ^int8u_t;                  {points to next byte in uncompressed buffer}
    ubuf_p: ^int8u_t;                  {points to local uncompressed buffer}
    pbuf_p: pix_value_pred_p_t;        {pnt to scan line after predictor applied}
    pred_p: pix_value_pred_p_t;        {pointer to current PBUF_P^ value}
    ubuf_size: sys_int_adr_t;          {number of bytes in UBUF}
    maph: file_map_handle_t;           {handle to current mapped region in file}
    map_p: ^int8u_t;                   {current read pointer to mapped region}
    left_mbytes: sys_int_adr_t;        {bytes left in mapped region}
    ofs_after_map: sys_int_adr_t;      {file offset after mapped region}
    whole_strip_done: boolean;         {TRUE if done uncompressing whole strip}
    end;

  pred_state_t = record                {state for processing predictor}
    dr_p: data_read_p_t;               {pointer to top level control block}
    ss_p: strip_state_p_t;             {pointer to strip state control block}
    bits_stored: sys_int_machine_t;    {bits of storage per component value}
    bits_val: sys_int_machine_t;       {meaningful bits per component value}
    bits_mask: sys_int_machine_t;      {mask of meaningful bits per component value}
    case integer of                    {different state for each predictor type}
pred_none_k: (                         {no predictor processing}
      );
pred_hdiff_k: (                        {raw values are horizontal differences}
      hdiff_mask: pix_value_pred_t;    {mask for valid bits in output value}
      hdiff_prev: pix_value_pred_t;    {final value of previous pixel}
      );
    end;

  rddir_state_t = record               {current directory reading state}
    maph_dir: file_map_handle_t;       {handle to currently mapped TIFF directory}
    read_dir_p: ^int8u_t;              {points to next byte in current directory}
    adr_ofs0_dir: sys_int_adr_t;       {adr where offset would be 0 for directory}
    ents_left: sys_int_machine_t;      {entries left after this one in curr TIFF dir}
    ent_n: sys_int_machine_t;          {number of current entry within directory}
    end;

  cfadat_p_t = ^cfadat_t;
  cfadat_t = array[0..3] of int16u_t;  {cached source scan line, dimensioned dynamically}

  cfaline_t = record                   {one cached line for interpolating CFA patt}
    dat_p: cfadat_p_t;                 {points to saved scan line}
    y: sys_int_machine_t;              {0-N scan line Y, -1 for no valid data}
    end;
{
*   Static data about one image somewhere within a TIFF file.
}
  imgflag_k_t = (                      {individual flags pertaining to a image}
    imgflag_time_k,                    {time within day has been filled in}
    imgflag_date_k,                    {date has been filled in}
    imgflag_texif_k,                   {time is from EXIF original date/time}
    imgflag_exif_sfrac_k,              {fractional seconds set in EXIF data}
    imgflag_tgps_k,                    {time is from GPS}
    imgflag_tzrel_k,                   {time is relative to the local time zone}
    imgflag_tz_k,                      {timezone explicitly set}
    imgflag_lat_k,                     {latitude set}
    imgflag_lon_k,                     {longitude set}
    imgflag_flen_act_k,                {actual lens focal length set}
    imgflag_flen_equiv_k);             {equivalent 35mm frame focal length set}
  imgflags_t = set of imgflag_k_t;

  image_t = record
    mem_p: util_mem_context_p_t;       {handle to all memory used for this desc}
    bits_red: sys_int_machine_t;       {meaningful bits per pixel per color component}
    bits_grn: sys_int_machine_t;
    bits_blu: sys_int_machine_t;
    bits_comp4: sys_int_machine_t;     {meaningful bits per extra non-RGB components}
    bits_comp5: sys_int_machine_t;
    bits_comp6: sys_int_machine_t;
    bits_comp7: sys_int_machine_t;
    bits_comp8: sys_int_machine_t;
    bits_stored: sys_int_machine_t;    {override of bits stored per component, 0 = no override}
    ncomp: sys_int_machine_t;          {total components per pixel}
    ncomp_color: sys_int_machine_t;    {number of components needed for pixel color}
    comp_alpha: sys_int_machine_t;     {1-N component for alpha, 0 = none}
    compress: sys_int_machine_t;       {data compression type, use COMPRESS_xxx_K}
    x_size: sys_int_machine_t;         {image width in pixels}
    y_size: sys_int_machine_t;         {image height in pixels}
    subfile: sys_int_machine_t;        {subfile type, combine SUBTYPE_xxx_K}
    photometric: sys_int_machine_t;    {photometric interpretation ID}
    truecolor: boolean;                {TRUE if separate RGB values supplied}
    rgb_sep: boolean;                  {TRUE if RGB pixel values in separate strips}
    alpha_premult: boolean;            {TRUE if colors premultiplied by alpha}
    predictor: sys_int_conv8_t;        {operation after encoding, use PRED_xxx_K}
    rows_strip: sys_int_machine_t;     {input scan lines per strip "block"}
    strip0_p: strip_p_t;               {points to data about strip 0, others follow}
    aspect: real;                      {width/height for properly displayed image}
    lut_max: sys_int_machine_t;        {largest allocated LUT slot number}
    red_max: sys_int_machine_t;        {largest color component values before LUT}
    grn_max: sys_int_machine_t;
    blu_max: sys_int_machine_t;
    lut_p: lut_ent_p_t;                {translates pixel values into final RGB}
    cfadimx, cfadimy: sys_int_machine_t; {CFA repeating pattern width and height}
    cfapatt:                           {CFA color IDs, use CFA_xxx_k}
      array[0..cfapatt_maxind_k, 0..cfapatt_maxind_k] of sys_int_machine_t;
    end;
{
*   Private data used for reading a TIFF file.
}
  data_read_t = record
    conn: file_conn_t;                 {handle to connection to TIFF file}
    byte_order: sys_byte_order_k_t;    {byte order used in this TIFF file}
    dir_n: sys_int_machine_t;          {number of current image file directory}
    entry_data_remote: boolean;        {data for curr entry is not in entry}
    maph_dir: file_map_handle_t;       {handle to currently mapped TIFF directory}
    read_dir_p: ^int8u_t;              {points to next byte in current directory}
    adr_ofs0_dir: sys_int_adr_t;       {adr where offset would be 0 for directory}
    ents_left: sys_int_machine_t;      {entries left after this one in curr TIFF dir}
    ent_n: sys_int_machine_t;          {number of current entry within directory}
    tag: sys_int_machine_t;            {tag ID of last entry read}
    dtype: sys_int_machine_t;          {data type ID of last entry read}
    cnt: sys_int_machine_t;            {count field value of last entry read}
    cntlft: sys_int_machine_t;         {N data items left to read in current entry}
    maph: file_map_handle_t;           {handle to mapped data of current entry}
    adr_ofs0: sys_int_adr_t;           {adr where offset would be 0 for curr read}
    tagdat_p: ^int8u_t;                {points to start of data for current tag}
    read_p: ^int8u_t;                  {pointer to next unread byte in TIFF file}
    strip_next_p: strip_p_t;           {points to data about next TIFF file strip}
    lines_left: sys_int_machine_t;     {whole scan lines left in curr strip}
    red, grn, blu,                     {data about reading current strips}
    comp4, comp5, comp6, comp7, comp8: strip_state_t;
    cfaline:                           {cached unpacked scan lines for CFA interpolation}
      array[0..cfaline_max_k] of cfaline_t;
    cfal_last: sys_int_machine_t;      {0-N index of last CFALINE entry in use}
    comp_read_fmt: read_fmt_k_t;       {format for reading pixel components}
    debug: sys_int_machine_t;          {0-10 debug level}
    comm: string_list_t;               {handle to comments strings list}
    nhead: sys_int_machine_t;          {number of header lines in comment list}
    date: sys_date_t;                  {date/time image was created}
    lat: double;                       {latitude, degrees north}
    lon: double;                       {longitude, degrees east}
    flen_act: real;                    {actual lens focal length, mm}
    flen_equiv: real;                  {equivalent 35mm frame lens focal length, mm}
    flags: imgflags_t;                 {set of individual flags}
    image: image_t;                    {data about the current image}
    end;
{
*   Private state used for writing a TIFF file.
}
  obuf_t = record                      {data about one output buffer}
    ofs_start: sys_int_adr_t;          {file offset for start of this buffer}
    ofs_curr: sys_int_adr_t;           {file offset for next byte to write}
    next_p: ^char;                     {pointer to next byte in buffer}
    len: sys_int_adr_t;                {buffer size}
    maph: file_map_handle_t;           {handle to buffer as mapped region of file}
    cont: boolean;                     {automatically continue buffer on overflow}
    buf_p: ^char;                      {points to buffer, mapped to file}
    end;

  obuf_p_t = ^obuf_t;                  {pointer to an output buffer descriptor}

  lzw_code_t = 0..4095;                {all the possible LZW compression codes}

  branch_node_p_t = ^branch_node_t;    {points to next node in 16-way tree}

  branch_info_t = record case integer of {info for one branch in a branch node}
    1:(                                {leaf node, no more splitting needed}
      code: lzw_code_t);               {the only code that could possibly match str}
    2:(                                {tree node, more splitting needed}
      node_p: branch_node_p_t);        {points to tree node for next 16-way split}
    end;

  branch_node_t = record               {one branch in 256-way tree to find LZW codes}
    code_end: lzw_code_t;              {code for string ends here}
    leaf:                              {1 bit value means leaf node for that char}
      array[0..7] of sys_int_min32_t;  {char 0 bit is bit 0 entry 0, next bit 1 ...}
    branch:                            {how to handle each 16-way split}
      array[0..255] of branch_info_t;
    end;

  data_write_t = record
    di_p: img_conn2_p_t;               {points back to IMG library private data}
    conn: file_conn_t;                 {handle to file, opened for mapped access}
    ofs_next_ent: sys_int_adr_t;       {file offset for start of next entry to write}
    buf_write_p: obuf_p_t;             {points to buffer currently writing to}
    buf_dir_p: obuf_p_t;               {points to buffer for directory entries}
    buf_strofs_p: obuf_p_t;            {points to buffer for strip offsets}
    str0_ofs_p: univ_ptr;              {pnt to offset of first strip}
    buf_strlen_p: obuf_p_t;            {points to buffer for strip lengths}
    str0_len_p: univ_ptr;              {pointer to length of first strip}
    tag_id_last: sys_int_machine_t;    {ID of last tag written for error checking}
    ofs_next: sys_int_adr_t;           {first unused file offset}
    lines_strip: sys_int_machine_t;    {scan lines per strip}
    n_strips: sys_int_machine_t;       {number of strips in image}
    lines_left: sys_int_machine_t;     {lines left in this strip}
    strip_start: sys_int_adr_t;        {file offset for start of current strip}
    dx: sys_int_machine_t;             {pixels in one scan line}
    mem_p: util_mem_context_p_t;       {dynamic mem context for LZW compression}
    uwb: sys_int_machine_t;            {bits not yet written to output stream}
    n_uwb: sys_int_machine_t;          {number of bits in UWB}
    node_top: branch_node_t;           {root LZW codes tree node}
    node_p: branch_node_p_t;           {points to last node for current STR}
    code_next: lzw_code_t;             {next code that will be entered into table}
    code_resize: sys_int_machine_t;    {codes one bit larger when reach this value}
    code_bits: sys_int_machine_t;      {number of bits in current LZW codes}
    ncomp: sys_int_machine_t;          {number of pixel components}
    debug: sys_int_machine_t;          {0-10 debug level, 0 = normal}
    str: string_var8192_t;             {input chars that match curr LZW code}
    alpha: boolean;                    {TRUE if writing alpha values}
    end;

  data_write_p_t =                     {pointer to TIFF file private data for writing}
    ^data_write_t;
{
*************************************************************************
*
*   Local function DEBUG_LEVEL
*
*   Return the 0-10 debug level to use in deciding what level of diagnostic
*   debug messages to print.  This value is taken from the DEBUG_TIFF
*   environment variable, if present.  The default is 0, which is "production"
*   mode.
}
function debug_level                   {return 0-10 debug level}
  :sys_int_machine_t;
  val_param; internal;

var
  str: string_var80_t;                 {environment variable value string}
  i: sys_int_machine_t;                {scratch integer}
  stat: sys_err_t;

begin
  str.max := sizeof(str.len);          {init local var string}
  debug_level := 0;                    {init to default debug level}

  sys_envvar_get (                     {get environment variable string}
    string_v('DEBUG_TIFF'(0)),         {environment variable name}
    str,                               {returned envvar value}
    stat);
  if sys_error(stat) then return;      {assume no such environment variable}

  string_t_int (str, i, stat);         {convert envvar string to integer value}
  if sys_error(stat) then return;      {use default on conversion error}
  debug_level := max(0, min(10, i));   {clip to legal range}
  end;
{
********************************************************************************
*
*   Local function STR_YMD (S, YEAR, MONTH, DAY)
*
*   Interprets a date string in the format:
*
*     YYYY:MM:DD
*
*   into the individual integer year, month, and day values.
*
*   The function returns TRUE on success and FALSE if the input string could
*   not be intepreted unambiguously.  When the function returns FALSE, the
*   values of the YEAR, MONTH and DAY returned values is undefined.
}
function str_ymd (                     {interpret YYYY:MM:DD date string}
  in      s: univ string_var_arg_t;    {input string}
  out     year: sys_int_machine_t;     {returned year, as in the string}
  out     month: sys_int_machine_t;    {returned month, as in the string}
  out     day: sys_int_machine_t)      {returned day, as in the string}
  :boolean;                            {success, return values undefined on failure}
  val_param; internal;

var
  p: string_index_t;                   {input string parse index}
  tk: string_var32_t;                  {individual field parsed from input string}
  delim: sys_int_machine_t;            {ID of delimiter used to end field}
  stat: sys_err_t;                     {completion status}

begin
  tk.max := size_char(tk.str);         {init local var string}
  str_ymd := false;                    {init to unable to fully parse input string}
  p := 1;                              {init input string parse index}

  string_token_anyd (                  {extract year field}
    s, p,                              {input string and parse index}
    ' :', 2,                           {list of delimiters}
    1,                                 {first N delimiters that may be repeated}
    [string_tkopt_padsp_k],            {strip any leading or trailing blanks}
    tk,                                {returned field value}
    delim,                             {1-N index of the final delimiter to end field}
    stat);
  if sys_error(stat) then return;
  string_t_int (tk, year, stat);
  if sys_error(stat) then return;

  string_token_anyd (                  {extract month field}
    s, p,                              {input string and parse index}
    ' :', 2,                           {list of delimiters}
    1,                                 {first N delimiters that may be repeated}
    [string_tkopt_padsp_k],            {strip any leading or trailing blanks}
    tk,                                {returned field value}
    delim,                             {1-N index of the final delimiter to end field}
    stat);
  if sys_error(stat) then return;
  string_t_int (tk, month, stat);
  if sys_error(stat) then return;

  string_token_anyd (                  {extract day field}
    s, p,                              {input string and parse index}
    ' ', 1,                            {list of delimiters}
    1,                                 {first N delimiters that may be repeated}
    [string_tkopt_padsp_k],            {strip any leading or trailing blanks}
    tk,                                {returned field value}
    delim,                             {1-N index of the final delimiter to end field}
    stat);
  if sys_error(stat) then return;
  string_t_int (tk, day, stat);
  if sys_error(stat) then return;

  str_ymd := true;                     {indicate success}
  end;
{
********************************************************************************
*
*   Local function STR_DATE (S, DATE)
*
*   Interprets the standard TIFF date/time string in S and sets the appropriate
*   fields in DATE according to the specified date/time.  The function returns
*   TRUE if this was successful and FALSE if the input string was not in valid
*   TIFF date/time format.  When the function returns FALSE, no changes will
*   have been made to DATE.
*
*   The TIFF date/time string format is:
*
*     YYYY:MM:DD HH:MM:SS
*
*   All the fields may be zero to indicate the string is not indicating a valid
*   or known time.
}
function str_date (                    {convert TIFF date/time string to date desc}
  in      s: univ string_var_arg_t;    {input string, must be TIFF date format}
  in out  date: sys_date_t)            {date/time descriptor to update}
  :boolean;                            {success, DATE not changed on failure}
  val_param; internal;

var
  p: string_index_t;                   {input string parse index}
  tk: string_var32_t;                  {individual field parsed from input string}
  delim: sys_int_machine_t;            {ID of delimiter used to end field}
  year: sys_int_machine_t;             {field values as parsed from the input string}
  month: sys_int_machine_t;
  day: sys_int_machine_t;
  hour: sys_int_machine_t;
  minute: sys_int_machine_t;
  sec: real;
  stat: sys_err_t;                     {completion status}

begin
  tk.max := size_char(tk.str);         {init local var string}
  str_date := false;                   {init to unable to fully parse input string}
  p := 1;                              {init input string parse index}

  string_token_anyd (                  {extract year field}
    s, p,                              {input string and parse index}
    ' :', 2,                           {list of delimiters}
    1,                                 {first N delimiters that may be repeated}
    [string_tkopt_padsp_k],            {strip any leading or trailing blanks}
    tk,                                {returned field value}
    delim,                             {1-N index of the final delimiter to end field}
    stat);
  if sys_error(stat) then return;
  string_t_int (tk, year, stat);
  if sys_error(stat) then return;

  string_token_anyd (                  {extract month field}
    s, p,                              {input string and parse index}
    ' :', 2,                           {list of delimiters}
    1,                                 {first N delimiters that may be repeated}
    [string_tkopt_padsp_k],            {strip any leading or trailing blanks}
    tk,                                {returned field value}
    delim,                             {1-N index of the final delimiter to end field}
    stat);
  if sys_error(stat) then return;
  string_t_int (tk, month, stat);
  if sys_error(stat) then return;

  string_token_anyd (                  {extract day field}
    s, p,                              {input string and parse index}
    ' ', 1,                            {list of delimiters}
    1,                                 {first N delimiters that may be repeated}
    [string_tkopt_padsp_k],            {strip any leading or trailing blanks}
    tk,                                {returned field value}
    delim,                             {1-N index of the final delimiter to end field}
    stat);
  if sys_error(stat) then return;
  string_t_int (tk, day, stat);
  if sys_error(stat) then return;

  if (year = 0) and (month = 0) and (day = 0) {invalid or unknown time ?}
    then return;

  string_token_anyd (                  {extract hour field}
    s, p,                              {input string and parse index}
    ' :', 2,                           {list of delimiters}
    1,                                 {first N delimiters that may be repeated}
    [string_tkopt_padsp_k],            {strip any leading or trailing blanks}
    tk,                                {returned field value}
    delim,                             {1-N index of the final delimiter to end field}
    stat);
  if sys_error(stat) then return;
  string_t_int (tk, hour, stat);
  if sys_error(stat) then return;

  string_token_anyd (                  {extract minute field}
    s, p,                              {input string and parse index}
    ' :', 2,                           {list of delimiters}
    1,                                 {first N delimiters that may be repeated}
    [string_tkopt_padsp_k],            {strip any leading or trailing blanks}
    tk,                                {returned field value}
    delim,                             {1-N index of the final delimiter to end field}
    stat);
  if sys_error(stat) then return;
  string_t_int (tk, minute, stat);
  if sys_error(stat) then return;

  string_token_anyd (                  {extract seconds field}
    s, p,                              {input string and parse index}
    ' ', 1,                            {list of delimiters}
    1,                                 {first N delimiters that may be repeated}
    [string_tkopt_padsp_k],            {strip any leading or trailing blanks}
    tk,                                {returned field value}
    delim,                             {1-N index of the final delimiter to end field}
    stat);
  if sys_error(stat) then return;
  string_t_fpm (tk, sec, stat);
  if sys_error(stat) then return;
{
*   The string has been successfully parsed and the field values extracted.
}
  date.year := year;
  date.month := month - 1;
  date.day := day - 1;
  date.hour := hour;
  date.minute := minute;
  date.second := trunc(sec);
  date.sec_frac := sec - date.second;

  str_date := true;                    {return indicating success}
  end;
{
********************************************************************************
*
*
*   Local subroutine ERROR_DTYPE (DR)
*
*   Abort program on unexpected data type error.  The data type ID in
*   the directory entry was not one we expected or knew how to handle.
}
procedure error_dtype (
  in      dr: data_read_t);            {our local state}
  options (internal, noreturn);

const
  max_msg_parms = 4;                   {max parameters we can pass to a message}

var
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  sys_msg_parm_int (msg_parm[1], dr.ent_n);
  sys_msg_parm_int (msg_parm[2], dr.dir_n);
  sys_msg_parm_int (msg_parm[3], dr.tag);
  sys_msg_parm_int (msg_parm[4], dr.dtype);
  sys_message_bomb ('img', 'tif_dtype_unexpected', msg_parm, 4);
  end;
{
********************************************************************************
*
*   Local function READ_I8 (DR)
*
*   Return the value of the next TIFF file byte interpreted as
*   an 8 bit unsigned integer.  This updates the local state for
*   reading the next byte.
}
function read_i8 (                     {read 8 bit unsigned integer from file}
  in out  dr: data_read_t)             {our local state}
  :sys_int_machine_t;                  {returned integer value}
  internal;

begin
  read_i8 := dr.read_p^;               {return byte value from file}
  dr.read_p := succ(dr.read_p);        {advance the pointer to the next byte}
  end;
{
********************************************************************************
*
*   Local function BACKUP8 (DR)
*
*   Back up one byte.  READ_I8 will now return the same byte as last time.
}
procedure backup8 (                    {back up one byte in reading sequence}
  in out  dr: data_read_t);            {our local state}
  val_param; internal;

begin
  dr.read_p := pred(dr.read_p);        {point back to the previous byte read}
  end;
{
********************************************************************************
*
*   Local subroutine READ_STR (S, DR)
*
*   Read the string at the current TIFF file position and return it in
*   S.  DR is our local data for reading the TIFF file.  The read pointer
*   will not be updated to after the string, since no data directly follows
*   a string in a TIFF file.
*
*   The string is returned with all trailing blanks stipped off.
}
procedure read_str (
  in out  s: univ string_var_arg_t;    {returned string}
  in out  dr: data_read_t);            {our local state for reading this file}
  val_param; internal;

var
  s_p: ^string;

begin
  if dr.dtype <> type_ascii_k then begin {wrong data type ?}
    sys_message ('img', 'tif_dtype_not_string');
    error_dtype (dr);
    end;
  s_p := univ_ptr(dr.read_p);          {get pointer to data in file}
  string_vstring (s, s_p^, dr.cnt);    {read string and make var string}
  string_unpad (s);                    {delete trailing blanks}
  end;
{
********************************************************************************
*
*   Local subroutine READ_TEXT (S, DR)
*
*   Read one line of text from the current TIFF file position and return it in
*   S.  Text lines are ended with CR, LF, or a CRLF or LFCR pair.  The
*   line end sequence is not returned in S.  Trailing blanks are stripped from
*   S.
}
procedure read_text (                  {read one line of text}
  in out  s: univ string_var_arg_t;    {returned line of text}
  in out  dr: data_read_t);            {our local state for reading this file}
  val_param; internal;

var
  b: sys_int_machine_t;                {value of last byte read}

begin
  s.len := 0;                          {init returned string to empty}

  while true do begin
    if dr.cntlft <= 0 then exit;       {no more bytes to read ?}
    b := read_i8 (dr);                 {get the next byte}
    dr.cntlft := dr.cntlft - 1;        {count one less data item left to read}
    if b = cr_k then begin             {found carriage return to end the line ?}
      b := read_i8(dr);                {get the next byte after that}
      dr.cntlft := dr.cntlft - 1;      {count one less data item left to read}
      if b <> lf_k then begin          {not LF of CR/LF sequence ?}
        backup8 (dr);                  {put the byte back}
        dr.cntlft := dr.cntlft + 1;
        end;
      exit;
      end;
    if b = lf_k then begin             {found line feed to end the line ?}
      b := read_i8(dr);                {get the next byte after that}
      if b <> cr_k then begin          {not CR of LF/CR sequence ?}
        backup8 (dr);                  {put the byte back}
        dr.cntlft := dr.cntlft + 1;
        end;
      exit;
      end;
    string_append1 (s, chr(b));        {append the data byte to end of this line}
    end;                               {back to do next byte on this text line}

  string_unpad (s);                    {delete trailing blanks}
  end;
{
********************************************************************************
*
*   Local function READ_I16 (DR)
*
*   Return the value of the next two TIFF file bytes interpreted as
*   a 16 bit integer.  This takes care of the byte ordering, and
*   updates the local state for reading the next byte.
*
*   The 16 bit integer is read as individual bytes because it may not
*   be aligned, and some machines barf on that.
}
function read_i16 (                    {read 16 bit unsigned integer from file}
  in out  dr: data_read_t)             {our local state}
  :sys_int_machine_t;                  {returned integer value}
  internal;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  s_p: ^string;
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  s_p := univ_ptr(dr.read_p);          {get pointer to the first byte}
  dr.read_p := univ_ptr(               {update read pointer to after the integer}
    sys_int_adr_t(s_p) + 2);
  case dr.byte_order of
sys_byte_order_fwd_k: begin            {byte order is high to low}
      read_i16 :=
        lshft(ord(s_p^[1]), 8) !
        ord(s_p^[2]);
      end;
sys_byte_order_bkw_k: begin            {byte order is low to high}
      read_i16 :=
        lshft(ord(s_p^[2]), 8) !
        ord(s_p^[1]);
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(dr.byte_order));
    sys_message_bomb ('img', 'tif_byte_order_bad', msg_parm, 1);
    end;                               {end of TIFF file byte order cases}
  end;
{
*************************************************************************
*
*   Local function READ_I32 (DR)
*
*   Return the value of the next four TIFF file bytes interpreted as
*   a 32 bit integer.  This takes care of the byte ordering, and
*   updates the local state for reading the next byte.
*
*   The 32 bit integer is read as individual bytes because it may not
*   be aligned, and some machines barf on that.
}
function read_i32 (                    {read 32 bit unsigned integer from file}
  in out  dr: data_read_t)             {our local state}
  :sys_int_machine_t;                  {returned integer value}
  internal;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  s_p: ^string;
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  s_p := univ_ptr(dr.read_p);          {get pointer to the first byte}
  dr.read_p := univ_ptr(               {update read pointer to after the integer}
    sys_int_adr_t(s_p) + 4);
  case dr.byte_order of
sys_byte_order_fwd_k: begin            {byte order is high to low}
      read_i32 :=
        lshft(ord(s_p^[1]), 24) !
        lshft(ord(s_p^[2]), 16) !
        lshft(ord(s_p^[3]), 8) !
        ord(s_p^[4]);
      end;
sys_byte_order_bkw_k: begin            {byte order is low to high}
      read_i32 :=
        lshft(ord(s_p^[4]), 24) !
        lshft(ord(s_p^[3]), 16) !
        lshft(ord(s_p^[2]), 8) !
        ord(s_p^[1]);
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], ord(dr.byte_order));
    sys_message_bomb ('img', 'tif_byte_order_bad', msg_parm, 1);
    end;                               {end of TIFF file byte order cases}
  end;
{
********************************************************************************
*
*   Local function ISINT (DR)
*
*   Returns TRUE if the next piece of data is integer.  This means READ_I can
*   return it successfully.  If this routine returns FALSE, then the data can
*   only be read with READ_FP.
}
function isint (                       {check for next data is integer}
  in out  dr: data_read_t)             {our local state}
  :boolean;                            {TRUE for integer}
  val_param; internal;

begin
  case dr.dtype of
type_i8_k,                             {unsigned 8 bit integer}
type_i16_k,                            {unsigned 16 bit integer}
type_i32_k,                            {unsigned 32 bit integer}
type_i8s_k,                            {signed 8 bit integer}
type_u8_k,                             {undefined 8 bit byte}
type_i16s_k,                           {signed 16 bit integer}
type_i32s_k: isint := true;            {signed 32 bit integer}
otherwise
    isint := false;
    end;
  end;
{
*************************************************************************
*
*   Local function READ_I (DR)
*
*   Read the next piece of data as an integer value.
}
function read_i (
  in out  dr: data_read_t)             {our local state}
  :sys_int_machine_t;                  {returned integer value}
  val_param; internal;

var
  i: sys_int_machine_t;

begin
  case dr.dtype of
type_i8_k,                             {unsigned 8 bit integer}
type_u8_k: read_i := read_i8(dr);      {undefined 8 bit byte}
type_i16_k: read_i := read_i16(dr);    {unsigned 16 bit integer}
type_i32_k: read_i := read_i32(dr);    {unsigned 32 bit integer}
type_i8s_k: begin                      {signed 8 bit integer}
      i := read_i8(dr);                {get raw unsigned value}
      if (i & 16#80) <> 0 then begin   {negative ?}
        i := i ! lshft(~0, 8);         {sign extend}
        end;
      read_i := i;
      end;
type_i16s_k: begin                     {signed 16 bit integer}
      i := read_i16(dr);               {get raw unsigned value}
      if (i & 16#8000) <> 0 then begin {negative ?}
        i := i ! lshft(~0, 16);        {sign extend}
        end;
      read_i := i;
      end;
type_i32s_k: read_i := read_i32(dr);   {signed 32 bit integer}
otherwise
    sys_message ('img', 'tif_dtype_not_int');
    error_dtype (dr);
    end;
  dr.cntlft := dr.cntlft - 1;          {count one less value left to read}
  end;
{
*************************************************************************
*
*   Local subroutine READ_RAT (DR, NUM, DEN)
*
*   Reads a TIFF file rational value.  The numerator and denominator of
*   the rational value are returned separately.
}
procedure read_rat (                   {read TIFF file rational data value}
  in out  dr: data_read_t;             {our local state}
  out     num: sys_int_machine_t;      {returned numerator}
  out     den: sys_int_machine_t);     {returned denominator}
  val_param; internal;

begin
  num := read_i32(dr);
  den := read_i32(dr);
  dr.cntlft := dr.cntlft - 1;          {count one less value left to read}
  end;
{
*************************************************************************
*
*   Local function READ_FP (DR)
*
*   Return the current TIFF file data value as a floating point number.
*   The TIFF file reading state is updated accordingly.
}
function read_fp (                     {read floating point value from TIFF file}
  in out  dr: data_read_t)             {our local state}
  :real;                               {returned floating point value}
  internal;

var
  num: real;                           {temp storage for numerator}
  ieee32: sys_fp_ieee32_t;             {32 bit IEEE floating point number}
  ieee64: sys_fp_ieee64_t;             {64 bit IEEE floating point number}
  put_p: ^char;                        {for writing byte into IEEE FP number}
  i: sys_int_machine_t;                {scratch integer}

begin
  case dr.dtype of                     {what is the TIFF file data type ?}
type_i8_k,                             {8 bit unsigned integer}
type_i16_k,                            {16 bit unsigned integer}
type_i32_k,                            {32 bit unsigned integer}
type_i8s_k,                            {8 bit signed integer}
type_u8_k,                             {undefined byte}
type_i16s_k,                           {16 bit signed integer}
type_i32s_k: begin                     {32 bit signed integer}
      read_fp := read_i(dr);           {read integer value}
      end;
type_rat_k,                            {numerator then denominator in I32 format}
type_rats_k: begin                     {numerator then denominator in I32S format}
      num := read_i32(dr);             {get numerator value}
      read_fp := num / read_i32(dr);   {return final quotient value}
      dr.cntlft := dr.cntlft - 1;      {count one less value left to read}
      end;
type_fp4_k: begin                      {32 bit IEEE floating point}
      put_p := univ_ptr(addr(ieee32)); {init pointer where to write bytes}
      for i := 1 to 4 do begin         {once for each byte in FP number}
        put_p^ := chr(read_i8(dr));    {get this byte and write to FP number}
        put_p := univ_ptr(sys_int_adr_t(put_p) + 1); {update put pointer}
        end;
      if dr.byte_order <> sys_byte_order_k then begin {not match sys byte order ?}
        sys_order_flip (ieee32, sizeof(ieee32)); {flip to system byte order}
        end;
      read_fp := sys_fp_from_ieee32(ieee32); {convert to system FP format}
      dr.cntlft := dr.cntlft - 1;      {count one less value left to read}
      end;
type_fp8_k: begin                      {64 bit IEEE floating point}
      put_p := univ_ptr(addr(ieee64)); {init pointer where to write bytes}
      for i := 1 to 8 do begin         {once for each byte in FP number}
        put_p^ := chr(read_i8(dr));    {get this byte and write to FP number}
        put_p := univ_ptr(sys_int_adr_t(put_p) + 1); {update put pointer}
        end;
      if dr.byte_order <> sys_byte_order_k then begin {not match sys byte order ?}
        sys_order_flip (ieee64, sizeof(ieee64)); {flip to system byte order}
        end;
      read_fp := sys_fp_from_ieee64(ieee64); {convert to system FP format}
      dr.cntlft := dr.cntlft - 1;      {count one less value left to read}
      end;
otherwise
    sys_message ('img', 'tif_dtype_not_fp');
    error_dtype (dr);
    end;
  end;
{
*************************************************************************
*
*   Local function READ_BITS (DR, N, SAVED_BITS, N_SAVED)
*
*   Return the next N sequential bits of a bit stream stored in memory.
*   DR.READ_P is pointing to the next memory byte to read, if neccessary.
*   SAVED_BITS contains bits that were read last time, but not used.
*   N_SAVED indicates the number of saved bits in SAVED_BITS.  The
*   exact format of SAVED_BITS is private to this routine.  The caller
*   must set both SAVED_BITS and N_SAVED to 0 before the first call.
*   DR.READ_P, SAVED_BITS, and N_SAVED will be appropriately updated.
}
function read_bits (                   {read new N bits from bit stream}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in      n: sys_int_machine_t;        {number of bits to read}
  in out  saved_bits: sys_int_machine_t; {contains bits read but not used yet}
  in out  n_saved: sys_int_machine_t)  {number of bits in SAVED_BITS}
  :sys_int_machine_t;
  val_param; internal;

var
  bits_left: sys_int_machine_t;        {number of bits left to read}
  bits: sys_int_machine_t;             {number of bits read this loop}
  mask: sys_int_conv32_t;              {mask for bits read this loop}
  val: sys_int_machine_t;              {returned value}

begin
  val := 0;                            {init accumulated bits}
  bits_left := n;                      {init number of bits left to read}

  repeat                               {loop until read all the required bits}
    if n_saved <= 0 then begin         {no unread bits available ?}
      if n <= 8
        then begin
          saved_bits := read_i8(dr);   {read memory in 8 bit units}
          n_saved := 8;
          end
        else begin
          if n <= 16
            then begin
              saved_bits := read_i16(dr); {read memory in 16 bit units}
              n_saved := 16;
              end
            else begin
              saved_bits := read_i32(dr); {read memory in 32 bit units}
              n_saved := 32;
              end
            ;
          end
        ;
      end;                             {N_SAVED bits are now available in SAVED_BITS}
    bits := min(n_saved, bits_left);   {make number of bits to read this loop}
    mask := ~lshft(~0, bits);          {make mask for new bits in VAL}
    val := lshft(val, bits);           {make room for the new bits}
    val := val ! (rshft(saved_bits, n_saved-bits) & mask); {accumulate new bits}
    n_saved := n_saved - bits;         {fewer bits now available in SAVED_BITS}
    bits_left := bits_left - bits;     {fewer bits left to go in returned value}
    until bits_left <= 0;              {back until got all the required bits}
  read_bits := val;                    {return the accumulated bits}
  end;
{
********************************************************************************
*
*   Local subroutine CLOSE_REMOTE (DR)
*
*   Close any mapped file segment open to access the remote data of a tag.  Data
*   of a tag is contained in the tag when it is 4 bytes in size of less.  When
*   the data exceeds 4 bytes, the 4 byte region in the tag is a file offset that
*   indicates where the data is stored.  In this case, a new file map region is
*   opened so that the data can be accessed.  ENTRY_DATA_REMOTE is set to TRUE
*   to indicate this happened, and the mapped region handle is saved in MAPH.
*
*   This routine does nothing if no remote data mapped region is open.
}
procedure close_remote (               {close mapped file region to remote data}
  in out  dr: data_read_t);            {private data for reading this TIFF file}
  internal;

begin
  if dr.entry_data_remote then begin   {a remote data mapped region is open ?}
    file_map_done (dr.maph);           {unmap data for previous directory entry}
    dr.entry_data_remote := false;     {remote mapped region no longer open}
    end;
  end;
{
********************************************************************************
*
*   Local subroutine NEXT_ENTRY (DR, STAT)
*
*   Read the header of the next directory entry, and set up for reading
*   its data.  The low level file reading state will be set up for reading
*   the data in this entry, whether it is remote from the entry or not.
*
*   It is assumed that DR.READ_DIR_P is pointing to the start of the
*   entry to read.
}
procedure next_entry (
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  out     stat: sys_err_t);            {returned completion status code}
  internal;

var
  size_ele: sys_int_adr_t;             {size of one data element}
  size_data: sys_int_adr_t;            {size of all the data elements}
  ofs_data: sys_int_adr_t;             {file offset for start of remote data}

begin
  sys_error_none (stat);               {init to no error}

  close_remote (dr);                   {close mapped region to previous data, if any}

  if dr.ents_left <= 0 then begin      {hit end of this directory ?}
    sys_stat_set (file_subsys_k, file_stat_eof_k, stat); {indicate end of directory}
    return;
    end;
  dr.ents_left := dr.ents_left - 1;    {there will be one less entry left}

  dr.read_p := dr.read_dir_p;          {set up state for reading entry}
  dr.adr_ofs0 := dr.adr_ofs0_dir;
  dr.tag := read_i16(dr);              {get entry tag value}
  dr.dtype := read_i16(dr);            {get data type ID}
  dr.cnt := read_i32(dr);              {get number of data items present}
  dr.cntlft := dr.cnt;                 {init number of data items left to read}
  dr.read_dir_p := univ_ptr(           {update directory pointer to after this entry}
    sys_int_adr_t(dr.read_p) + sizeof(integer32));
  dr.ent_n := dr.ent_n + 1;            {update number of this entry within directory}

  case dr.dtype of                     {get size of individual data elements}
type_i8_k: size_ele := 1;
type_ascii_k: size_ele := 1;
type_i16_k: size_ele := 2;
type_i32_k: size_ele := 4;
type_rat_k: size_ele := 8;
type_u8_k: size_ele := 1;
type_i16s_k: size_ele := 2;
type_i32s_k: size_ele := 4;
type_rats_k: size_ele := 8;
type_fp4_k: size_ele := 4;
type_fp8_k: size_ele := 8;
otherwise                              {unrecognized data type ID}
    sys_stat_set (img_subsys_k, img_err_dtype_k, stat); {bad data type}
    sys_stat_parm_int (dr.dtype, stat); {bad data type ID}
    sys_stat_parm_int (                {file offset at start of bad data}
      sys_int_adr_t(dr.read_p) - dr.adr_ofs0 - 6,
      stat);
    sys_stat_parm_vstr (dr.conn.tnam, stat); {file name}
    return;                            {return with bad data error}
    end;
  size_data := size_ele * dr.cnt;      {make size in bytes for all the data}

  if size_data > 4 then begin          {data is remote, not directly in the tag ?}
    dr.entry_data_remote := true;
    ofs_data := read_i32(dr);          {get file offset for start of data}
    file_map (                         {map entry data into our address space}
      dr.conn,                         {connection handle to mapped file}
      ofs_data,                        {file offset for this chunk of data}
      size_data,                       {size of region to map}
      [file_rw_read_k],                {we only need to read the data}
      dr.read_p,                       {returned pointer to mapped region}
      size_ele,                        {unused return value}
      dr.maph,                         {returned handle to mapped region}
      stat);
    if file_eof(stat) then begin
      sys_message_bomb ('img', 'end_unexpected', nil, 0);
      end;
    dr.adr_ofs0 :=                     {address where start of file would be mapped}
      sys_int_adr_t(dr.read_p) - ofs_data;
    end;
  dr.tagdat_p := dr.read_p;            {save pointer to start of tag data}
  end;
{
********************************************************************************
*
*   Local subroutine DIR_SAVE (DR, RDDIR)
*
*   Save the current directory reading state into RDDIR.  The directory reading
*   state can then be overwritten and restored later with DIR_REST.
}
procedure dir_save (                   {save current directory reading state}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  out     rddir: rddir_state_t);       {returned saved data}
  val_param; internal;

begin
  rddir.maph_dir := dr.maph_dir;
  rddir.read_dir_p := dr.read_dir_p;
  rddir.adr_ofs0_dir := dr.adr_ofs0_dir;
  rddir.ents_left := dr.ents_left;
  rddir.ent_n := dr.ent_n;
  end;
{
********************************************************************************
*
*   Local subroutine DIR_REST (DR, RDDIR)
*
*   Restore the current directory reading state from RDDIR.  The existing
*   current directory state will be overwritten.
}
procedure dir_rest (                   {restore current directory reading state}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in      rddir: rddir_state_t);       {save area to restore the state from}
  val_param; internal;

begin
  dr.maph_dir := rddir.maph_dir;
  dr.read_dir_p := rddir.read_dir_p;
  dr.adr_ofs0_dir := rddir.adr_ofs0_dir;
  dr.ents_left := rddir.ents_left;
  dr.ent_n := rddir.ent_n;
  end;
{
********************************************************************************
*
*   Local subroutine DIR_OPEN (DR, OFS, STAT)
*
*   Open the TIFF image file directory at file offset OFS and make it current.
*   The current directory state is overwritten, so either there must not be a
*   current directory open when this routine is called, or its state must have
*   been previously saved.
}
procedure dir_open (                   {open new image file directory (IFD)}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in      ofs: sys_int_adr_t;          {file offset of new directory start}
  out     stat: sys_err_t);            {returned completion status code}
  val_param; internal;

var
  size: sys_int_adr_t;                 {scratch memory size}

begin
  file_map (                           {map length word of new directory entry}
    dr.conn,                           {connection handle to mapped file}
    ofs,                               {file offset of mapped region start}
    sizeof(integer16),                 {amount of memory to map}
    [file_rw_read_k],                  {we only need to read the data}
    dr.read_p,                         {returned pointer to mapped region}
    size,                              {size of region actually mapped}
    dr.maph,                           {handle to mapped region}
    stat);
  if sys_error(stat) then return;
  dr.ents_left := read_i16(dr);        {get number of entries in this directory}
  file_map_done (dr.maph);             {unmap directory length word}

  file_map (                           {map all the entries in the directory}
    dr.conn,                           {connection handle to mapped file}
    ofs + 2,                           {file offset of mapped region start}
    (dr.ents_left * 12) + 4,           {size of region to map}
    [file_rw_read_k],                  {we only need to read the data}
    dr.read_dir_p,                     {returned pointer to mapped region}
    size,                              {size of region actually mapped}
    dr.maph_dir,                       {handle to mapped region}
    stat);
  if sys_error(stat) then return;
  dr.adr_ofs0_dir :=                   {address of where start of file would go}
    sys_int_adr_t(dr.read_dir_p) - ofs - 2;
  dr.ent_n := 0;                       {reset number of current entry within dir}
  end;
{
********************************************************************************
*
*   Local subroutine DIR_CLOSE (DR)
*
*   Close the current TIFF image file directory (IFD).
}
procedure dir_close (                  {close current directory}
  in out  dr: data_read_t);            {private data for reading this TIFF file}
  val_param; internal;

begin
  file_map_done (dr.maph_dir);         {close the map region to the directory}
  end;
{
********************************************************************************
*
*   Local subroutine NEXT_DIR (DR, STAT)
*
*   Set up the state for reading the entries in the next directory.
*   It is assumed that DR.READ_DIR_P is pointing to the file offset for
*   the next directory.  End of file is indicated when no subsequent
*   directory exists.  DR is the private data block for the connection
*   to this TIFF file.
}
procedure next_dir (
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  out     stat: sys_err_t);            {returned completion status code}
  internal;

var
  ofs: sys_int_adr_t;                  {file offset for new directory}

begin
  close_remote (dr);                   {close map to remote tag data, if open}

  dr.read_p := dr.read_dir_p;          {set pointer for low level read operations}
  ofs := read_i32(dr);                 {get file offset for new directory}
  file_map_done (dr.maph_dir);         {unmap old directory}
  if ofs <= 0 then begin               {no more directories ?}
    sys_stat_set (file_subsys_k, file_stat_eof_k, stat);
    return;                            {return indicating end of all directories}
    end;

  dir_open (dr, ofs, stat);            {open new dir and initialize the state}
  if sys_error(stat) then return;

  dr.dir_n := dr.dir_n + 1;            {update number for this new TIFF directory}

  if dr.debug >= 1 then begin
    writeln ('TIFF image file directory ', dr.dir_n, ', ', dr.ents_left, ' entries:');
    end;
  end;
{
*************************************************************************
*
*   Local subroutine CREATE_LUT (DR)
*
*   Create the pixel to RGB value translation table, if it has not already
*   been created.
}
procedure create_lut (
  in out  dr: data_read_t);            {private data for reading this TIFF file}
  internal;

var
  sz: sys_int_adr_t;                   {scratch size value}
  ent_p: lut_ent_p_t;                  {pointer to current LUT entry}
  i: sys_int_machine_t;                {loop counter}
  bits_max: sys_int_machine_t;         {max bits in any pixel component}

begin
  if dr.image.lut_p <> nil then return; {LUT already created ?}

  dr.image.red_max := ~lshft(~0, dr.image.bits_red); {make max component values}
  if dr.image.truecolor
    then begin
      dr.image.grn_max := ~lshft(~0, dr.image.bits_grn);
      dr.image.blu_max := ~lshft(~0, dr.image.bits_blu);
      end
    else begin
      dr.image.grn_max := dr.image.red_max;
      dr.image.blu_max := dr.image.red_max;
      end
    ;
  bits_max :=                          {max bits in any pixel component value}
    max(dr.image.bits_red, max(dr.image.bits_grn, dr.image.bits_blu));
  dr.image.lut_max := ~lshft(~0, bits_max); {make max needed LUT index}
  sz := (dr.image.lut_max + 1) * sizeof(lut_ent_t);
  util_mem_grab (                      {allocate memory for LUT}
    sz, dr.image.mem_p^, false, dr.image.lut_p);

  ent_p := dr.image.lut_p;             {init current LUT entry to first entry}
  for i := 0 to dr.image.lut_max do begin {once for each LUT entry}
    if
        (dr.image.red_max > 0) and     {this component is used at all ?}
        (i <= dr.image.red_max)        {still within range of this component ?}
        then begin
      ent_p^.red :=                    {set LUT value at index I for this component}
        min(lut_val_max_k, trunc((i / dr.image.red_max) * (lut_val_max_k+1)));
      end;
    if
        (dr.image.grn_max > 0) and     {this component is used at all ?}
        (i <= dr.image.grn_max)        {still within range of this component ?}
        then begin
      ent_p^.grn :=                    {set LUT value at index I for this component}
        min(lut_val_max_k, trunc((i / dr.image.grn_max) * (lut_val_max_k+1)));
      end;
    if
        (dr.image.blu_max > 0) and     {this component is used at all ?}
        (i <= dr.image.blu_max)        {still within range of this component ?}
        then begin
      ent_p^.blu :=                    {set LUT value at index I for this component}
        min(lut_val_max_k, trunc((i / dr.image.blu_max) * (lut_val_max_k+1)));
      end;
    ent_p := univ_ptr(sys_int_adr_t(ent_p) + sizeof(ent_p^)); {to next LUT entry}
    end;
  end;
{
********************************************************************************
*
*   local function UNCOMP_LINE_LEN (DR)
*
*   Return the TIFF file length of one uncompressed scan line in one strip.
}
function uncomp_line_len (
  in out  dr: data_read_t)             {private data for reading this TIFF file}
  :sys_int_adr_t;                      {returned line length}
  internal;

var
  sz: sys_int_adr_t;
  bits: sys_int_machine_t;             {scratch number of bits}

begin
  if dr.image.rgb_sep
    then begin                         {each component is in a separate strip}
      bits :=                          {bits per pixel}
        dr.image.bits_red;
      end
    else begin                         {all components are in the same strip}
      bits :=                          {bits per pixel}
        dr.image.bits_red + dr.image.bits_grn + dr.image.bits_blu +
        dr.image.bits_comp4 + dr.image.bits_comp5 + dr.image.bits_comp6 +
        dr.image.bits_comp7 + dr.image.bits_comp8;
      end
    ;
  bits := bits * dr.image.x_size;      {used bits per scan line}
  sz := (bits + 7) div 8;              {number of whole bytes needed for buffer}

  dr.comp_read_fmt := read_fmt_8_k;    {init to reading single bytes at a time}
  bits :=                              {max size of any pixel component}
    max(dr.image.bits_red, max(dr.image.bits_grn, dr.image.bits_blu));
  if bits > 8 then begin               {will read multiple bytes at a time ?}
    if bits > 16
      then begin                       {will read 32 bits at a time}
        sz := (sz + 3) div 4;          {round up to whole multiple of 32 bits}
        sz := sz * 4;
        dr.comp_read_fmt := read_fmt_32_k;
        end
      else begin                       {will read 16 bits at a time}
        sz := (sz + 1) div 2;          {round up to whole multiple of 16 bits}
        sz := sz * 2;
        dr.comp_read_fmt := read_fmt_16_k;
        end
      ;
    end;
  uncomp_line_len := sz;               {return final length answer}
  end;
{
*************************************************************************
*
*   Local subroutine RESET_STRIP_STATE (DR, SS)
*
*   Init the strip state data block SS.  DR is our private data for reading
*   this TIFF file.
}
procedure reset_strip_state (
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in out  ss: strip_state_t);          {strip state to initialize}
  internal;

var
  sz: sys_int_machine_t;               {scratch memory size}

begin
  if ss.ubuf_p = nil then begin        {no uncompressed buffer allocated yet ?}
    ss.ubuf_size := uncomp_line_len(dr); {get size of uncompressed scan line in strip}
    if dr.image.compress = compress_lzw_k then begin {need whole strip buffer ?}
      ss.ubuf_size := ss.ubuf_size * dr.image.rows_strip;
      end;
    util_mem_grab (                    {allocate memory for uncompressed buffer}
      ss.ubuf_size, dr.image.mem_p^, false, ss.ubuf_p);
    end;

  if ss.pbuf_p = nil then begin        {need to alloc post-predictor buffer data ?}
    sz :=                              {size of one expanded scan line}
      sizeof(ss.pbuf_p^) * dr.image.x_size;
    if not dr.image.rgb_sep then begin {all components together in this strip ?}
      sz := sz * dr.image.ncomp;       {times number of components per pixel}
      end;
    util_mem_grab (                    {allocate memory for post-predictor buffer}
      sz, dr.image.mem_p^, false, ss.pbuf_p);
    end;

  if ss.map_p <> nil then begin        {region of file is currently mapped ?}
    file_map_done (ss.maph);           {unmap the buffer}
    ss.map_p := nil;                   {indicate no region currently mapped}
    end;
  ss.left_mbytes := 0;
  end;
{
*************************************************************************
*
*   Local subroutine INIT_STRIP_STATE (DR, SS)
*
*   Completely initialize the strip state SS for the first time.
}
procedure init_strip_state (           {init strip state for the first time}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in out  ss: strip_state_t);          {strip state to initialize}
  internal;

begin
  ss.map_p := nil;
  ss.ubuf_p := nil;
  ss.pbuf_p := nil;
  reset_strip_state (dr, ss);
  end;
{
********************************************************************************
*
*   Local subroutine ADD_COMMENT (DR, HEAD, S)
*
*   Add the string S as a comment to the current image.  If HEAD is true, then
*   the string will be added to the header section of the comments.  The header
*   contains comment lines that have a specific format such that they provide
*   machine-readable data about the image.
*
*   Header lines start with a command name that must end with a colon, followed
*   by parameters specific to that command.  The parameters must be separated
*   from the command name and from each other by one or more spaces.  The
*   command must start in column 1 (column 1 must not be blank).
}
procedure add_comment (                {add comment line to the current image}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in      head: boolean;               {add comment to header, not at end}
  in      s: univ string_var_arg_t);   {the complete comment string to add}
  val_param; internal;

begin
  if head
    then begin                         {add string to end of header}
      string_list_pos_abs (dr.comm, dr.nhead); {to last header line}
      dr.nhead := dr.nhead + 1;        {there will be one more header line}
      end
    else begin                         {add string to end of comments}
      string_list_pos_last (dr.comm);  {to last line overall}
      end
    ;
  dr.comm.size := s.len;               {set min size required of the new line}
  string_list_line_add (dr.comm);      {add line after curr, go to new line}
  string_copy (s, dr.comm.str_p^);     {fill in line from caller's string}
  end;
{
********************************************************************************
*
*   Local subroutine DIR_EXIF (DR, OFS, STAT)
*
*   Read the EXIF IFD starting at the file offset OFS.  EXIF IFDs use different
*   tags from regular image IFDs.  All IFD processing is handled locally, and
*   the image descriptor is updated with any relevant data.
}
procedure dir_exif (                   {process EXIF IFD and update image info}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in      ofs: sys_int_adr_t;          {file offset to start of EXIF IFD}
  out     stat: sys_err_t);            {returned completion status code}
  val_param; internal;

var
  rddir: rddir_state_t;                {save state for existing directory reading}
  i1, i2, i3, i4, i5: sys_int_machine_t; {scratch integers and tag parameters}
  r1, r2: double;                      {scratch floating point parameters}
  tk: string_var80_t;
  tk2: string_var32_t;

label
  loop_ent, dump_int, ver_bad, subsect_bad, subsecor_bad,
  subsecdig_bad, done_ent, done_ents;

begin
  tk.max := size_char(tk.str);         {init local var strings}
  tk2.max := size_char(tk2.str);
  close_remote (dr);                   {make sure any previous remote data map is closed}
  dir_save (dr, rddir);                {save existing directory reading state}

  dir_open (dr, ofs, stat);            {open the new directory}

loop_ent:                              {back here each new directory entry}
  next_entry (dr, stat);               {set up for reading next directory entry}
  if file_eof(stat) then goto done_ents; {hit end of directory ?}
  case dr.tag of                       {which EXIF tag is this ?}
{
********************
*
*   EXIF tag EXPTIME
}
exif_exptime_k: begin
  r1 := read_fp(dr);
  string_vstring (tk, 'Exposure seconds: '(0), -1); {init image header comment string}
  string_f_fp_free (tk2, r1, 3);
  string_append (tk, tk2);
  string_appends (tk, ' 1/'(0));
  string_f_fp_free (tk2, 1.0/r1, 2);
  string_append (tk, tk2);
  add_comment (dr, true, tk);          {add header comment}

  if dr.debug >= 5 then begin
    string_f_fp_eng (tk, r1, 3, tk2);
    write ('    EXIF exposure time: ', tk.str:tk.len, ' ', tk2.str:tk2.len, 's');
    if r1 < 1.0 then begin
      i1 := round(1.0 / r1);
      write (', 1/', i1, ' s');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag FSTOP
}
exif_fstop_k: begin
  r1 := read_fp(dr);
  string_vstring (tk, 'F-stop: '(0), -1);
  string_f_fp_free (tk2, r1, 3);
  string_append (tk, tk2);
  add_comment (dr, true, tk);          {add header comment}

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF aperture: f/', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag EXPPROG
}
exif_expprog_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Not defined');
1:    string_appends (tk, 'Manual');
2:    string_appends (tk, 'Normal');
3:    string_appends (tk, 'Aperture priority');
4:    string_appends (tk, 'Shutter priority');
5:    string_appends (tk, 'More depth of field');
6:    string_appends (tk, 'Faster shutter speed');
7:    string_appends (tk, 'Portrait, close with background out of focus');
8:    string_appends (tk, 'Landscape, distant background in focus');
      end;
    write ('    EXIF exposure program ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag SPECSENS
}
exif_specsens_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    EXIF spectral sensitivity: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag ISO
}
exif_iso_k: begin
  i1 := read_i(dr);
  string_vstring (tk, 'ISO film speed: '(0), -1);
  string_f_int (tk2, i1);
  string_append (tk, tk2);
  add_comment (dr, true, tk);          {add header comment}

  if dr.debug >= 5 then begin
    writeln ('    EXIF film speed iso: ', i1);
    end;
  end;
{
********************
*
*   EXIF tag OECF
}
exif_oecf_k: begin

  if dr.debug >= 5 then begin
    writeln ('    EXIF oecf:');
dump_int:                              {dump remaining data as integers}
    i2 := dr.cntlft;
    if i2 <= 0 then goto done_ent;
    write ('     ');
    for i1 := 1 to min(8, i2) do begin
      write (' ', read_i(dr));
      end;
    if i2 > 8 then begin
      write (' ...');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag VER
}
exif_ver_k: begin
  read_text (tk, dr);                  {get version string, should be 4 chars}
  if tk.len <> 4 then goto ver_bad;    {not 4 characters as expected ?}
  string_substr (tk, 1, 2, tk2);       {extract major version string}
  string_t_int (tk2, i1, stat);        {convert to integer}
  if sys_error(stat) then goto ver_bad;
  string_substr (tk, 3, 4, tk2);       {extract minor version string}
  string_t_int (tk2, i2, stat);        {convert to integer}
  if sys_error(stat) then goto ver_bad;

  if dr.debug >= 5 then begin
    writeln ('    EXIF version: ', i1, '.', i2);
    goto done_ent;
    end;

ver_bad:                               {unexpected EXIF version string}
  if dr.debug >= 5 then begin
    writeln ('    EXIF version string: "', tk.str:tk.len, '"');
    goto done_ent;
    end;
  end;
{
********************
*
*   EXIF tag DTM
}
exif_dtm_k: begin
  read_str (tk, dr);
  if str_date (tk, dr.date) then begin {try to set image time}
    dr.flags := dr.flags + [
      imgflag_time_k,                  {time within day is known}
      imgflag_date_k,                  {date is known}
      imgflag_texif_k,                 {date/time is from EXIF data}
      imgflag_tzrel_k];                {time is relative to the local time zone}
    end;

  if dr.debug >= 5 then begin
    writeln ('    EXIF date/time original: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag DTMDIG
}
exif_dtmdig_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    EXIF date/time digitized: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag CCONFIG
}
exif_cconfig_k: begin

  if dr.debug >= 5 then begin
    writeln ('    EXIF components config:');
    goto dump_int;
    end;
  end;
{
********************
*
*   EXIF tag CPBPP
}
exif_cpbpp_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF compressed bits/pixel: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag SHUTT
}
exif_shutt_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF APEX shutter speed value: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag APER
}
exif_aper_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF APEX aperture: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag BRIGHT
}
exif_bright_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF APEX brightness: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag EXPBIAS
}
exif_expbias_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF APEX exposure bias: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag MAXF
}
exif_maxf_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF APEX max lens aperture: f/', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag DIST
}
exif_dist_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    r2 := r1 * 3.2808;
    string_f_fp_free (tk2, r1, 3);
    writeln ('    EXIF dist to subject: ', tk.str:tk.len, ' m, ', tk2.str:tk2.len, ' feet');
    end;
  end;
{
********************
*
*   EXIF tag METMODE
}
exif_metmode_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Unknown');
1:    string_appends (tk, 'Average');
2:    string_appends (tk, 'Center-weighted average');
3:    string_appends (tk, 'Spot');
4:    string_appends (tk, 'Multi-spot');
5:    string_appends (tk, 'Pattern');
6:    string_appends (tk, 'Partial');
255:  string_appends (tk, 'Other');
      end;
    write ('    EXIF metering mode ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag LSOURCE
}
exif_lsource_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Unknown');
1:    string_appends (tk, 'Daylight');
2:    string_appends (tk, 'Flurescent');
3:    string_appends (tk, 'Tungsten incandescent');
4:    string_appends (tk, 'Flash');
9:    string_appends (tk, 'Fine weather');
10:   string_appends (tk, 'Cloudy weather');
11:   string_appends (tk, 'Shade');
12:   string_appends (tk, 'Daylight flourescent (5700-7100 K)');
13:   string_appends (tk, 'Day white flourescent (4600-5400 K)');
14:   string_appends (tk, 'Cool white flourescent (3900-4500 K)');
15:   string_appends (tk, 'White flourescent (3200-3700 K)');
17:   string_appends (tk, 'Standard light A');
18:   string_appends (tk, 'Standard light B');
19:   string_appends (tk, 'Standard light C');
20:   string_appends (tk, 'D55');
21:   string_appends (tk, 'D65');
22:   string_appends (tk, 'D75');
23:   string_appends (tk, 'D50');
24:   string_appends (tk, 'ISO studio tungsten');
255:  string_appends (tk, 'Other');
      end;
    write ('    EXIF light source type ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag FLASH
}
exif_flash_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    string_f_int16h (tk, i1);
    writeln ('    EXIF flash status: ', tk.str:tk.len, 'h');
    case i1 & 1 of
0:    writeln ('      Did not fire');
1:    writeln ('      Fired');
      end;
    case rshft(i1, 1) & 3 of
0:    writeln ('      No strobe return detetction function');
2:    writeln ('      Strobe return light not detected');
3:    writeln ('      Strobe return light detected');
      end;
    case rshft(i1, 3) & 3 of
0:    writeln ('      Flash mode unknown');
1:    writeln ('      Flash mode always on');
2:    writeln ('      Flash mode always off');
3:    writeln ('      Flash mode auto on/off');
      end;
    case rshft(i1, 5) & 1 of
0:    writeln ('      Flash function present');
1:    writeln ('      No flash function');
      end;
    case rshft(i1, 6) & 1 of
0:    writeln ('      No red-eye reduction mode or unknown');
1:    writeln ('      Red-eye reduction supported');
      end;
    end;
  end;
{
********************
*
*   EXIF tag FLEN
}
exif_flen_k: begin
  r1 := read_fp(dr);
  dr.flen_act := r1;
  dr.flags := dr.flags + [imgflag_flen_act_k];

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF lens physical focal length: ', tk.str:tk.len, ' mm');
    end;
  end;
{
********************
*
*   EXIF tag SUBAREA
}
exif_subarea_k: begin
  if dr.cntlft > 0 then i1 := read_i(dr);
  if dr.cntlft > 0 then i2 := read_i(dr);
  if dr.cntlft > 0 then i3 := read_i(dr);
  if dr.cntlft > 0 then i4 := read_i(dr);

  if dr.debug >= 5 then begin
    write ('    EXIF subject area in picture:');
    case dr.cnt of
2:    writeln (' at ', i1, ',', i2);
3:    writeln (' at ', i1, ',', i2, ' radius ', i3);
4:    writeln (' at ', i1, ',', i2, ' width ', i3, ' height ', i4);
otherwise
      dr.read_p := dr.tagdat_p;        {reset to start of tag data}
      goto dump_int;
      end;
    end;
  end;
{
********************
*
*   EXIF tag MAKNOTE
}
exif_maknote_k: begin

  if dr.debug >= 5 then begin
    writeln ('    EXIF manufacturer info, length ', dr.cnt, ':');
    goto dump_int;
    end;
  end;
{
********************
*
*   EXIF tag COMM
}
exif_comm_k: begin
  tk.len := 0;
  for i1 := 1 to 8 do begin            {get the 8 header bytes}
    if dr.cntlft <= 0 then exit;
    string_append1 (tk, chr(read_i(dr)));
    end;
  while (tk.len > 0) and then (ord(tk.str[tk.len]) = 0) do begin
    tk.len := tk.len - 1;              {truncate trailing NULL character}
    end;
  string_upcase (tk);                  {make final character set name}

  if string_equal (tk, string_v('ASCII'(0))) then begin
    if dr.debug >= 5 then begin
      writeln ('    EXIF user comment, character set ', tk.str:tk.len, ':');
      end;
    while dr.cntlft > 0 do begin
      read_text (tk, dr);
      for i1 := 1 to tk.len do begin   {truncate at first control character}
        if ord(tk.str[i1]) < 32 then begin
          tk.len := i1 - 1;            {truncate before this control character}
          exit;
          end;
        end;
      string_unpad (tk);
      if (tk.len <= 0) and (dr.cntlft <= 0) {ignore trailing blank line}
        then goto done_ent;
      if dr.debug >= 5 then begin
        writeln ('      ', tk.str:tk.len);
        end;
      add_comment (dr, false, tk);
      end;
    goto done_ent;
    end;

  if dr.debug >= 5 then begin
    writeln ('    EXIF user comment, character set ', tk.str:tk.len, ', ', dr.cntlft, ' bytes:');
    goto dump_int;
    end;
  end;
{
********************
*
*   EXIF tag SUBSECT
}
exif_subsect_k: begin
  read_str (tk, dr);
  string_t_int (tk, i1, stat);         {convert to integer}
  if sys_error(stat) then goto subsect_bad;
  i2 := 1;                             {init denominator}
  for i3 := 1 to tk.len do begin
    i2 := i2 * 10;
    end;
  r1 := i1 / i2;                       {make seconds fraction}

  if dr.debug >= 5 then begin
    writeln ('    EXIF sec fraction date/time: ', r1:5:3);
    goto done_ent;
    end;
subsect_bad:
  if dr.debug >= 5 then begin
    writeln ('    EXIF sec fraction date/time: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag SUBSECOR
}
exif_subsecor_k: begin
  read_str (tk, dr);
  string_t_int (tk, i1, stat);         {convert to integer}
  if sys_error(stat) then goto subsect_bad;
  i2 := 1;                             {init denominator}
  for i3 := 1 to tk.len do begin
    i2 := i2 * 10;
    end;
  r1 := i1 / i2;                       {make seconds fraction}
  if imgflag_texif_k in dr.flags then begin {EXIF original time in DATE ?}
    dr.date.sec_frac := r1;            {save fraction into seconds}
    dr.flags := dr.flags + [
      imgflag_exif_sfrac_k];           {indicate seconds fraction from EXIF data}
    end;

  if dr.debug >= 5 then begin
    writeln ('    EXIF sec fraction date/time original: ', r1:5:3);
    goto done_ent;
    end;
subsecor_bad:
  if dr.debug >= 5 then begin
    writeln ('    EXIF sec fraction date/time original: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag SUBSECDIG
}
exif_subsecdig_k: begin
  read_str (tk, dr);
  string_t_int (tk, i1, stat);         {convert to integer}
  if sys_error(stat) then goto subsect_bad;
  i2 := 1;                             {init denominator}
  for i3 := 1 to tk.len do begin
    i2 := i2 * 10;
    end;
  r1 := i1 / i2;                       {make seconds fraction}

  if dr.debug >= 5 then begin
    writeln ('    EXIF sec fraction date/time digitized: ', r1:5:3);
    goto done_ent;
    end;
subsecdig_bad:
  if dr.debug >= 5 then begin
    writeln ('    EXIF sec fraction date/time digitized: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag FLASHPIX
}
exif_flashpix_k: begin
  read_text (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    EXIF flashpix format version: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag COLSPACE
}
exif_colspace_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
1:    string_appends (tk, 'sRGB');
65535: string_appends (tk, 'Uncalibrated');
      end;
    write ('    EXIF color space ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag PIXX
}
exif_pixx_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    writeln ('    EXIF valid image width: ', i1);
    end;
  end;
{
********************
*
*   EXIF tag PIXY
}
exif_pixy_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    writeln ('    EXIF valid image height: ', i1);
    end;
  end;
{
********************
*
*   EXIF tag SOUND
}
exif_sound_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    EXIF audio file: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag FLASHE
}
exif_flashe_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF flash energy: ', tk.str:tk.len, ' BCPS');
    end;
  end;
{
********************
*
*   EXIF tag FREQ
}
exif_freq_k: begin

  if dr.debug >= 5 then begin
    writeln ('    EXIF recording device spacial frequency, ', dr.cnt, ' bytes:');
    goto dump_int;
    end;
  end;
{
********************
*
*   EXIF tag FOCPX
}
exif_focpx_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF focal plane X resolution: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag FOCPY
}
exif_focpy_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF focal plane Y resolution: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag FOCRES
}
exif_focres_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
1:    string_appends (tk, 'Not specified');
2:    string_appends (tk, 'Inch');
3:    string_appends (tk, 'Centimeter');
      end;
    write ('    EXIF focal plane resolution unit ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag SUBLOC
}
exif_subloc_k: begin
  i1 := read_i(dr);
  i2 := read_i(dr);

  if dr.debug >= 5 then begin
    writeln ('    EXIF main subject pixel coor: ', i1, ',', i2);
    end;
  end;
{
********************
*
*   EXIF tag EXPIND
}
exif_expind_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 3);
    writeln ('    EXIF exposure index: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag SENTYPE
}
exif_sentype_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
1:    string_appends (tk, 'Not defined');
2:    string_appends (tk, 'One-chip color area sensor');
3:    string_appends (tk, 'Two-chip color area sensor');
4:    string_appends (tk, 'Three-chip color area sensor');
5:    string_appends (tk, 'Color sequential area sensor');
7:    string_appends (tk, 'Trilinear sensor');
8:    string_appends (tk, 'Color sequential linear sensor');
      end;
    write ('    EXIF sensing method ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag FSOURCE
}
exif_fsource_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
3:    string_appends (tk, 'Digital still camera');
      end;
    write ('    EXIF file source equipment type ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag SCETYPE
}
exif_scetype_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
1:    string_appends (tk, 'Directly photographed');
      end;
    write ('    EXIF scene type ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag CFAPATTERN
}
exif_cfapatt_k: begin
  i3 := read_i(dr);                    {get pattern width into I1}
  i4 := read_i(dr);
  if i3 <= i4
    then i1 := (i3 * 256) + i4
    else i1 := (i4 * 256) + i3;
  i3 := read_i(dr);                    {get pattern height into I2}
  i4 := read_i(dr);
  if i3 <= i4
    then i2 := (i3 * 256) + i4
    else i2 := (i4 * 256) + i3;

  if dr.debug >= 5 then begin
    writeln ('    EXIF sensor color pattern, ', i1, ' x ', i2, ':');
    for i4 := 1 to i2 do begin         {down the pattern rows}
      write ('     ');
      for i3 := 1 to i1 do begin       {accross this pattern row}
        i5 := read_i(dr);              {get ID for this patten pixel}
        case i5 of
0:        write (' red');
1:        write (' grn');
2:        write (' blu');
3:        write (' cya');
4:        write (' mag');
5:        write (' yel');
6:        write (' wht');
otherwise
          write (' ', i5:3);
          end;
        end;
      writeln;
      end;
    end;
  end;
{
********************
*
*   EXIF tag CUSTREND
}
exif_custrend_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Normal');
1:    string_appends (tk, 'Custom process');
      end;
    write ('    EXIF custom rendering ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag EXPMODE
}
exif_expmode_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Auto');
1:    string_appends (tk, 'Manual');
2:    string_appends (tk, 'Auto bracket');
      end;
    write ('    EXIF exposure mode ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag WHIBAL
}
exif_whibal_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Auto');
1:    string_appends (tk, 'Manual');
      end;
    write ('    EXIF white ballance ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag ZOOMD
}
exif_zoomd_k: begin
  read_rat (dr, i1, i2);

  if dr.debug >= 5 then begin
    if i1 = 0
      then string_vstring (tk, 'None'(0), -1)
      else string_f_fp_free (tk, i1/i2, 3);
    writeln ('    EXIF digital zoom ratio: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   EXIF tag FLEN35
}
exif_flen35_k: begin
  i1 := read_i(dr);
  dr.flen_equiv := i1;
  dr.flags := dr.flags + [imgflag_flen_equiv_k];

  if dr.debug >= 5 then begin
    writeln ('    EXIF 35mm equivalent focal length: ', i1, ' mm');
    end;
  end;
{
********************
*
*   EXIF tag SCAPTYPE
}
exif_scaptype_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Standard');
1:    string_appends (tk, 'Landscape');
2:    string_appends (tk, 'Portrait');
3:    string_appends (tk, 'Night');
      end;
    write ('    EXIF scene capture type ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag GAIN
}
exif_gain_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'None');
1:    string_appends (tk, 'Low gain up');
2:    string_appends (tk, 'High gain up');
3:    string_appends (tk, 'Low gain down');
4:    string_appends (tk, 'High gain down');
      end;
    write ('    EXIF gain control ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag CONTR
}
exif_contr_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Normal');
1:    string_appends (tk, 'Soft');
2:    string_appends (tk, 'Hard');
      end;
    write ('    EXIF applied contrast ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag SAT
}
exif_sat_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Normal');
1:    string_appends (tk, 'Low');
2:    string_appends (tk, 'High');
      end;
    write ('    EXIF applied saturation ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag SHARP
}
exif_sharp_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Normal');
1:    string_appends (tk, 'Soft');
2:    string_appends (tk, 'Hard');
      end;
    write ('    EXIF applied sharpness ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag DEVSETT
}
exif_devsett_k: begin

  if dr.debug >= 5 then begin
    writeln ('    EXIF device settings, ', dr.cnt, ' bytes:');
    goto dump_int;
    end;
  end;
{
********************
*
*   EXIF tag SUBDIST
}
exif_subdist_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    tk.len := 0;
    case i1 of
0:    string_appends (tk, 'Unknown');
1:    string_appends (tk, 'Macro');
2:    string_appends (tk, 'Close');
3:    string_appends (tk, 'Distant');
      end;
    write ('    EXIF distance range ID: ', i1);
    if tk.len > 0 then begin
      write (' (', tk.str:tk.len, ')');
      end;
    writeln;
    end;
  end;
{
********************
*
*   EXIF tag UNIQUE
}
exif_unique_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    EXIF unique image ID: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   Unexpected EXIF tag type.
}
otherwise
    writeln ('    EXIF tag ', dr.tag, ' type ', dr.dtype, ' count ', dr.cnt);
    end;
done_ent:                              {done with this directory entry}
  goto loop_ent;                       {back to do next directory entry}

done_ents:                             {done reading all directory entries}
  dir_close (dr);                      {end reading of the EXIF directory}
  dir_rest (dr, rddir);                {restore to directory state found on entry}
  end;
{
********************************************************************************
*
*   Local subroutine DIR_GPS (DR, IFDOFS, STAT)
*
*   Read the GPS IFD starting at the file offset OFS.  GPS IFDs use different
*   tags from regular image IFDs.  All IFD processing is handled locally, and
*   the image descriptor is updated with any relevant data.
}
procedure dir_gps (                    {process GPS IFD and update image info}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in      ofs: sys_int_adr_t;          {file offset to start of GPS IFD}
  out     stat: sys_err_t);            {returned completion status code}
  val_param; internal;

var
  rddir: rddir_state_t;                {save state for existing directory reading}
  i1, i2, i3, i4: sys_int_machine_t;   {scratch integers and tag parameters}
  r1, r2, r3, r4: double;              {scratch floating point parameters}
  tk: string_var80_t;
  tk2: string_var32_t;
  date: sys_date_t;                    {local expanded date descriptor}
  tgps: sys_clock_t;                   {absolute image time specified by GPS}
  texif: sys_clock_t;                  {absolute image time from EXIF data}
  north: boolean;                      {latitude is north, not south}
  east: boolean;                       {latitude is east, not west}
  above: boolean;                      {altitude is above sea level, not below}
  timeset: boolean;                    {time within day has been set in DATE}
  dateset: boolean;                    {year/month/day set in DATE}

label
  loop_ent, done_ents, done_autotz, done_time;

begin
  tk.max := size_char(tk.str);         {init local var strings}
  tk2.max := size_char(tk2.str);
  close_remote (dr);                   {make sure any previous remote data map is closed}
  dir_save (dr, rddir);                {save existing directory reading state}

  dir_open (dr, ofs, stat);            {open the GPS directory}
  north := true;                       {init to defaults}
  east := true;
  above := true;
  dateset := false;                    {init to year/month/day not set in DATE}
  timeset := false;                    {init to time not set in DATE}

loop_ent:                              {back here each new directory entry}
  next_entry (dr, stat);               {set up for reading next directory entry}
  if file_eof(stat) then goto done_ents; {hit end of directory ?}
  case dr.tag of                       {which GPS tag is this ?}
{
********************
*
*   GPS tag VERSION
}
gps_version_k: begin
  i1 := read_i(dr);                    {read the version bytes}
  i2 := read_i(dr);
  i3 := read_i(dr);
  i4 := read_i(dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS version: ', i1, ',', i2, ',', i3, ',', i4);
    end;
  end;
{
********************
*
*   GPS tag LAT_REF
}
gps_lat_ref_k: begin
  read_str (tk, dr);
  string_upcase (tk);
  if (tk.len >= 1) and (tk.str[1] = 'S') then begin
    north := false;
    end;

  if dr.debug >= 5 then begin
    writeln ('    GPS latitude reference: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag LAT
}
gps_lat_k: begin
  r1 := read_fp(dr);                   {degrees}
  r2 := read_fp(dr);                   {minutes}
  r3 := read_fp(dr);                   {seconds}
  r4 := r1 + (r2 / 60.0) + (r3 / 3600.0); {make degrees only}
  if not north then begin
    r4 := -r4;                         {negative for southern hemisphere}
    end;
  dr.lat := r4;                        {save final latitude}
  dr.flags := dr.flags + [imgflag_lat_k];

  if dr.debug >= 5 then begin
    writeln ('    GPS latitude: ', r1:9:5, ' deg ', r2:7:4, ' min ', r3:7:4, ' sec',
      ', =', r4:10:5);
    end;
  end;
{
********************
*
*   GPS tag LON_REF
}
gps_lon_ref_k: begin
  read_str (tk, dr);
  string_upcase (tk);
  if (tk.len >= 1) and (tk.str[1] = 'W') then begin
    east := false;
    end;

  if dr.debug >= 5 then begin
    writeln ('    GPS longitude reference: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag LON
}
gps_lon_k: begin
  r1 := read_fp(dr);                   {degrees}
  r2 := read_fp(dr);                   {minutes}
  r3 := read_fp(dr);                   {seconds}
  r4 := r1 + (r2 / 60.0) + (r3 / 3600.0); {make degrees only}
  if not east then begin
    r4 := -r4;                         {negative for western hemisphere}
    end;
  dr.lon := r4;                        {save final latitude}
  dr.flags := dr.flags + [imgflag_lon_k];

  if dr.debug >= 5 then begin
    writeln ('    GPS longitude: ', r1:9:5, ' deg ', r2:7:4, ' min ', r3:7:4, ' sec',
      ', =', r4:10:5);
    end;
  end;
{
********************
*
*   GPS tag ALT_REF
}
gps_alt_ref_k: begin
  i1 := read_i(dr);                    {get above/below sea level flag}
  if i1 <> 0 then begin
    above := false;
    end;

  if dr.debug >= 5 then begin
    writeln ('    GPS altitude reference: ', i1);
    end;
  end;
{
********************
*
*   GPS tag ALT
}
gps_alt_k: begin
  r1 := read_fp(dr);
  if not above then begin              {value is distance below sea level ?}
    r1 := -r1;
    end;
  string_vstring (tk, 'Altitude: '(0), -1);
  string_f_int (tk2, round(r1));
  string_append (tk, tk2);
  string_appends (tk, ' m, '(0));
  string_f_int (tk2, round(r1 * 3.28084));
  string_append (tk, tk2);
  string_appends (tk, ' ft'(0));
  add_comment (dr, true, tk);          {add header comment}

  if dr.debug >= 5 then begin
    writeln ('    GPS altitude: ', r1:7:1, ' meters (', trunc(r1*3.28084), ' feet)');
    end;
  end;
{
********************
*
*   GPS tag TIME
}
gps_time_k: begin
  r1 := read_fp(dr);                   {hours}
  r2 := read_fp(dr);                   {minutes}
  r3 := read_fp(dr);                   {seconds}

  r4 := (r1 * 3600.0) + (r2 * 60.0) + r3; {make total in seconds}
  i4 := round(r4 * 1000.0);            {make nearest integer milliseconds}
  i1 := i4 div 3600000;                {make whole hours}
  i4 := i4 - (i1 * 3600000);
  i2 := i4 div 60000;                  {make whole minutes}
  i4 := i4 - (i2 * 60000);
  i3 := i4 div 1000;                   {make whole seconds}
  i4 := i4 - (i3 * 1000);
  r4 := i4 / 1000.0;                   {fraction into second}

  date.hour := i1;                     {save data in local date descriptor}
  date.minute := i2;
  date.second := i3;
  date.sec_frac := r4;
  date.hours_west := 0.0;              {data is in coord universal time units}
  date.tzone_id := sys_tzone_cut_k;
  date.daysave := sys_daysave_no_k;
  date.daysave_on := false;
  timeset := true;                     {indicate time is set in DATE}

  if dr.debug >= 5 then begin
    string_f_int_max_base (            {make 2-digit hours string}
      tk, i1, 10, 2, [string_fi_leadz_k], stat);
    string_f_int_max_base (            {make 2-digit minutes string}
      tk2, i2, 10, 2, [string_fi_leadz_k], stat);
    string_append1 (tk, ':');
    string_append (tk, tk2);
    string_f_fp (                      {make seconds string}
      tk2, i3+r4, 6, 0, 0, 2, 3, 3, [string_ffp_leadz_k, string_ffp_z_aft_k], stat);
    string_append1 (tk, ':');
    string_append (tk, tk2);
    writeln ('    GPS time: ',
      r1:6:2, r2:6:2, r3:6:2, ' (', tk.str:tk.len, ' UTC)');
    end;
  end;
{
********************
*
*   GPS tag SAT
}
gps_sat_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS sat: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag STAT
}
gps_stat_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS stat: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag MEASMODE
}
gps_measmode_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS measmode: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag DOP
}
gps_dop_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 4);
    writeln ('    GPS dop: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag SPEED_REF
}
gps_speed_ref_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS speed_ref: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag SPEED
}
gps_speed_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 4);
    writeln ('    GPS speed: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag TRACK_REF
}
gps_track_ref_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS track reference: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag TRACK
}
gps_track_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 5);
    writeln ('    GPS track direction: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag DIR_REF
}
gps_dir_ref_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS direction reference: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag DIR
}
gps_dir_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 5);
    writeln ('    GPS direction: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag MAPDAT
}
gps_mapdat_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS map datum: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag LATDEST_REF
}
gps_latdest_ref_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS dest latitude reference: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag LATDEST
}
gps_latdest_k: begin
  r1 := read_fp(dr);                   {degrees}
  r2 := read_fp(dr);                   {minutes}
  r3 := read_fp(dr);                   {seconds}

  if dr.debug >= 5 then begin
    writeln ('    GPS dest latitude: ', r1:9:5, ' deg ', r2:7:4, ' min ', r3:7:4, ' sec');
    end;
  end;
{
********************
*
*   GPS tag LONDEST_REF
}
gps_londest_ref_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS dest longitude reference: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag LONDEST
}
gps_londest_k: begin
  r1 := read_fp(dr);                   {degrees}
  r2 := read_fp(dr);                   {minutes}
  r3 := read_fp(dr);                   {seconds}

  if dr.debug >= 5 then begin
    writeln ('    GPS dest longitude: ', r1:9:5, ' deg ', r2:7:4, ' min ', r3:7:4, ' sec');
    end;
  end;
{
********************
*
*   GPS tag BEAR_REF
}
gps_bear_ref_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS bearing reference: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag BEAR
}
gps_bear_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 5);
    writeln ('    GPS bearing: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag DIST_REF
}
gps_dist_ref_k: begin
  read_str (tk, dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS dest distance reference: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag DIS
}
gps_dis_k: begin
  r1 := read_fp(dr);

  if dr.debug >= 5 then begin
    string_f_fp_free (tk, r1, 4);
    writeln ('    GPS dest distance: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag PROCMETH
}
gps_procmeth_k: begin
  if dr.debug >= 5 then begin
    writeln ('    GPS procmeth, type ', dr.dtype, ' count ', dr.cnt);
    end;
  end;
{
********************
*
*   GPS tag AREA
}
gps_area_k: begin
  tk.len := 0;
  for i1 := 1 to dr.cnt do begin
    string_append1 (tk, chr(read_i(dr)));
    end;

  if dr.debug >= 5 then begin
    writeln ('    GPS area: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag DATE
}
gps_date_k: begin
  read_str (tk, dr);                   {date in YYYY:MM:DD format, UTC}
  if str_ymd(tk, i1, i2, i3) then begin {able to extract year, month, and day ?}
    date.year := i1;
    date.month := i2 - 1;
    date.day := i3 - 1;
    dateset := true;                   {indicate year/month/day set in DATE}
    end;

  if dr.debug >= 5 then begin
    writeln ('    GPS date: ', tk.str:tk.len);
    end;
  end;
{
********************
*
*   GPS tag DIFF
}
gps_diff_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    writeln ('    GPS diff: ', i1);
    end;
  end;
{
********************
*
*   Unexpected GPS tag type.
}
otherwise
    writeln ('    GPS tag ', dr.tag, ' type ', dr.dtype, ' count ', dr.cnt);
    end;
  goto loop_ent;                       {back to do next directory entry}
{
********************
*
*   Done with all GPS directory entries.
}
done_ents:                             {done reading all directory entries}
  dir_close (dr);                      {end reading of the GPS directory}
  dir_rest (dr, rddir);                {restore to directory state found on entry}

  if not (dateset and timeset)         {didn't get full GPS time ?}
    then goto done_time;
  tgps := sys_clock_from_date (date);  {make absolute time specified by the GPS}

  if                                   {use EXIF and GPS time to infer time zone ?}
      (imgflag_time_k in dr.flags) and {date/time has been set ?}
      (imgflag_date_k in dr.flags) and
      (imgflag_texif_k in dr.flags) and {time is from EXIF data ?}
      (imgflag_tzrel_k in dr.flags) and {time is relative to TZ ?}
      (not (imgflag_tz_k in dr.flags)) {but that timezone is not known ?}
      then begin
    {
    *   The current time has been filled in from EXIF data.  It is the local
    *   time when the image was made, but that local timezone is not known.
    *   The GPS time is the absolute time to within a second.  The EXIF local
    *   time and the GPS absolute time are subtracted to infer the local time
    *   zone offset.
    }
    dr.date.hours_west := 0.0;         {set time zone to UTC}
    dr.date.tzone_id := sys_tzone_cut_k;
    dr.date.daysave := sys_daysave_no_k;
    dr.date.daysave_on := false;
    texif := sys_clock_from_date (dr.date); {make time as if in UTC}
    r1 := sys_clock_to_fp2( sys_clock_sub(tgps, texif) ); {seconds offset from UTC}
    if abs(r1) > (3600.0 * 12.0 + 10.0) {offset too large to be just time zone}
      then goto done_autotz;
    i1 := round(r1 / 900.0);           {round to nearest multiple of 15 minutes}
    r2 := r1 - (i1 * 900);             {delta from nearest 15 minute multiple}
    if abs(r2) > 1.5 then goto done_autotz; {not multiple of 15 minutes ?}
    dr.date.hours_west := i1 / 4.0;    {set local time zone offset}
    dr.date.tzone_id := sys_tzone_other_k; {specific time zone not known}
    dr.flags := dr.flags + [
      imgflag_tzrel_k,                 {time is relative to the indicated time zone}
      imgflag_tz_k];                   {the image local time zone is known}
    end;
done_autotz:                           {done trying to infer time zone from EXIF and GPS data}

  if                                   {use EXIF time over GPS time ?}
      (imgflag_time_k in dr.flags) and {time and date are set ?}
      (imgflag_date_k in dr.flags) and
      (imgflag_exif_sfrac_k in dr.flags) and {EXIF data provided seconds fraction ?}
      (imgflag_tz_k in dr.flags)       {the time zone is known ?}
      then begin
    if imgflag_tzrel_k in dr.flags     {time is relative to known time zone ?}
      then goto done_time;             {everything is already set}
    {
    *   We want to use the EXIF time currently in DR.DATE, but it is not
    *   relative to its time zone.
    }
    date.hours_west := dr.date.hours_west; {save target timezone in DATE}
    date.tzone_id := dr.date.tzone_id;
    date.daysave := dr.date.daysave;
    date.daysave_on := dr.date.daysave_on;
    dr.date.hours_west := 0.0;         {set IMAGE.DATE to UTC}
    dr.date.tzone_id := sys_tzone_cut_k;
    dr.date.daysave := sys_daysave_no_k;
    dr.date.daysave_on := false;
    texif := sys_clock_from_date (dr.date); {make absolute image time}
    sys_clock_to_date (                {expand to date/time in image time zone}
      texif,                           {the absolute image time}
      date.tzone_id,                   {time zone ID}
      date.hours_west,                 {hours west offset of the target time zone}
      date.daysave,                    {time zone daylight savings strategy}
      dr.date);                        {returned expanded date/time descriptor}
    dr.flags := dr.flags + [
      imgflag_tzrel_k];                {the date/time is relative to this time zone}
    goto done_time;
    end;
{
*   Set the time from the GPS time, although the local image timezone is used if
*   it is known.
}
  dr.flags := dr.flags - [
    imgflag_tzrel_k];                  {existing date/time won't be used}
  if imgflag_tz_k in dr.flags then begin {local image timezone is in IMAGE.DATE ?}
    date.hours_west := dr.date.hours_west; {save target timezone in DATE}
    date.tzone_id := dr.date.tzone_id;
    date.daysave := dr.date.daysave;
    date.daysave_on := dr.date.daysave_on;
    dr.flags := dr.flags + [
      imgflag_tzrel_k];                {will be relative to image local time zone}
    end;
  sys_clock_to_date (                  {expand GPS time into IMAGE.DATE}
    tgps,                              {the absolute time to expand}
    date.tzone_id,                     {time zone ID}
    date.hours_west,                   {hours west offset of the target time zone}
    date.daysave,                      {time zone daylight savings strategy}
    dr.date);                          {returned expanded date/time descriptor}
  dr.flags := dr.flags - [
    imgflag_texif_k,                   {time is not from EXIF data}
    imgflag_exif_sfrac_k];             {fractional seconds not from EXIF data}
  dr.flags := dr.flags + [
    imgflag_date_k,                    {date it set}
    imgflag_time_k,                    {time is set}
    imgflag_tgps_k];                   {date/time is from GPS data}
done_time:                             {done fixing up image to to GPS time info}

  end;
{
********************************************************************************
*
*   Local subroutine NEXT_IMAGE (DI, DR, STAT)
*
*   Search forwards to find the next suitable image.  The data describing
*   the image will be put into DR.IMAGE.  It is assumed that
*   DR.READ_DIR_P is pointing to the file offset word of where the
*   next directory is stored.  DI is the IMG library private data block
*   for this image file connection.  DR is our own private data block for
*   this TIFF file connection.
}
procedure next_image (
  in out  di: img_conn2_t;             {IMG library private data for this conn}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  out     stat: sys_err_t);            {returned completion status code}
  internal;

const
  max_msg_parms = 2;                   {max parameters we can pass to a message}
  max_subifd = 8;                      {max number of sub-IFDs supported}

var
  i, j: sys_int_machine_t;             {scratch integers and loop counters}
  i1, i2, i3: sys_int_machine_t;       {scratch integers and tag parameters}
  lines_left: sys_int_machine_t;       {scratch scan lines left}
  lines_strip: sys_int_machine_t;      {scan lines in this strip}
  iadr_p, jadr_p: sys_int_adr_p_t;     {scratch pointers to address integers}
  ent_p: lut_ent_p_t;                  {scratch LUT entry pointer}
  sz: sys_int_adr_t;                   {scratch memory size, etc.}
  ofs: sys_int_adr_t;                  {scratch file offset}
  strip_p: strip_p_t;                  {scratch pointer to strip data array entry}
  strip_ofs_p: sys_int_adr_p_t;        {points to first in list of strip offsets}
  strip_len_p: sys_int_adr_p_t;        {points to first in list of strip sizes}
  n_strip_vals: sys_int_machine_t;     {number of values at STRIP_OFS_P^}
  read_save_p: univ_ptr;               {saved copy of read pointer for entry data}
  cntlft_save: sys_int_machine_t;      {saved copy of CNTLFT}
  den_mult: real;                      {density mult value for gray response curve}
  r, r2: real;                         {scratch floating point values}
  res_h, res_v: real;                  {horizontal and vertical resolution values}
  res_h_set, res_v_set: boolean;       {TRUE if horiz/vert resolution set}
  bits_comp_set: boolean;              {TRUE if bits/component explicitly set}
  punt_image: boolean;                 {TRUE if current image not acceptable}
  tcolor: boolean;                     {scratch TRUECOLOR flag}
  tagname: string_var256_t;            {name of current entry and data values}
  time: sys_clock_t;                   {scratch time descriptor}
  token, token2: string_var256_t;      {scratch strings}
  hcomm: boolean;                      {new comment should be written to header}
  subifd:                              {list of offsets to sub-IFDs found}
    array[1..max_subifd] of sys_int_adr_t;
  nsubifd: sys_int_machine_t;          {number of entries in SUBIFD}
  dirsave: rddir_state_t;              {saved state when reading a subdirectory}
  tzone: sys_tzone_k_t;                {time zone}
  hwest: real;                         {time zone hours west}
  daysave: sys_daysave_k_t;            {daylight savings time strategy}
  insubdir: boolean;                   {in a subdirectory}
  shown: boolean;                      {debug information already shown for this tag}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  next_image_loop, entry_loop, add_comment_string, dump_int, done_entry,
  done_entry_dbg, done_entries, done_flen;

begin
  tagname.max := sizeof(tagname.str);  {init local var strings}
  token.max := sizeof(token.str);
  token2.max := sizeof(token2.str);
  nsubifd := 0;                        {init to no unread sub-IFDs in the list}
  insubdir := false;                   {init to not in a subdirectory}

next_image_loop:                       {back here when reject last image}
  if dr.dir_n > 0 then begin           {IMAGE previously used ?}
    util_mem_context_del (dr.image.mem_p); {delete all memory of old image}
    end;

  if insubdir then begin               {was reading a subdirectory ?}
    dir_rest (dr, dirsave);            {restore state to main directory}
    insubdir := false;                 {no longer in the subdirectory}
    end;
{
*   Init local state for reading this image.
}
  res_h_set := false;                  {horizontal resolution not explicitly set}
  res_v_set := false;                  {vertical resolution not explicitly set}
  punt_image := false;                 {init to image may be acceptable}
  strip_ofs_p := nil;                  {init to no STRIP_OFFSETS read yet}
  strip_len_p := nil;                  {init to no STRIP_BYTE_COUNTS read yet}
  n_strip_vals := 0;                   {init number of strip values pointed to}
  den_mult := 0.01;                    {default resolution for gray response curve}
  bits_comp_set := false;              {init to bits/component not explicitly set}
{
*   Init the fields in IMAGE to default values.
}
  util_mem_context_get (di.mem_p^, dr.image.mem_p); {get mem context for this image}
  dr.image.bits_red := 1;
  dr.image.bits_grn := 0;
  dr.image.bits_blu := 0;
  dr.image.bits_comp4 := 0;
  dr.image.bits_comp5 := 0;
  dr.image.bits_comp6 := 0;
  dr.image.bits_comp7 := 0;
  dr.image.bits_comp8 := 0;
  dr.image.bits_stored := 0;           {init to stored and usable bits the same}
  dr.image.ncomp := 0;
  dr.image.ncomp_color := 0;
  dr.image.comp_alpha := 0;
  dr.image.compress := compress_none_k;
  dr.image.x_size := 0;
  dr.image.y_size := 0;
  dr.image.subfile := 0;
  dr.image.photometric := 0;
  dr.image.truecolor := false;
  dr.image.rgb_sep := false;
  dr.image.alpha_premult := true;
  dr.image.predictor := pred_none_k;
  dr.image.rows_strip := rshft(lastof(dr.image.rows_strip), 1);
  dr.image.strip0_p := nil;
  dr.image.aspect := 0.0;
  dr.image.lut_max := 1;
  dr.image.red_max := 1;
  dr.image.grn_max := 0;
  dr.image.blu_max := 0;
  dr.image.lut_p := nil;               {indicate no LUT created yet}
  dr.image.cfadimx := 0;
  dr.image.cfadimy := 0;
{
*   Set up for reading successive directory entries.
}
  if nsubifd = 0
    then begin                         {no unread sub-IFDs in the list ?}
      next_dir (dr, stat);             {go to new directory and read header}
      if sys_error(stat) then return;
      end
    else begin                         {set up for reading a subdirectory}
      dir_save (dr, dirsave);          {save existing directory reading state}
      ofs := subifd[1];                {get offset to next subdirectory}
      dir_open (dr, ofs, stat);        {open the subdirectory}
      if sys_error(stat) then return;
      nsubifd := nsubifd - 1;          {count one less pending subdirectory}
      for i := 1 to nsubifd do begin   {move others up to fill the hole}
        subifd[i] := subifd[i+1];
        end;
      insubdir := true;                {remember we are in a subdirectory}
      if dr.debug >= 1 then begin
        writeln ('Reading subdirectory at ', ofs, ', ', dr.ents_left, ' entries:');
        end;
      end
    ;
{
*   Main loop.  Come back here to process each new entry in this directory.
}
entry_loop:
  next_entry (dr, stat);               {read header of next directory entry}
  if file_eof(stat) then goto done_entries; {hit end of current directory ?}
  if sys_error(stat) then return;      {a real error on reading entry ?}
  shown := false;                      {init to no debug info shown for this tag}
  read_save_p := dr.read_p;            {save state for reading start of data}
  cntlft_save := dr.cntlft;

  if dr.debug >= 5 then begin
    case dr.tag of                     {which tag is this ?}
f_subfile_type_k: string_vstring (tagname, 'subfile_type'(0), -1);
f_image_width_k: string_vstring (tagname, 'image_width'(0), -1);
f_image_length_k: string_vstring (tagname, 'image_length'(0), -1);
f_bits_per_sample_k: string_vstring (tagname, 'bits_per_sample'(0), -1);
f_thresholding_k: string_vstring (tagname, 'thresholding'(0), -1);
f_cell_width_k: string_vstring (tagname, 'cell_width'(0), -1);
f_cell_length_k: string_vstring (tagname, 'cell_length'(0), -1);
f_fill_order_k: string_vstring (tagname, 'fill_order'(0), -1);
f_document_name_k: string_vstring (tagname, 'document_name'(0), -1);
f_image_description_k: string_vstring (tagname, 'image_description'(0), -1);
f_make_k: string_vstring (tagname, 'make'(0), -1);
f_model_k: string_vstring (tagname, 'model'(0), -1);
f_strip_offsets_k: string_vstring (tagname, 'strip_offsets'(0), -1);
f_orientation_k: string_vstring (tagname, 'orientation'(0), -1);
f_samples_per_pixel_k: string_vstring (tagname, 'samples_per_pixel'(0), -1);
f_rows_per_strip_k: string_vstring (tagname, 'rows_per_strip'(0), -1);
f_strip_byte_counts_k: string_vstring (tagname, 'strip_byte_counts'(0), -1);
f_min_sample_value_k: string_vstring (tagname, 'min_sample_value'(0), -1);
f_max_sample_value_k: string_vstring (tagname, 'max_sample_value'(0), -1);
f_x_resolution_k: string_vstring (tagname, 'x_resolution'(0), -1);
f_y_resolution_k: string_vstring (tagname, 'y_resolution'(0), -1);
f_page_name_k: string_vstring (tagname, 'page_name'(0), -1);
f_x_position_k: string_vstring (tagname, 'x_position'(0), -1);
f_y_position_k: string_vstring (tagname, 'y_position'(0), -1);
f_free_offsets_k: string_vstring (tagname, 'free_offsets'(0), -1);
f_free_byte_counts_k: string_vstring (tagname, 'free_byte_counts'(0), -1);
f_gray_response_unit_k: string_vstring (tagname, 'gray_response_unit'(0), -1);
f_gray_response_curve_k: string_vstring (tagname, 'gray_response_curve'(0), -1);
f_group3_options_k: string_vstring (tagname, 'group3_options'(0), -1);
f_group4_options_k: string_vstring (tagname, 'group4_options'(0), -1);
f_page_number_k: string_vstring (tagname, 'page_number'(0), -1);
f_color_repsonse_curves_k: string_vstring (tagname, 'color_repsonse_curves'(0), -1);
f_software_k: string_vstring (tagname, 'software'(0), -1);
f_date_time_k: string_vstring (tagname, 'date_time'(0), -1);
f_artist_k: string_vstring (tagname, 'artist'(0), -1);
f_host_computer_k: string_vstring (tagname, 'host_computer'(0), -1);
f_white_point_k: string_vstring (tagname, 'white_point'(0), -1);
f_primary_chromaticities_k: string_vstring (tagname, 'primary_chromaticities'(0), -1);
f_color_map_k: string_vstring (tagname, 'color_map'(0), -1);
f_halftone_hints_k: string_vstring (tagname, 'halftone_hints'(0), -1);
f_tile_width_k: string_vstring (tagname, 'tile_width'(0), -1);
f_tile_length_k: string_vstring (tagname, 'tile_length'(0), -1);
f_tile_offsets_k: string_vstring (tagname, 'tile_offsets'(0), -1);
f_tile_byte_counts_k: string_vstring (tagname, 'tile_byte_counts'(0), -1);
f_subifd_k: string_vstring (tagname, 'subIFD'(0), -1);
f_ink_sets_k: string_vstring (tagname, 'ink_sets'(0), -1);
f_ink_names_k: string_vstring (tagname, 'ink_names'(0), -1);
f_number_of_inks_k: string_vstring (tagname, 'number_of_inks'(0), -1);
f_dot_range_k: string_vstring (tagname, 'dot_range'(0), -1);
f_target_printer_k: string_vstring (tagname, 'target_printer'(0), -1);
f_extra_samples_k: string_vstring (tagname, 'extra_samples'(0), -1);
f_sample_format_k: string_vstring (tagname, 'sample_format'(0), -1);
f_smin_sample_value_k: string_vstring (tagname, 'smin_sample_value'(0), -1);
f_smax_sample_value_k: string_vstring (tagname, 'smax_sample_value'(0), -1);
f_transfer_range_k: string_vstring (tagname, 'transfer_range'(0), -1);
f_jpeg_proc_k: string_vstring (tagname, 'jpeg_proc'(0), -1);
f_jpeg_interchange_fmt_k: string_vstring (tagname, 'jpeg_interchange_fmt'(0), -1);
f_jpeg_interchange_fmt_len_k: string_vstring (tagname, 'jpeg_interchange_fmt_len'(0), -1);
f_jpeg_restart_interval_k: string_vstring (tagname, 'jpeg_restart_interval'(0), -1);
f_jpeg_lossless_predictors_k: string_vstring (tagname, 'jpeg_lossless_predictors'(0), -1);
f_jpeg_point_transforms_k: string_vstring (tagname, 'jpeg_point_transforms'(0), -1);
f_jpeg_q_tables_k: string_vstring (tagname, 'jpeg_q_tables'(0), -1);
f_jpeg_dc_tables_k: string_vstring (tagname, 'jpeg_dc_tables'(0), -1);
f_jpeg_ac_tables_k: string_vstring (tagname, 'jpeg_ac_tables'(0), -1);
f_ycbcr_coeficients_k: string_vstring (tagname, 'ycbcr_coeficients'(0), -1);
f_ycbcr_sub_sampling_k: string_vstring (tagname, 'ycbcr_sub_sampling'(0), -1);
f_ycbcr_positioning_k: string_vstring (tagname, 'ycbcr_positioning'(0), -1);
f_reference_black_white_k: string_vstring (tagname, 'reference_black_white'(0), -1);
f_xmp_k: begin
  writeln ('  XMP metadata, count ', dr.cnt, ':');
  while dr.cntlft > 0 do begin
    read_text (token, dr);             {get line of text}
    if token.len > 0 then begin
      writeln ('    ', token.str:token.len);
      end;
    end;
  goto done_entry_dbg;
  end;
f_exif_ifd_k: string_vstring (tagname, 'EXIF IFD'(0), -1);
f_iccprofile_k: string_vstring (tagname, 'ICC profile'(0), -1);
f_gps_ifd_k: string_vstring (tagname, 'GPS info IFD'(0), -1);
otherwise
      goto done_entry_dbg;
      end;                             {end of specific debug output cases}

    shown := true;                     {debug info will be shown for this tag}
    if (dr.dtype <> type_ascii_k) and (dr.cnt > 1)
      then begin                       {show count, data on next line}
        string_appends (tagname, ', count '(0));
        string_f_int (token, dr.cnt);
        string_append (tagname, token);
        string_append1 (tagname, ':');
        writeln ('  ', tagname.str:tagname.len);
        end
      else begin
        string_appendn (tagname, ' =', 2);
        write ('  ', tagname.str:tagname.len);
        end
      ;
    if dr.dtype = type_ascii_k then begin {data value is a string ?}
      read_str (token, dr);            {get the string}
      writeln (' "', token.str:token.len, '"');
      goto done_entry_dbg;
      end;
    if dr.cnt > 1 then begin           {going to write list of values ?}
      write ('   ');                   {indent}
      end;
    for i := 1 to min(dr.cnt, 8) do begin {once for each data value to write}
      if isint(dr)
        then begin                     {the data value is integer}
          string_f_int (token, read_i(dr));
          end
        else begin                     {not integer, get as floating point}
          string_f_fp_free (token, read_fp(dr), 5);
          end
        ;
      write (' ', token.str:token.len);
      end;
    if dr.cnt > 8 then begin           {not all data values written ?}
      write (' ...');                  {indicate more data values exist}
      end;
    writeln;

done_entry_dbg:
    dr.read_p := read_save_p;          {reset state for reading start of data}
    dr.cntlft := cntlft_save;
    end;                               {end of debug code}

  case dr.tag of                       {which tag is this ?}
{
****************************************
*
*   TAG = new_subfile_type
}
f_new_subfile_type_k: begin
  i1 := read_i(dr);
  dr.image.subfile := i1;

  if dr.debug >= 5 then begin
    write ('  New subfile type: ', i1);
    i2 := 0;
    if i1 = 0 then begin
      write (' (normal image)');
      i2 := 1;
      end;
    if (i1 & 1) <> 0 then begin
      if i2 = 0
        then write (' (')
        else write (', ');
      i2 := 1;
      write ('reduced');
      end;
    if (i1 & 2) <> 0 then begin
      if i2 = 0
        then write (' (')
        else write (', ');
      i2 := 1;
      write ('page');
      end;
    if (i1 & 4) <> 0 then begin
      if i2 = 0
        then write (' (')
        else write (', ');
      i2 := 1;
      write ('mask');
      end;
    if i1 <> 0 then begin
      write (')');
      end;
    writeln;
    end;
  end;
{
****************************************
*
*   TAG = subfile_type  (old)
}
f_subfile_type_k: begin
  i := read_i(dr);                     {get old subfield type ID}
  case i of
oldtype_full_k: dr.image.subfile := 0;
oldtype_reduced_k: dr.image.subfile := subtype_reduced_k;
oldtype_page_k: dr.image.subfile := subtype_page_k;
otherwise
    sys_msg_parm_int (msg_parm[1], i);
    sys_message_bomb ('img', 'tif_subfield_type_bad', msg_parm, 1);
    end;
  end;
{
****************************************
*
*   TAG = image_width
}
f_image_width_k: begin
  dr.image.x_size := read_i(dr);
  end;
{
****************************************
*
*   TAG = image_length
}
f_image_length_k: begin
  dr.image.y_size := read_i(dr);
  end;
{
****************************************
*
*   TAG = bits_per_sample
}
f_bits_per_sample_k: begin
  bits_comp_set := true;               {indicate bits/component explicitly set}
  dr.image.ncomp := dr.cnt;            {number of components per pixel}
  dr.image.truecolor := dr.image.ncomp >= 3;
  if dr.image.ncomp >= 1 then dr.image.bits_red := read_i(dr);
  if dr.image.ncomp >= 2 then dr.image.bits_grn := read_i(dr);
  if dr.image.ncomp >= 3 then dr.image.bits_blu := read_i(dr);
  if dr.image.ncomp >= 4 then dr.image.bits_comp4 := read_i(dr);
  if dr.image.ncomp >= 5 then dr.image.bits_comp5 := read_i(dr);
  if dr.image.ncomp >= 6 then dr.image.bits_comp6 := read_i(dr);
  if dr.image.ncomp >= 7 then dr.image.bits_comp7 := read_i(dr);
  if dr.image.ncomp >= 8 then dr.image.bits_comp8 := read_i(dr);
  end;
{
****************************************
*
*   TAG = compression
}
f_compression_k: begin
  i1 := read_i(dr);
  dr.image.compress := i1;

  if dr.debug >= 5 then begin
    write ('  Compression: ', i1);
    case i1 of
1:    write (' (none)');
2:    write (' (CCITT3 TIFF subset)');
3:    write (' (CCITT group 3)');
4:    write (' (CCITT group 4)');
5:    write (' (LZW)');
7:    write (' (TIFF JPEG)');
32773: write (' (runlength)');
      end;
    writeln;
    end;
  end;
{
****************************************
*
*   TAG = photometric_interpretation
}
f_photometric_interpretation_k: begin
  i := read_i(dr);                     {get photometric interpretation ID}

  if dr.debug >= 5 then begin
    write ('  Photometric interpetation: ', i);
    end;

  dr.image.photometric := i;           {save photometric interpretation ID}
  tcolor := false;                     {init to photometric not imply true color}
  create_lut (dr);                     {make sure LUT exists and initialized}

  case i of
photo_wb_k: begin                      {gray scale image, 0 = white, max = black}
      if dr.debug >= 5 then begin
        write (' (white to black)');
        end;
      dr.image.ncomp_color := 1;       {color components/pixel needed for color val}
      ent_p := dr.image.lut_p;         {init current LUT entry to first entry}
      for i := dr.image.lut_max downto 0 do begin {once for each LUT entry}
        j := min(lut_val_max_k, trunc((i / dr.image.lut_max) * (lut_val_max_k+1)));
        ent_p^.red := j;
        ent_p^.grn := j;
        ent_p^.blu := j;
        ent_p := univ_ptr(sys_int_adr_t(ent_p) + sizeof(ent_p^)); {to next LUT entry}
        end;
      end;
photo_bw_k: begin                      {gray scale, 0 = black, max = white}
      if dr.debug >= 5 then begin
        write (' (black to white)');
        end;
      dr.image.ncomp_color := 1;       {color components/pixel needed for color val}
      end;
photo_rgb_k: begin                     {separate RGB color values are given}
      if dr.debug >= 5 then begin
        write (' (RGB)');
        end;
      dr.image.ncomp_color := 3;       {color components/pixel needed for color val}
      tcolor := true;                  {definitely true color}
      end;
photo_pcolor_k: begin                  {psuedo color}
      if dr.debug >= 5 then begin
        write (' (pseudo color)');
        end;
      dr.image.ncomp_color := 1;       {color components/pixel needed for color val}
      end;
photo_mask_k: begin                    {image is 1-bit alpha mask for another image}
      if dr.debug >= 5 then begin
        write (' (1-bit mask)');
        end;
      dr.image.ncomp_color := 1;       {color components/pixel needed for color val}
      punt_image := true;              {we ignore 1-bit alpha masks}
      end;
photo_cmyk_k: begin                    {separated image, usually CMYK}
      if dr.debug >= 5 then begin
        write (' (separated color, like CMYK)');
        end;
      dr.image.ncomp_color := 4;       {color components/pixel needed for color val}
      tcolor := true;
      end;
photo_cfa_k: begin                     {CFA (color filter array)}
      if dr.debug >= 5 then begin
        write (' (CFA, color filter array)');
        end;
      dr.image.ncomp_color := 3;
      tcolor := true;
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], i);
    sys_message_bomb ('img', 'tif_photo_interp_unexpected', msg_parm, 1);
    end;

  dr.image.truecolor := tcolor;        {set final true/pseudo color flag}

  if dr.debug >= 5 then writeln;
  end;
{
****************************************
*
*   TAG = thresholding  (old)
}
f_thresholding_k: begin
  end;
{
****************************************
*
*   TAG = cell_width  (old)
}
f_cell_width_k: begin
  end;
{
****************************************
*
*   TAG = cell_length  (old)
}
f_cell_length_k: begin
  end;
{
****************************************
*
*   TAG = fill_order  (old)
}
f_fill_order_k: begin
  end;
{
****************************************
*
*   TAG = document_name
}
f_document_name_k: begin
  string_vstring (token2, 'Document: '(0), -1);
  hcomm := true;                       {add this comment to the header lines}

add_comment_string:                    {jump here when TOKEN2 is comment line start}
  read_str (token, dr);                {get string from TIFF file}
  if token.len > 0 then begin
    string_append (token2, token);     {assemble full comment string}
    add_comment (dr, hcomm, token2);   {add the comment line to the list}
    end;
  end;
{
****************************************
*
*   TAG = image_description
}
f_image_description_k: begin
  string_vstring (token2, '  ', 2);
  hcomm := false;                      {this is not a header comment}
  goto add_comment_string;
  end;
{
****************************************
*
*   TAG = make
}
f_make_k: begin
  string_vstring (token2, 'Source equipment manufacturer: '(0), -1);
  hcomm := true;                       {add this comment to the header lines}
  goto add_comment_string;
  end;
{
****************************************
*
*   TAG = model
}
f_model_k: begin
  string_vstring (token2, 'Source equipment model: '(0), -1);
  hcomm := true;                       {add this comment to the header lines}
  goto add_comment_string;
  end;
{
****************************************
*
*   TAG = strip_offsets
}
f_strip_offsets_k: begin
  n_strip_vals := dr.cnt;              {get number of strips in list}
  sz := sizeof(strip_ofs_p^) * n_strip_vals; {amount of mem needed to hold list}
  util_mem_grab (                      {allocate memory for strip offsets list}
    sz, dr.image.mem_p^, true, strip_ofs_p);
  iadr_p := strip_ofs_p;               {init pointer to first entry in list}
  for i := 1 to n_strip_vals do begin  {once for each strip offset supplied}
    iadr_p^ := read_i(dr);             {save this strip offset}
    iadr_p := univ_ptr(sys_int_adr_t(iadr_p) + sizeof(iadr_p^)); {point to next val}
    end;
  end;
{
****************************************
*
*   TAG = orientation  (old)
}
f_orientation_k: begin
  end;
{
****************************************
*
*   TAG = samples_per_pixel
}
f_samples_per_pixel_k: begin
  i := read_i(dr);

  if bits_comp_set and (i <> dr.image.ncomp) then begin
    sys_msg_parm_int (msg_parm[1], dr.image.ncomp);
    sys_msg_parm_int (msg_parm[2], i);
    sys_message_bomb ('img', 'tif_ncomp_mismatch_sampix', msg_parm, 2);
    end;

  dr.image.ncomp := i;                 {set number of components per pixel}
  bits_comp_set := true;
  create_lut (dr);                     {create and init LUT if not done already}
  end;
{
****************************************
*
*   TAG = rows_per_strip
}
f_rows_per_strip_k: begin
  dr.image.rows_strip := read_i(dr);
  end;
{
****************************************
*
*   TAG = strip_byte_counts
}
f_strip_byte_counts_k: begin
  if dr.cnt <> n_strip_vals then begin
    sys_msg_parm_int (msg_parm[1], dr.cnt);
    sys_msg_parm_int (msg_parm[2], n_strip_vals);
    sys_message_bomb ('img', 'tif_strip_byte_cnt_bad', msg_parm, 2);
    end;
  sz := sizeof(strip_len_p^) * n_strip_vals; {amount of mem needed to hold list}
  util_mem_grab (                      {allocate memory for strip offsets list}
    sz, dr.image.mem_p^, true, strip_len_p);
  iadr_p := strip_len_p;               {init pointer to first entry in list}
  for i := 1 to n_strip_vals do begin  {once for each strip length supplied}
    iadr_p^ := read_i(dr);             {save this strip length}
    iadr_p := univ_ptr(sys_int_adr_t(iadr_p) + sizeof(iadr_p^)); {point to next val}
    end;
  end;
{
****************************************
*
*   TAG = min_sample_value  (old)
}
f_min_sample_value_k: begin
  end;
{
****************************************
*
*   TAG = max_sample_value  (old)
}
f_max_sample_value_k: begin
  end;
{
****************************************
*
*   TAG = x_resolution
}
f_x_resolution_k: begin
  res_h := read_fp(dr);
  res_h_set := true;
  end;
{
****************************************
*
*   TAG = y_resolution
}
f_y_resolution_k: begin
  res_v := read_fp(dr);
  res_v_set := true;
  end;
{
****************************************
*
*   TAG = planar_configuration
}
f_planar_configuration_k: begin
  i := read_i(dr);

  if dr.debug >= 5 then begin
    write ('  Planar configuration: ', i);
    end;
  case i of
planar_pix_k: begin
      dr.image.rgb_sep := false;
      if dr.debug >= 5 then begin
        writeln (' (whole pixels together)');
        end;
      end;
planar_samp_k: begin
      dr.image.rgb_sep := true;
      if dr.debug >= 5 then begin
        writeln (' (components in separate strips)');
        end;
      end;
otherwise
    if dr.debug >= 5 then writeln;
    sys_msg_parm_int (msg_parm[1], i);
    sys_message_bomb ('img', 'tif_planar_unexpected', msg_parm, 1);
    end;
  end;
{
****************************************
*
*   TAG = page_name
}
f_page_name_k: begin
  string_vstring (token2, 'Page Name: '(0), -1);
  hcomm := true;                       {add this comment to the header lines}
  goto add_comment_string;
  end;
{
****************************************
*
*   TAG = x_position
}
f_x_position_k: begin
  end;
{
****************************************
*
*   TAG = y_position
}
f_y_position_k: begin
  end;
{
****************************************
*
*   TAG = free_offsets  (old)
}
f_free_offsets_k: begin
  end;
{
****************************************
*
*   TAG = free_byte_counts  (old)
}
f_free_byte_counts_k: begin
  end;
{
****************************************
*
*   TAG = gray_response_unit
}
f_gray_response_unit_k: begin
  den_mult := read_i(dr);
  r := 10.0;
  sys_dummy (den_mult);                {for compiler bug on DN1000}
  sys_dummy (r);
  den_mult := r ** den_mult;
  sys_dummy (den_mult);
  den_mult := 1.0 / den_mult;
  end;
{
****************************************
*
*   TAG = gray_response_curve
}
f_gray_response_curve_k: begin
  create_lut (dr);                     {make sure LUT exists}
  ent_p := dr.image.lut_p;             {init current LUT entry to first entry}
  for i := 0 to dr.image.lut_max do begin {once for each LUT entry}
    r := read_i(dr) * den_mult;        {get log10 density value}
    r := min(5.0, max(0.0, r));        {clip to useful range}
    sys_dummy (r);                     {for compiler bug on DN1000}
    r := 10 ** (-r);                   {make mult factor from max brightness}
    sys_dummy (r);
    j := min(lut_val_max_k, trunc(r * (lut_val_max_k+1)));
    ent_p^.red := j;
    ent_p^.grn := j;
    ent_p^.blu := j;
    ent_p := univ_ptr(sys_int_adr_t(ent_p) + sizeof(ent_p^)); {to next LUT entry}
    end;
  end;
{
****************************************
*
*   TAG = group3_options
}
f_group3_options_k: begin
  end;
{
****************************************
*
*   TAG = group4_options
}
f_group4_options_k: begin
  end;
{
****************************************
*
*   TAG = resolution_unit
}
f_resolution_unit_k: begin
  i1 := read_i(dr);

  if dr.debug >= 5 then begin
    write ('  Resolution unit: ', i1);
    case i1 of
1:    write (' (no absolute resolution)');
2:    write (' (inch)');
3:    write (' (centimeter)');
      end;
    writeln;
    end;
  end;
{
****************************************
*
*   TAG = page_number
}
f_page_number_k: begin
  end;
{
****************************************
*
*   TAG = color_repsonse_curves
}
f_color_repsonse_curves_k: begin
  end;
{
****************************************
*
*   TAG = software
}
f_software_k: begin
  string_vstring (token2, 'Source software: '(0), -1);
  hcomm := true;                       {add this comment to the header lines}
  goto add_comment_string;
  end;
{
****************************************
*
*   TAG = date_time
}
f_date_time_k: begin
  read_str(token, dr);                 {get the date/time string}
  if str_date (token, dr.date) then begin {successfully extracted info into DATE ?}
    dr.flags := dr.flags + [imgflag_date_k, imgflag_time_k];
    end;
  end;
{
****************************************
*
*   TAG = artist
}
f_artist_k: begin
  string_vstring (token2, 'Created by: '(0), -1);
  hcomm := true;                       {add this comment to the header lines}
  goto add_comment_string;
  end;
{
****************************************
*
*   TAG = host_computer
}
f_host_computer_k: begin
  string_vstring (token2, 'Host computer: '(0), -1);
  hcomm := true;                       {add this comment to the header lines}
  goto add_comment_string;
  end;
{
****************************************
*
*   TAG = predictor
}
f_predictor_k: begin
  i1 := read_i(dr);
  dr.image.predictor := i1;

  if dr.debug >= 5 then begin
    write ('  Predictor: ', i1);
    case i1 of
1:    write (' (none)');
2:    write (' (horizontal difference)');
      end;
    writeln;
    end;
  end;
{
****************************************
*
*   TAG = white_point
}
f_white_point_k: begin
  end;
{
****************************************
*
*   TAG = primary_chromaticities
}
f_primary_chromaticities_k: begin
  end;
{
****************************************
*
*   TAG = color_map
}
f_color_map_k: begin
  create_lut (dr);                     {make sure LUT exists}

  ent_p := dr.image.lut_p;
  for i := 0 to dr.image.red_max do begin {loop over all red LUT values}
    ent_p^.red := read_i(dr);
    ent_p := univ_ptr(sys_int_adr_t(ent_p) + sizeof(ent_p^)); {to next LUT entry}
    end;

  ent_p := dr.image.lut_p;
  for i := 0 to dr.image.red_max do begin {loop over all green LUT values}
    ent_p^.grn := read_i(dr);
    ent_p := univ_ptr(sys_int_adr_t(ent_p) + sizeof(ent_p^)); {to next LUT entry}
    end;

  ent_p := dr.image.lut_p;
  for i := 0 to dr.image.red_max do begin {loop over all blue LUT values}
    ent_p^.blu := read_i(dr);
    ent_p := univ_ptr(sys_int_adr_t(ent_p) + sizeof(ent_p^)); {to next LUT entry}
    end;
  end;
{
****************************************
*
*   TAG = subifd
}
f_subifd_k: begin
  for j := 1 to dr.cnt do begin        {once for each subIFD}
    if nsubifd < max_subifd then begin {there is room in subIFD list ?}
      nsubifd := nsubifd + 1;          {count one more subIFD in the list}
      subifd[nsubifd] := read_i(dr);   {save offset to this subIFD}
      end;
    end;
  end;
{
****************************************
*
*   TAG = extra_samples
}
f_extra_samples_k: begin
  j := dr.cnt;                         {get number of extra components described}
  if j + dr.image.ncomp_color <> dr.image.ncomp then begin
    if dr.debug >= 1 then begin
      writeln ('Wrong number of extra samples, ',
        j, ' found, ', dr.image.ncomp - dr.image.ncomp_color, ' expected.');
      end;
    j := min(j, dr.image.ncomp - dr.image.ncomp_color); {clip to allowable limit}
    end;

  for i := 1 to j do begin             {once for each extra component}
    case read_i(dr) of                 {what is the meaning of this component ?}
extra_alpha_k: begin                   {opacity, color is pre-multiplied}
        dr.image.comp_alpha := i + dr.image.ncomp_color; {save alpha component num}
        dr.image.alpha_premult := true; {indicate colors are pre-multiplie by alpha}
        end;
extra_alpha_sep_k: begin               {opacity, color is not pre-multiplied}
        if dr.image.comp_alpha = 0 then begin {not already have an alpha channel ?}
          dr.image.comp_alpha := i + dr.image.ncomp_color; {save alpha component num}
          dr.image.alpha_premult := false; {colors are not pre-multiplie by alpha}
          end;
        end;                           {done with separate alpha component}
      end;                             {done with extra component meaning cases}
    end;                               {back to process meaning of next extra sample}
  end;                                 {end of EXTRA_SAMPLE tag case}
{
****************************************
*
*   TAG = CFA dimensions
}
f_cfadim_k: begin
  i1 := read_i(dr);                    {pattern width in X}
  i2 := read_i(dr);                    {pattern width in Y}
  if (i1 <= cfa_dim_max_k) and (i2 <= cfa_dim_max_k) then begin
    dr.image.cfadimx := i1;
    dr.image.cfadimy := i2;
    dr.cfal_last := (dr.image.cfadimy div 2) * 2;
    end;

  if dr.debug >= 5 then begin
    writeln ('  CFA repeating pattern size: ', i1, ' x ', i2);
    end;
  end;
{
****************************************
*
*   TAG = CFA pattern
}
f_cfapatt_k: begin
  if dr.debug >= 5 then begin
    writeln ('  CFA pattern, count ', dr.cnt, ':');
    end;

  if (dr.cnt <> (dr.image.cfadimx * dr.image.cfadimy)) {not expected pattern size ?}
      then begin
    dr.image.cfadimx := 0;             {explicitly set CFA pattern to unknown}
    dr.image.cfadimy := 0;
    goto dump_int;
    end;

  for i2 := 0 to dr.image.cfadimy-1 do begin {down the pattern rows}
    if dr.debug >= 5 then begin
      write ('   ');
      end;
    for i1 := 0 to dr.image.cfadimx-1 do begin {accross this row}
      i3 := read_i(dr);                {get ID for this color}
      dr.image.cfapatt[i1, i2] := i3;  {save it in the array}
      if dr.debug >= 5 then begin
        case i3 of
0:        write (' red');
1:        write (' grn');
2:        write (' blu');
3:        write (' cya');
4:        write (' mag');
5:        write (' yel');
6:        write (' wht');
otherwise
          write (i3:4);
          end;
        end;
      end;                             {back for next pixel in this row}
    if dr.debug >= 5 then writeln;
    end;                               {back for next pattern row down}
  goto done_entry;

dump_int:                              {dump remaining integer values}
  if dr.debug >= 5 then begin
    i2 := dr.cntlft;
    if i2 <= 0 then goto done_entry;
    write ('   ');
    for i1 := 1 to min(8, i2) do begin
      write (' ', read_i(dr));
      end;
    if i2 > 8 then begin
      write (' ...');
      end;
    writeln;
    end;

  end;                                 {end of CFA PATTERN tag case}
{
****************************************
*
*   TAG = copyright
}
f_copyright_k: begin
  read_str (token, dr);                {get the copyright string}
  if dr.debug >= 5 then begin
    writeln ('  Copyright: "', token.str:token.len, '"');
    end;

  if token.len > 0 then begin
    string_vstring (token2, 'Copyright: '(0), -1);
    string_append (token2, token);
    add_comment (dr, true, token2);
    end;
  end;
{
****************************************
*
*   TAG = exif_ifd
}
f_exif_ifd_k: begin
  ofs := read_i(dr);                   {get the offset to the EXIF directory}
  dir_exif (dr, ofs, stat);            {read the EXIF directory and come back}
  if sys_error(stat) then return;
  end;
{
****************************************
*
*   TAG = ICC profile
}
{
****************************************
*
*   TAG = gps_ifd
}
f_gps_ifd_k: begin
  ofs := read_i(dr);                   {save offset to the IFD}
  dir_gps (dr, ofs, stat);             {read the GPS IFD and come back}
  if sys_error(stat) then return;
  end;
{
****************************************
*
*   TAG = original creation date/time
}
f_dtm_orig_k: begin
  read_str (token, dr);                {date/time, format YYYY:MM:DD HH:MM:SS}

  if dr.debug >= 5 then begin
    writeln ('  Original creation date/time: ', token.str:token.len);
    end;

  if
      (not (imgflag_texif_k in dr.flags)) and {don't have EXIF orig time ?}
      (not (imgflag_tgps_k in dr.flags)) {dont have GPS time ?}
      then begin
    if str_date (token, dr.date) then begin {set time from this tag ?}
      dr.flags := dr.flags + [
        imgflag_time_k,                {time within day has been set}
        imgflag_date_k,                {date has been set}
        imgflag_tzrel_k];              {this date/time is relative to timezone}
      end;
    end;
  end;
{
****************************************
*
*   TAG = TIFF/EP ID
}
f_ep_stdid_k: begin
  if dr.debug >= 5 then begin
    write ('  TIFF/EP version ID: ');
    while dr.cntlft > 0 do begin
      if dr.cntlft <> dr.cnt then write ('.');
      i1 := read_i(dr);
      write (i1);
      end;
    writeln;
    end;
  end;
{
****************************************
*
*   TAG = sensing method
}
f_senmeth_k: begin
  i1 := read_i(dr);                    {get sensing method ID}

  if dr.debug >= 5 then begin
    write ('  Sensor layout type: ', i1);
    case i1 of
0:    write (' (undefined)');
1:    write (' (monochrome area)');
2:    write (' (1-chip color area)');
3:    write (' (2-chip color area)');
4:    write (' (3-chip color area)');
5:    write (' (color sequential area)');
6:    write (' (monochrome linear)');
7:    write (' (tri-linear)');
8:    write (' (color sequential linear)');
      end;
    writeln;
    end;
  end;
{
****************************************
*
*   TAG value is not recongnized.
}
otherwise
    if dr.debug < 5 then goto done_entry; {silently ignore this tag in production mode}
    if shown then goto done_entry;     {debug information already shown for this tag ?}

    write ('  Unknown tag ', dr.tag, ', type ', dr.dtype, ', count ', dr.cnt, ':');

    if dr.dtype = type_ascii_k then begin {data value is a string ?}
      writeln;
      read_str (token, dr);            {get the string}
      writeln ('    "', token.str:token.len, '"');
      goto done_entry;
      end;

    if dr.cnt > 1 then begin           {going to write list of values ?}
      writeln;                         {end original line}
      write ('   ');                   {indent for subordinate line}
      end;
    for i := 1 to min(dr.cnt, 8) do begin {once for each data value to write}
      if isint(dr)
        then begin                     {the data value is integer}
          string_f_int (token, read_i(dr));
          end
        else begin                     {not integer, get as floating point}
          string_f_fp_free (token, read_fp(dr), 5);
          end
        ;
      write (' ', token.str:token.len);
      end;
    if dr.cnt > 8 then begin           {not all data values written ?}
      write (' ...');                  {indicate more data values exist}
      end;
    writeln;
    end;                               {end of tag ID cases}

done_entry:                            {all done processing this entry}
  goto entry_loop;
{
*   All done processing the entries in this directory.
}
done_entries:
  if punt_image then goto next_image_loop; {already know image not acceptable ?}
  if dr.image.x_size = 0 then goto next_image_loop; {invalid dimension ?}
  if dr.image.y_size = 0 then goto next_image_loop;
  if (dr.image.subfile & (subtype_reduced_k ! subtype_mask_k)) <> 0 {not right type ?}
    then goto next_image_loop;

  dr.image.rows_strip :=               {clip to total number of scan lines}
    min(dr.image.rows_strip, dr.image.y_size);
{
*   Set the final image aspect ratio.
}
  dr.image.aspect := dr.image.x_size / dr.image.y_size; {init, assume square pixels}
  if res_h_set and res_v_set then begin {explicit resolutions were supplied ?}
    dr.image.aspect := dr.image.aspect * res_v / res_h;
    end;
{
*   Create our final strips list, and deallocate the temporary strip
*   offsets and lengths lists.
}
  if dr.image.photometric = photo_cfa_k then begin {source pixels from color filter array ?}
    dr.image.bits_grn := dr.image.bits_red;
    dr.image.bits_blu := dr.image.bits_red;
    dr.image.truecolor := true;
    end;

  j :=                                 {make number of strips in this image}
    (dr.image.y_size + dr.image.rows_strip - 1) div dr.image.rows_strip;
  sz := j * sizeof(strip_t);           {amount of mem needed for strips data}
  util_mem_grab (                      {allocate mem for strips data}
    sz, dr.image.mem_p^, false, dr.image.strip0_p);
  iadr_p := strip_ofs_p;               {save pointer values before they get altered}
  jadr_p := strip_len_p;
  if dr.image.compress = compress_none_k
    then begin                         {we can calculate strip sizes}
      sz := uncomp_line_len(dr);       {get uncompressed line length}
      end
    else begin                         {no way for us to calculate strip sizes}
      sz := 0;                         {indicate don't know scan line size}
      end
    ;

  strip_p := dr.image.strip0_p;        {init pointer to current strips list entry}
  lines_left := dr.image.y_size;       {init number of scan lines after curr strip}
  for i := 1 to j do begin             {once for each strip}
    lines_strip := min(lines_left, dr.image.rows_strip); {scan line in this strip}
    lines_left := lines_left - lines_strip; {update lines left after this strip}
    strip_p^.ofs_red := strip_ofs_p^;  {set mandatory field}
    strip_p^.ofs_grn := 0;             {init remaining fields to default values}
    strip_p^.ofs_blu := 0;
    strip_p^.ofs_comp4 := 0;
    strip_p^.ofs_comp5 := 0;
    strip_p^.ofs_comp6 := 0;
    strip_p^.ofs_comp7 := 0;
    strip_p^.ofs_comp8 := 0;
    if sz > 0
      then strip_p^.len_red := sz * lines_strip
      else strip_p^.len_red := adr_unk_k;
    strip_p^.len_grn := strip_p^.len_red;
    strip_p^.len_blu := strip_p^.len_red;
    strip_p^.len_comp4 := strip_p^.len_red;
    strip_p^.len_comp5 := strip_p^.len_red;
    strip_p^.len_comp6 := strip_p^.len_red;
    strip_p^.len_comp7 := strip_p^.len_red;
    strip_p^.len_comp8 := strip_p^.len_red;
    strip_ofs_p := univ_ptr(
      sys_int_adr_t(strip_ofs_p) + sizeof(strip_ofs_p^));
    if strip_len_p <> nil then begin   {strip lengths were supplied ?}
      strip_p^.len_red := strip_len_p^;
      strip_len_p := univ_ptr(
        sys_int_adr_t(strip_len_p) + sizeof(strip_len_p^));
      end;
    strip_p := univ_ptr(               {advance to next strips list entry}
      sys_int_adr_t(strip_p) + sizeof(strip_p^));
    end;

  if dr.image.rgb_sep then begin       {colors are stored in separate strips ?}
    if dr.image.ncomp >= 2 then begin
      strip_p := dr.image.strip0_p;    {init pointer to current strips list entry}
      for i := 1 to j do begin         {once for each strip}
        strip_p^.ofs_grn := strip_ofs_p^; {set green strip offset}
        strip_ofs_p := univ_ptr(       {advance offsets source pointer}
          sys_int_adr_t(strip_ofs_p) + sizeof(strip_ofs_p^));
        if strip_len_p <> nil then begin {strip lengths were supplied ?}
          strip_p^.len_grn := strip_len_p^; {set green strip length}
          strip_len_p := univ_ptr(
            sys_int_adr_t(strip_len_p) + sizeof(strip_len_p^));
          end;
        strip_p := univ_ptr(           {advance to next strips list entry}
          sys_int_adr_t(strip_p) + sizeof(strip_p^));
        end;
      end;
    if dr.image.ncomp >= 3 then begin
      strip_p := dr.image.strip0_p;    {init pointer to current strips list entry}
      for i := 1 to j do begin         {once for each strip}
        strip_p^.ofs_blu := strip_ofs_p^; {set blue strip offset}
        strip_ofs_p := univ_ptr(       {advance offsets source pointer}
          sys_int_adr_t(strip_ofs_p) + sizeof(strip_ofs_p^));
        if strip_len_p <> nil then begin {strip lengths were supplied ?}
          strip_p^.len_blu := strip_len_p^; {set blue strip length}
          strip_len_p := univ_ptr(
            sys_int_adr_t(strip_len_p) + sizeof(strip_len_p^));
          end;
        strip_p := univ_ptr(           {advance to next strips list entry}
          sys_int_adr_t(strip_p) + sizeof(strip_p^));
        end;
      end;
    if dr.image.ncomp >= 4 then begin
      strip_p := dr.image.strip0_p;
      for i := 1 to j do begin
        strip_p^.ofs_comp4 := strip_ofs_p^;
        strip_ofs_p := univ_ptr(
          sys_int_adr_t(strip_ofs_p) + sizeof(strip_ofs_p^));
        if strip_len_p <> nil then begin
          strip_p^.len_comp4 := strip_len_p^;
          strip_len_p := univ_ptr(
            sys_int_adr_t(strip_len_p) + sizeof(strip_len_p^));
          end;
        strip_p := univ_ptr(
          sys_int_adr_t(strip_p) + sizeof(strip_p^));
        end;
      end;
    if dr.image.ncomp >= 5 then begin
      strip_p := dr.image.strip0_p;
      for i := 1 to j do begin
        strip_p^.ofs_comp5 := strip_ofs_p^;
        strip_ofs_p := univ_ptr(
          sys_int_adr_t(strip_ofs_p) + sizeof(strip_ofs_p^));
        if strip_len_p <> nil then begin
          strip_p^.len_comp5 := strip_len_p^;
          strip_len_p := univ_ptr(
            sys_int_adr_t(strip_len_p) + sizeof(strip_len_p^));
          end;
        strip_p := univ_ptr(
          sys_int_adr_t(strip_p) + sizeof(strip_p^));
        end;
      end;
    if dr.image.ncomp >= 6 then begin
      strip_p := dr.image.strip0_p;
      for i := 1 to j do begin
        strip_p^.ofs_comp6 := strip_ofs_p^;
        strip_ofs_p := univ_ptr(
          sys_int_adr_t(strip_ofs_p) + sizeof(strip_ofs_p^));
        if strip_len_p <> nil then begin
          strip_p^.len_comp6 := strip_len_p^;
          strip_len_p := univ_ptr(
            sys_int_adr_t(strip_len_p) + sizeof(strip_len_p^));
          end;
        strip_p := univ_ptr(
          sys_int_adr_t(strip_p) + sizeof(strip_p^));
        end;
      end;
    if dr.image.ncomp >= 7 then begin
      strip_p := dr.image.strip0_p;
      for i := 1 to j do begin
        strip_p^.ofs_comp7 := strip_ofs_p^;
        strip_ofs_p := univ_ptr(
          sys_int_adr_t(strip_ofs_p) + sizeof(strip_ofs_p^));
        if strip_len_p <> nil then begin
          strip_p^.len_comp7 := strip_len_p^;
          strip_len_p := univ_ptr(
            sys_int_adr_t(strip_len_p) + sizeof(strip_len_p^));
          end;
        strip_p := univ_ptr(
          sys_int_adr_t(strip_p) + sizeof(strip_p^));
        end;
      end;
    if dr.image.ncomp >= 8 then begin
      strip_p := dr.image.strip0_p;
      for i := 1 to j do begin
        strip_p^.ofs_comp8 := strip_ofs_p^;
        strip_ofs_p := univ_ptr(
          sys_int_adr_t(strip_ofs_p) + sizeof(strip_ofs_p^));
        if strip_len_p <> nil then begin
          strip_p^.len_comp8 := strip_len_p^;
          strip_len_p := univ_ptr(
            sys_int_adr_t(strip_len_p) + sizeof(strip_len_p^));
          end;
        strip_p := univ_ptr(
          sys_int_adr_t(strip_p) + sizeof(strip_p^));
        end;
      end;
    end;                               {done with components in separate strips}

  if iadr_p <> nil then begin          {deallocate the temporary lists}
    util_mem_ungrab (iadr_p, dr.image.mem_p^);
    end;
  if jadr_p <> nil then begin
    util_mem_ungrab (jadr_p, dr.image.mem_p^);
    end;
{
*   Write the lens focal length header comment(s) if this information is
*   available.
}
  if dr.flen_act < 0.001 then begin    {not real actual focal length value ?}
    dr.flags := dr.flags - [imgflag_flen_act_k];
    end;
  if dr.flen_equiv < 0.001 then begin  {not real equivalent focal length value ?}
    dr.flags := dr.flags - [imgflag_flen_equiv_k];
    end;

  if                                   {have both actual and 35mm equiv focal lengths ?}
      (imgflag_flen_act_k in dr.flags) and
      (imgflag_flen_equiv_k in dr.flags)
      then begin
    r := max(dr.flen_act, dr.flen_equiv); {make largest of either}
    r2 := abs(dr.flen_act - dr.flen_equiv); {make difference}
    if (r2 / r) < 0.01 then begin      {both are the same ?}
      string_vstring (token, 'Focal length: '(0), -1);
      string_f_fp_free (token2, dr.flen_equiv, 3);
      string_append (token, token2);
      string_appends (token, ' mm'(0));
      add_comment (dr, true, token);   {add single focal length comment}
      goto done_flen;
      end;
    end;

  if imgflag_flen_act_k in dr.flags then begin {have actual focal length ?}
    string_vstring (token, 'Focal length, actual: '(0), -1);
    string_f_fp_free (token2, dr.flen_act, 3);
    string_append (token, token2);
    string_appends (token, ' mm'(0));
    add_comment (dr, true, token);     {add actual focal length comment}
    end;

  if imgflag_flen_equiv_k in dr.flags then begin {have actual focal length ?}
    string_vstring (token, 'Focal length, 35mm equivalent: '(0), -1);
    string_f_fp_free (token2, dr.flen_equiv, 3);
    string_append (token, token2);
    string_appends (token, ' mm'(0));
    add_comment (dr, true, token);     {add actual focal length comment}
    end;

done_flen:                             {done with focal length issues}
{
*   Write the image created date/time as a header comment if this information is
*   known.  If the timezone is not known, then the date/time will be written
*   relative to the current timezone.
}
  if                                   {date and time both explicitly known ?}
      (imgflag_date_k in dr.flags) and
      (imgflag_time_k in dr.flags)
      then begin
    time := sys_clock_from_date (dr.date); {make absolute time}
    if imgflag_tz_k in dr.flags
      then begin                       {the timezone is explicitly known}
        tzone := dr.date.tzone_id;     {use explicitly set timezone info}
        hwest := dr.date.hours_west;
        daysave := dr.date.daysave;
        end
      else begin
        sys_timezone_here (tzone, hwest, daysave); {use local time zone}
        end
      ;
    sys_clock_to_date (                {make final adjusted expanded time}
      time,                            {absolute time to expand to date/time}
      tzone,                           {timezone ID}
      hwest,                           {hours west of the timezone}
      daysave,                         {daylight savings time strategy}
      dr.date);                        {returned expanded date descriptor}

    string_vstring (token, 'Time: '(0), -1); {init comment with command name}

    sys_date_string (                  {year}
      dr.date, sys_dstr_year_k, 4, token2, stat);
    string_append (token, token2);

    sys_date_string (                  {month}
      dr.date, sys_dstr_mon_k, 2, token2, stat);
    string_append1 (token, '/');
    string_append (token, token2);

    sys_date_string (                  {day}
      dr.date, sys_dstr_day_k, 2, token2, stat);
    string_append1 (token, '/');
    string_append (token, token2);

    sys_date_string (                  {hour}
      dr.date, sys_dstr_hour_k, 2, token2, stat);
    string_append1 (token, '.');
    string_append (token, token2);

    sys_date_string (                  {minute}
      dr.date, sys_dstr_min_k, 2, token2, stat);
    string_append1 (token, ':');
    string_append (token, token2);

    sys_date_string (                  {second}
      dr.date, sys_dstr_sec_frac_k, 6, token2, stat);
    string_append1 (token, ':');
    string_append (token, token2);

    string_f_fp (                      {make timzone hours offset string}
      token2,                          {output string}
      -dr.date.hours_west,             {input number}
      0, 0,                            {free format}
      0,                               {minimum significant digits required}
      2,                               {max digits allowed left of point}
      2, 2,                            {min and max digits right of point}
      [string_ffp_plus_man_k],         {write sign even if positive}
      stat);
    string_append1 (token, ' ');
    string_append (token, token2);

    add_comment (dr, true, token);     {add the date/time info as header comment}
    end;
{
*   Add latitude/longitude header comment if this information is available.
*
*   This inforation could only have come from GPS data in this implementation,
*   so the error radius is fixed at 10 meters.
}
  if                                   {both lat and long available ?}
     (imgflag_lat_k in dr.flags) and
     (imgflag_lon_k in dr.flags)
      then begin
    string_vstring (token, 'Lat/lon:'(0), -1); {write header command}
    string_f_fp_fixed (token2, dr.lat, 6); {make degrees latitude string}
    string_append_token (token, token2);
    string_f_fp_fixed (token2, dr.lon, 6); {make degrees longitude string}
    string_append_token (token, token2);
    string_appends (token, ' 10');     {add fixed error radius}
    add_comment (dr, true, token);     {add to header comments}
    end;
{
*   Now set up other internal data that the scan line read routine expects.
}
  dr.lines_left := 0;                  {number of scan lines left in curr strip}
  dr.strip_next_p := dr.image.strip0_p; {set pointer to next set of strips}

  if dr.image.rgb_sep
    then begin                         {each component is in a separate strip ?}
      if dr.image.ncomp >= 1 then init_strip_state (dr, dr.red);
      if dr.image.ncomp >= 2 then init_strip_state (dr, dr.grn);
      if dr.image.ncomp >= 3 then init_strip_state (dr, dr.blu);
      if dr.image.ncomp >= 4 then init_strip_state (dr, dr.comp4);
      if dr.image.ncomp >= 5 then init_strip_state (dr, dr.comp5);
      if dr.image.ncomp >= 6 then init_strip_state (dr, dr.comp6);
      if dr.image.ncomp >= 7 then init_strip_state (dr, dr.comp7);
      if dr.image.ncomp >= 8 then init_strip_state (dr, dr.comp8);
      end
    else begin                         {all components sequential per pixel}
      init_strip_state (dr, dr.red);   {we use RED strip state for single strip}
      end
    ;
{
*   Set BITS_STORED if the number of bits per component stored in the file is
*   different from the number of meaningful bits per component.  BITS_STORED has
*   been initialized to 0 to indicate the number of stored and meaningful bits
*   is the same.
*
*   The TIFF spec seems to indicate that only the meaningful number of bits
*   should be stored.  However, we know of a few cases where the storage size is
*   rounded up to whole bytes.
}
  if
      (dr.image.compress = compress_none_k) and {no compression ?}
      (dr.image.ncomp = 1)             {1 component per pixel ?}
      then begin
    i := (dr.image.bits_red + 7) div 8; {whole bytes required for component value}
    j := i * dr.image.x_size * dr.image.rows_strip; {strip size if whole bytes used}
    if dr.image.strip0_p^.len_red = j then begin {size based on whole bytes ?}
      i := i * 8;                      {make bit size of stored data}
      if i > dr.image.bits_red then begin {padding bits apparently used ?}
        dr.image.bits_stored := i;
        end;
      end;
    end;

  dr.comm.size := 132;                 {restore comment string size to reasonable}

  if dr.debug >= 1 then begin
    writeln;
    end;
  end;
{
*************************************************************************
*
*   Local subroutine NEXT_STRIP (DR, SS, OFS, LEN, STAT)
*
*   Set up the strip state SS for reading the start of the next strip.
*   OFS is the file offset for the next strip.  LEN is the length of the
*   next strip, if known.  LEN will be ADR_UNK_K if not known.
}
procedure next_strip (
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in out  ss: strip_state_t;           {particular data for reading the curr strip}
  in      ofs: sys_int_adr_t;          {file offset for start of new strip}
  in      len: sys_int_adr_t;          {length of new strip, if known}
  out     stat: sys_err_t);            {completion status code}
  val_param; internal;

var
  sz: sys_int_adr_t;                   {requested size of new mapped region}

begin
  if ss.map_p <> nil then begin        {another region is previously mapped ?}
    file_map_done (ss.maph);           {unmap previous mapped region}
    end;

  sz := len;                           {init strip size to length passed}
  if len = adr_unk_k then begin        {strip size if not exactly known ?}
    sz := strip_len_unknown_k;         {pick an initial amount to map}
    end;

  file_map (                           {map strip to our address space}
    dr.conn,                           {connection handle to TIFF file}
    ofs,                               {file offset to start of strip}
    sz,                                {size of region to map}
    [file_rw_read_k],                  {only read access is required}
    ss.map_p,                          {returned pointer to start of mapped region}
    ss.left_mbytes,                    {actual length mapped}
    ss.maph,                           {returned handle for mapped region}
    stat);
  if len = adr_unk_k then begin        {may have mapped more than strip size ?}
    discard(file_eof_partial(stat));   {it's OK to get less than requested}
    end;
  ss.ofs_after_map := ofs + ss.left_mbytes; {file offset right after mapped region}
  ss.read_p := nil;
  ss.whole_strip_done := false;        {whole strip not uncompressed yet}
  end;
{
*************************************************************************
*
*   Local function NEXT_BYTE_STRIP (DR, SS)
*
*   Read the next raw input byte from the indicated strip.  All appropriate
*   state is updated.
}
function next_byte_strip (
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in out  ss: strip_state_t)           {data for this particular strip}
  :sys_int_machine_t;                  {returned 0-255 byte value}
  internal;

var
  stat: sys_err_t;                     {completion status code}

begin
  if ss.left_mbytes <= 0 then begin    {need to read another chunk ?}
    file_map_done (ss.maph);           {unmap previous mapped region}
    file_map (                         {map next chunk in this strip}
      dr.conn,                         {connection handle to TIFF file}
      ss.ofs_after_map,                {file offset to right after previous chunk}
      strip_len_unknown_k,             {size of region to map}
      [file_rw_read_k],                {only read access is required}
      ss.map_p,                        {returned pointer to start of mapped region}
      ss.left_mbytes,                  {actual length mapped}
      ss.maph,                         {returned handle for mapped region}
      stat);
    ss.ofs_after_map :=                {file offset right after mapped region}
      ss.ofs_after_map + ss.left_mbytes;
    discard(file_eof_partial(stat));   {it's OK to get less than requested}
    sys_error_abort (stat, 'img', 'tif_err_map_pixels', nil, 0);
    end;                               {there is now at least one byte available}

  next_byte_strip := ord(ss.map_p^);   {return this byte value}
  ss.map_p := univ_ptr(                {update read pointer to next byte in buffer}
    sys_int_adr_t(ss.map_p) + sizeof(ss.map_p^));
  ss.left_mbytes := ss.left_mbytes - 1; {one less byte left in current mapped region}
  end;
{
********************************************************************************
*
*   UNCOMPRESS_RUNLEN (DR, SS)
*
*   Read data for next scan line, uncompress, and set SS.READ_P pointing
*   to the start of the uncompressed data.  This routine performs
*   run length decompression.
}
procedure uncompress_runlen (
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in out  ss: strip_state_t);          {particular data for the strip to read from}
  internal;

var
  cnt: sys_int_machine_t;              {bytes left this "run", -N for replicate run}
  runval: sys_int_machine_t;           {data value if in replicate run}
  i: sys_int_machine_t;                {uncompressed byte loop counter}

begin
  cnt := 0;
  ss.read_p := ss.ubuf_p;              {init uncompressed buffer write pointer}

  for i := 1 to ss.ubuf_size do begin  {once for each resulting uncompressed byte}
    if cnt = 0 then begin              {need to read the next input run ?}
      repeat                           {loop until not read a NOP byte}
        cnt := next_byte_strip (dr, ss); {read the run length byte}
        until cnt <> 128;              {back if this was a NOP byte}
      if cnt > 128
        then begin                     {this is a replicate run}
          cnt := cnt - 257;            {make number of times to repeat next byte}
          runval := next_byte_strip (dr, ss); {get data value to repeat}
          end
        else begin                     {this is a literal run}
          cnt := cnt + 1;              {make number of bytes that will follow}
          end
        ;
      end;                             {CNT is now non-zero}
    if cnt > 0
      then begin                       {we are in a literal run}
        ss.read_p^ := next_byte_strip(dr, ss); {copy this byte directly}
        cnt := cnt - 1;                {one less byte left in literal run}
        end
      else begin                       {we are in a replicate run}
        ss.read_p^ := runval;          {write run data value into uncompressed buf}
        cnt := cnt + 1;                {one less byte left in replicate run}
        end
      ;
    ss.read_p := univ_ptr(             {update uncompressed data write pointer}
      sys_int_adr_t(ss.read_p) + sizeof(ss.read_p^));
    end;                               {back to get next uncompressed byte}

  ss.read_p := ss.ubuf_p;              {init pointer for reading uncompressed data}
  end;
{
*************************************************************************
*
*   Local subroutine UNCOMPRESS_LZW (DR, SS)
*
*   Read data for next scan line, uncompress, and set SS.READ_P pointing
*   to the start of the uncompressed data.  This routine does LZW
*   decompression.  D is the private LZW data block in SS.
}
procedure uncompress_lzw (
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in out  ss: strip_state_t);          {particular data for the strip to read from}
  internal;

var
  mem_p: util_mem_context_p_t;         {memory context for this decompression}
  code_bits: sys_int_machine_t;        {number of bits to read for next code value}
  code: sys_int_machine_t;             {current LZW code}
  code_old: sys_int_machine_t;         {previous LZW code}
  next_ent: sys_int_machine_t;         {TABLE index for next entry to make}
  ent_upsize: sys_int_machine_t;       {table entry where codes get one bit bigger}
  end_write_adr: sys_int_adr_t;        {first address past end of uncompressed buf}
  str_add: string_var8192_t;           {string to add to table as new entry}
  str_write_p: string_var_p_t;         {points to string to write to output stream}
  i: sys_int_machine_t;                {loop counter}
  n_saved: sys_int_machine_t;          {number of bits saved in SAVED_BITS}
  saved_bits: sys_int_machine_t;       {bits read from input but not used yet}
  s: string_var4_t;                    {string for single char codes}
  table: lzw_table_read_t;             {code to strings translation table}

label
  code_loop, done;
{
*********************************
*
*   Local subroutine GET_NEXT_CODE
*   This subroutine is local to UNCOMPRESS_LZW.
*
*   Read the next LZW code from the input stream and put it into CODE.
}
procedure get_next_code;

var
  bits_left: sys_int_machine_t;        {number of bits left to read}
  bits: sys_int_machine_t;             {number of bits getting this loop}
  mask: sys_int_conv32_t;              {mask for bits read this loop}

begin
  code := 0;                           {init accumulated bits}
  bits_left := code_bits;              {init number of bits left to read}

  repeat                               {loop until read all the required bits}
    if n_saved <= 0 then begin         {no unread bits available ?}
      saved_bits := ord(ss.map_p^);    {read next byte from input}
      ss.map_p := univ_ptr(            {update input stream read pointer}
        sys_int_adr_t(ss.map_p) + sizeof(ss.map_p^));
      n_saved := 8;                    {update number of bits in SAVED_BITS}
      end;
    bits := min(n_saved, bits_left);   {make number of bits to read this loop}
    mask := ~lshft(~0, bits);          {make mask for new bits in CODE}
    code := lshft(code, bits);         {make room for the new bits}
    code := code ! (rshft(saved_bits, n_saved-bits) & mask); {accumulate new bits}
    n_saved := n_saved - bits;         {fewer bits now available in SAVED_BITS}
    bits_left := bits_left - bits;     {fewer bits left to go in returned value}
    until bits_left <= 0;              {back until got all the required bits}
  end;
{
*********************************
*
*   Start of UNCOMPRESS_LZW
}
begin
  str_add.max := sizeof(str_add.str);  {init local var strings}
  s.max := sizeof(s.str);
  s.len := 1;
  ss.read_p := ss.ubuf_p;              {init uncompressed buffer write pointer}
  end_write_adr :=                     {make first address past end of UBUF}
    sys_int_adr_t(ss.ubuf_p) + ss.ubuf_size;
  n_saved := 0;                        {init to no unused bits left over}
  code_bits := lzw_bits_first_code_k;  {init bit size of next code value}
  mem_p := nil;                        {init to no memory context created yet}

code_loop:                             {back here to read each new LZW code value}
  get_next_code;                       {get next code from input stream}
  if code = lzw_code_eoi_k then goto done; {END OF INFORMATION code ?}
{
*   LZW code is CLEAR.
}
  if code = lzw_code_clear_k then begin {need to reset table ?}
    if mem_p <> nil then begin         {previous memory context exists ?}
      util_mem_context_del (mem_p);    {delete all our temporary memory}
      end;
    util_mem_context_get (dr.image.mem_p^, mem_p); {create our own private mem context}
    next_ent := lzw_code_first_var_k;  {init code value of next table entry}
    code_bits := lzw_bits_first_code_k; {init bit size of next code value}
    ent_upsize := 511;                 {last table entry before code size change}
    get_next_code;                     {read first code in new block}
    if code = lzw_code_eoi_k then goto done; {hit END OF INFORMATION code ?}
    code_old := code;                  {this is the initial "old" code value}
    ss.read_p^ := code_old;            {write value for initial code}
    ss.read_p := univ_ptr(             {advance the buffer write pointer}
      sys_int_adr_t(ss.read_p) + sizeof(ss.read_p^));
    goto code_loop;                    {back to get next LZW code}
    end;
{
*   Init the string for the new table entry to the string for the old code.
}
  if code > next_ent then goto done;   {illegal code value ?}

  if code_old <= 255
    then begin                         {single char code}
      str_add.str[1] := chr(code_old);
      str_add.len := 1;
      end
    else begin                         {multi-char code from table}
      string_copy (table[code_old]^, str_add); {init string for new code}
      end
    ;

  if code < next_ent
    then begin                         {code is for existing table entry}
      if code <= 255
        then begin                     {single char code}
          s.str[1] := chr(code);
          str_write_p := univ_ptr(addr(s)); {we will write string for this code}
          end
        else begin                     {multi-char code from table}
          str_write_p := table[code];  {we will write string for this code}
          end
        ;
      end
    else begin                         {code is not an existing table entry}
      str_write_p := univ_ptr(addr(str_add)); {we will write str for new table entry}
      end
    ;
  string_append1 (str_add, str_write_p^.str[1]); {make final string for new entry}
  code_old := code;                    {this code now becomes the previous code}
{
*   Write this expansion string to the output buffer.
}
  for i := 1 to str_write_p^.len do begin {once for each character to write}
    ss.read_p^ := ord(str_write_p^.str[i]); {transfer this character}
    ss.read_p := univ_ptr(             {update the buffer write pointer}
      sys_int_adr_t(ss.read_p) + sizeof(ss.read_p^));
    if sys_int_adr_t(ss.read_p) >= end_write_adr {filled uncompressed buffer ?}
      then goto done
    end;                               {back to copy next char to output buffer}
{
*   Create the new table entry.
}
  string_alloc (                       {allocate memory for new table entry string}
    str_add.len,                       {number of characters needed in new string}
    mem_p^,                            {parent memory context}
    false,                             {allocate from pool, if possible}
    table[next_ent]);                  {returned pointer to new string}
  string_copy (str_add, table[next_ent]^); {fill in the table entry string}

  next_ent := next_ent + 1;            {make index of next TABLE entry to create}
  if next_ent = ent_upsize then begin  {code will now be one bit bigger ?}
    code_bits := code_bits + 1;        {make new size of input codes in bits}
    ent_upsize := ~lshft(~0, code_bits); {make next table entry trigger value}
    end;
  goto code_loop;                      {back to handle next LZW input code}

done:                                  {encountered END OF INFORMATION code}
  util_mem_context_del (mem_p);        {delete all our temporary memory}
  ss.read_p := ss.ubuf_p;              {init read pointer to uncompressed data}
  ss.whole_strip_done := true;         {we are done uncompressing entire strip}
  ss.left_mbytes := 0;                 {indicate all input bytes used up}
  end;
{
*************************************************************************
*
*   Local subroutine UNCOMPRESS_LINE (DR, SS)
*
*   Read data for next scan line, uncompress, and set SS.READ_P pointing
*   to the start of the uncompressed data.
}
procedure uncompress_line (
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in out  ss: strip_state_t);          {particular data for the strip to read from}
  internal;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

label
  leave;

begin
  if ss.whole_strip_done then goto leave; {already decompressed whole strip ?}
  case dr.image.compress of            {what compression scheme is being used ?}
{
*************************************
*
*   No compression.
}
compress_none_k: begin
  ss.read_p := ss.map_p;               {we can read the file data directly}
  ss.map_p := univ_ptr(                {advance file read pointer to next strip}
    sys_int_adr_t(ss.map_p) + ss.ubuf_size);
  ss.whole_strip_done := true;
  end;
{
*************************************
*
*   Various forms of CCITT (fax) compression.
}
compress_ccitt3s_k,
compress_ccitt3_k,
compress_ccitt4_k: begin
  sys_msg_parm_int (msg_parm[1], dr.image.compress);
  sys_message_bomb ('img', 'tif_compress_ccitt', msg_parm, 1);
  end;
{
*************************************
*
*   Lempel-Ziv Welch compression.  See appendix F in TIFF file format manual.
}
compress_lzw_k: begin
  uncompress_lzw (dr, ss);
  end;
{
*************************************
*
*   Run length compression.  See appendix C in TIFF file format manual.
}
compress_runlen_k: begin
  uncompress_runlen (dr, ss);
  end;
{
*************************************
*
*   Unexpected compression method ID.
}
otherwise
    sys_msg_parm_int (msg_parm[1], dr.image.compress);
    sys_message_bomb ('img', 'tif_compress_bad', msg_parm, 1);
    end;

leave:                                 {common exit point}
  ss.bits_n := 0;                      {init state for reading new scan line}
  ss.bits := 0;
  end;
{
*************************************************************************
*
*   Local subroutine PRED_STATE_INIT (DR, SS, BITS_VAL, STATE)
*
*   Init the predictor state STATE.  The internal format of STATE should be
*   considered private to the PRED_ routines.  This routine must be called
*   to initialize STATE for each component per scan line.
}
procedure pred_state_init (            {init predictor procssing state}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in out  ss: strip_state_t;           {particular data for the strip to read from}
  in      bits_val: sys_int_machine_t; {number of real bits per component value}
  out     state: pred_state_t);        {returned predictor processing state}
  val_param; internal;

const
  max_msg_parms = 1;                   {max parameters we can pass to a message}

var
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;

begin
  state.dr_p := addr(dr);              {save pointer to top level state block}
  state.ss_p := addr(ss);              {save pointer to state block for this strip}
  state.bits_val := bits_val;          {save meaningful bits per component value}
  state.bits_stored := bits_val;       {init to assume no padding bits}
  if dr.image.bits_stored <> 0 then begin {specific number of stored bits given ?}
    state.bits_stored := dr.image.bits_stored;
    end;
  state.bits_mask := ~lshft(~0, state.bits_val); {make mask for meaningful bits}

  case dr.image.predictor of           {what is predictor method ?}
pred_none_k: ;                         {raw data is just transferred}
pred_hdiff_k: begin                    {horizontal differencing}
      state.hdiff_mask := state.bits_mask; {mask for valid output value bits}
      state.hdiff_prev := 0;           {first value is transferred raw}
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], dr.image.predictor);
    sys_message_bomb ('img', 'tif_predictor_unsupp', msg_parm, 1);
    end;                               {end of predictor type cases}

  ss.bits_n := 0;                      {init arbitrary bit field read state}
  ss.bits := 0;
  ss.pred_p := ss.pbuf_p;              {init write pointer in strip state block}
  end;
{
*************************************************************************
*
*   Local subroutine PRED_COMP (STATE)
*
*   Perform the predictor function processing on one component of one pixel.
*   STATE must be the same state block returned from the last PRED_COMP
*   call, or from a PRED_STATE_INIT call if this is the first pixel in a
*   scan line.  Note that STATE is specific to the pixel component and scan
*   line.
}
procedure pred_comp (                  {do predictor for one component of one pixel}
  in out  state: pred_state_t);        {predictor processing state}
  internal;

var
  nval: pix_value_pred_t;              {final new output component value}

begin
  state.dr_p^.read_p := state.ss_p^.read_p; {set pointer to next byte to read}
  nval := read_bits (                  {read raw bit-aligned pixel value}
    state.dr_p^,                       {handle to top level control block}
    state.bits_stored,                 {number of bits to read}
    state.ss_p^.bits, state.ss_p^.bits_n); {current bit reading state this strip}
  state.ss_p^.read_p := state.dr_p^.read_p; {update read pointer in pred state}
  nval := nval & state.bits_mask;      {mask in only the meaningful bits}
{
*   The new raw component value is in NVAL.
}
  case state.dr_p^.image.predictor of  {what is the predictor function ?}

pred_none_k: ;                         {no predictor, just transfer value}

pred_hdiff_k: begin                    {predictor function is horizontal diff}
      nval := (state.hdiff_prev + nval) & state.hdiff_mask; {make output value}
      state.hdiff_prev := nval;        {save this pixel value for next time}
      end;
    end;                               {end of predictor function cases}
{
*   The predictor function has been applied.  The final processed component
*   value is in NVAL.
}
  state.ss_p^.pred_p^ := nval;         {write final value into buffer}
  state.ss_p^.pred_p := univ_ptr(      {udpate write pointer}
    sys_int_adr_t(state.ss_p^.pred_p) + sizeof(state.ss_p^.pred_p^));
  end;
{
*************************************************************************
*
*   Local subroutine PRED_LINE (DR)
*
*   Apply the predictor function to the current scan line.  The scan line data
*   must have already been uncompressed.  The output values will be put
*   at PBUF_P^ in the appropriate strip blocks.  The PRED_P pointers will
*   be initialized to point to the beginning of the output data.
}
procedure pred_line (                  {apply predictor to one scan line}
  in out  dr: data_read_t);            {top level state block}
  val_param; internal;

var
  ps_red, ps_grn, ps_blu: pred_state_t; {predictor component processing state blocks}
  ps_comp4, ps_comp5, ps_comp6, ps_comp7, ps_comp8: pred_state_t;
  i: sys_int_machine_t;                {loop counter}

begin
  if dr.image.truecolor
    then begin                         {pixel contains separate RGB components}
      if dr.image.rgb_sep
        then begin                     {RGB components are stored in separate strips}
          if dr.image.ncomp >= 1 then begin
            pred_state_init (          {init RED predictor processing state block}
              dr,                      {top level control block}
              dr.red,                  {control block for strip with this component}
              dr.image.bits_red,       {number of bits in this component}
              ps_red);                 {returned initialized predictor state block}
            end;
          if dr.image.ncomp >= 2 then begin
            pred_state_init (          {init GREEN predictor processing state block}
              dr,                      {top level control block}
              dr.grn,                  {control block for strip with this component}
              dr.image.bits_grn,       {number of bits in this component}
              ps_grn);                 {returned initialized predictor state block}
            end;
          if dr.image.ncomp >= 3 then begin
            pred_state_init (          {init BLUE predictor processing state block}
              dr,                      {top level control block}
              dr.blu,                  {control block for strip with this component}
              dr.image.bits_blu,       {number of bits in this component}
              ps_blu);                 {returned initialized predictor state block}
            end;
          if dr.image.ncomp >= 4 then begin
            pred_state_init (
              dr,
              dr.comp4,
              dr.image.bits_comp4,
              ps_comp4);
            end;
          if dr.image.ncomp >= 5 then begin
            pred_state_init (
              dr,
              dr.comp5,
              dr.image.bits_comp5,
              ps_comp5);
            end;
          if dr.image.ncomp >= 6 then begin
            pred_state_init (
              dr,
              dr.comp6,
              dr.image.bits_comp6,
              ps_comp6);
            end;
          if dr.image.ncomp >= 7 then begin
            pred_state_init (
              dr,
              dr.comp7,
              dr.image.bits_comp7,
              ps_comp7);
            end;
          if dr.image.ncomp >= 8 then begin
            pred_state_init (
              dr,
              dr.comp8,
              dr.image.bits_comp8,
              ps_comp8);
            end;
          for i := 1 to dr.image.x_size do begin {once for each pixel in scan line}
            if dr.image.ncomp >= 1 then pred_comp (ps_red); {perform predictor}
            if dr.image.ncomp >= 2 then pred_comp (ps_grn);
            if dr.image.ncomp >= 3 then pred_comp (ps_blu);
            if dr.image.ncomp >= 4 then pred_comp (ps_comp4);
            if dr.image.ncomp >= 5 then pred_comp (ps_comp5);
            if dr.image.ncomp >= 6 then pred_comp (ps_comp6);
            if dr.image.ncomp >= 7 then pred_comp (ps_comp7);
            if dr.image.ncomp >= 8 then pred_comp (ps_comp8);
            end;
          dr.red.pred_p := dr.red.pbuf_p; {init pointer to read resulting values}
          dr.grn.pred_p := dr.grn.pbuf_p;
          dr.blu.pred_p := dr.blu.pbuf_p;
          dr.comp4.pred_p := dr.comp4.pbuf_p;
          dr.comp5.pred_p := dr.comp5.pbuf_p;
          dr.comp6.pred_p := dr.comp6.pbuf_p;
          dr.comp7.pred_p := dr.comp7.pbuf_p;
          dr.comp8.pred_p := dr.comp8.pbuf_p;
          end
        else begin                     {RGB components are stored in same strip}
          if dr.image.ncomp >= 1 then begin
            pred_state_init (          {init RED predictor processing state block}
              dr,                      {top level control block}
              dr.red,                  {control block for strip with this component}
              dr.image.bits_red,       {number of bits in this component}
              ps_red);                 {returned initialized predictor state block}
            end;
          if dr.image.ncomp >= 2 then begin
            pred_state_init (          {init GREEN predictor processing state block}
              dr,                      {top level control block}
              dr.red,                  {control block for strip with this component}
              dr.image.bits_grn,       {number of bits in this component}
              ps_grn);                 {returned initialized predictor state block}
            end;
          if dr.image.ncomp >= 3 then begin
            pred_state_init (          {init BLUE predictor processing state block}
              dr,                      {top level control block}
              dr.red,                  {control block for strip with this component}
              dr.image.bits_blu,       {number of bits in this component}
              ps_blu);                 {returned initialized predictor state block}
            end;
          if dr.image.ncomp >= 4 then begin
            pred_state_init (
              dr,
              dr.red,
              dr.image.bits_comp4,
              ps_comp4);
            end;
          if dr.image.ncomp >= 5 then begin
            pred_state_init (
              dr,
              dr.red,
              dr.image.bits_comp5,
              ps_comp5);
            end;
          if dr.image.ncomp >= 6 then begin
            pred_state_init (
              dr,
              dr.red,
              dr.image.bits_comp6,
              ps_comp6);
            end;
          if dr.image.ncomp >= 7 then begin
            pred_state_init (
              dr,
              dr.red,
              dr.image.bits_comp7,
              ps_comp7);
            end;
          if dr.image.ncomp >= 8 then begin
            pred_state_init (
              dr,
              dr.red,
              dr.image.bits_comp8,
              ps_comp8);
            end;
          for i := 1 to dr.image.x_size do begin {once for each pixel in scan line}
            if dr.image.ncomp >= 1 then pred_comp (ps_red); {perform predictor}
            if dr.image.ncomp >= 2 then pred_comp (ps_grn);
            if dr.image.ncomp >= 3 then pred_comp (ps_blu);
            if dr.image.ncomp >= 4 then pred_comp (ps_comp4);
            if dr.image.ncomp >= 5 then pred_comp (ps_comp5);
            if dr.image.ncomp >= 6 then pred_comp (ps_comp6);
            if dr.image.ncomp >= 7 then pred_comp (ps_comp7);
            if dr.image.ncomp >= 8 then pred_comp (ps_comp8);
            end;
          dr.red.pred_p := dr.red.pbuf_p; {init pointer to read resulting values}
          end
        ;
      end                              {done handling RGB format pixel data}
    else begin                         {pixel contains only one component}
      pred_state_init (                {init predictor processing state block}
        dr,                            {top level control block}
        dr.red,                        {control block for strip with this component}
        dr.image.bits_red,             {number of bits in this component}
        ps_red);                       {returned initialized predictor state block}
      for i := 1 to dr.image.x_size do begin {once for each pixel in scan line}
        pred_comp (ps_red);            {perform predictor on this pixel}
        end;
      dr.red.pred_p := dr.red.pbuf_p;  {init pointer to read resulting values}
      end
    ;
  end;
{
*************************************************************************
*
*   Local function COMP_VAL (DR, SS)
*
*   Get the next pixel component value from the strip SS.
}
function comp_val (                    {get raw component value}
  in out  dr: data_read_t;             {top level state block}
  in out  ss: strip_state_t)           {particular data for the strip to read from}
  :pix_value_pred_t;                   {returned 0-65535 value}
  val_param; internal;

begin
  comp_val := ss.pred_p^;              {fetch and return next component value}
  ss.pred_p := univ_ptr(               {update read pointer}
    sys_int_adr_t(ss.pred_p) + sizeof(ss.pred_p^));
  end;
{
*************************************************************************
*
*   Local subroutine GET_LUT_ENT (DR, SS, ENT_P)
*
*   Return pointer to the LUT entry indicated by the next component
*   value read from strip indicated by SS.  The strip data for this scan
*   line must already have been processed thru the predictor function.
}
procedure get_lut_ent (
  in out  dr: data_read_t;             {top level state block}
  in out  ss: strip_state_t;           {particular data for the strip to read from}
  out     ent_p: lut_ent_p_t);         {returned pointer to LUT entry}
  val_param; internal;

var
  comp: sys_int_machine_t;             {component value read from strip}

begin
  comp := comp_val (dr, ss);           {fetch next component value}

  ent_p := univ_ptr(
    sys_int_adr_t(dr.image.lut_p) +    {address of first LUT entry}
    (comp * sizeof(dr.image.lut_p^))); {offset to desired LUT entry}
  end;
{
*************************************************************************
*
*   Global routine to open TIFF file for read.
}
procedure img_d_tif_open_read (        {open TIFF file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}

var
  dr_p: data_read_p_t;                 {pointer to our private TIFF read data}
  di_p: img_conn2_p_t;                 {pointer to IMG library private data}
  junk: sys_int_adr_t;                 {unused subroutine return argument}
  i: sys_int_machine_t;                {scratch integer value}
  i_p: sys_int_machine_p_t;            {scratch pointer to integer}
  tzone: sys_tzone_k_t;                {time zone}
  hwest: real;                         {time zone hours west}
  daysave: sys_daysave_k_t;            {daylight savings time strategy}

label
  abort1, abort2;

begin
  di_p := img.data_p;                  {get pointer to IMG library private data}
  img_mem_alloc (img, sizeof(dr_p^), dr_p); {allocate our private data block}
  di_p^.data_p := dr_p;                {point IMG data to our private data block}
  dr_p^.debug := debug_level;          {get debug level and save in state block}

  file_open_map (                      {open file for memory mapped access}
    fnam,                              {name of file to open}
    '.tif .tiff .nef',                 {list of possible file suffixes}
    [file_rw_read_k],                  {open file for read only}
    dr_p^.conn,                        {returned file connection handle}
    stat);                             {returned completion status code}
  if sys_error(stat) then goto abort1; {error opening TIFF file ?}
{
*   Read the TIFF file header and initialize for reading directories.
}
  file_map (                           {map file header into memory}
    dr_p^.conn,                        {handle to file connection}
    0,                                 {file offset for mapped region}
    8,                                 {length of header to map}
    [file_rw_read_k],                  {map region for read access}
    dr_p^.read_dir_p,                  {returned pointer to start of mapped area}
    junk,
    dr_p^.maph_dir,                    {returned handle for this mapped area}
    stat);
  if sys_error(stat) then goto abort1;
  dr_p^.adr_ofs0_dir := sys_int_adr_t(dr_p^.read_dir_p);
  dr_p^.read_p := dr_p^.read_dir_p;    {set up for reading values}
  dr_p^.adr_ofs0 := dr_p^.adr_ofs0_dir;

  dr_p^.byte_order := sys_byte_order_k; {init to some valid value}
  i := read_i16(dr_p^);                {get byte order word}
  case i of
order_fwd_k: begin                     {file byte order is forwards}
      dr_p^.byte_order := sys_byte_order_fwd_k;
      end;
order_bkw_k: begin                     {file byte order is backwards}
      dr_p^.byte_order := sys_byte_order_bkw_k;
      end;
otherwise                              {bad byte order field value}
    sys_stat_set (img_subsys_k, img_err_file_bad_k, stat);
    sys_stat_parm_vstr (dr_p^.conn.tnam, stat);
    sys_stat_parm_int (i, stat);
    sys_stat_parm_int (0, stat);
    goto abort2;
    end;

  i := read_i16(dr_p^);                {get version ID word}
  if i <> version_id_k then begin      {not the right TIFF file version ?}
    sys_stat_set (img_subsys_k, img_err_file_bad_k, stat);
    sys_stat_parm_vstr (dr_p^.conn.tnam, stat);
    sys_stat_parm_int (i, stat);
    sys_stat_parm_int (2, stat);
    goto abort2;
    end;

  dr_p^.dir_n := 0;                    {init to before first TIFF file directory}
  dr_p^.read_dir_p := dr_p^.read_p;    {point to file offset for first directory}

  string_list_init (dr_p^.comm, di_p^.mem_p^); {init list of comment lines}
  dr_p^.comm.deallocable := false;
  dr_p^.nhead := 0;

  sys_timezone_here (tzone, hwest, daysave); {get local time zone info}
  sys_clock_to_date (                  {init date/time to current}
    sys_clock,                         {time to set}
    tzone,                             {time zone ID}
    hwest,                             {hours west}
    daysave,                           {dayling savings time strategy}
    dr_p^.date);                       {returned date/time descriptor}
  dr_p^.lat := 0.0;
  dr_p^.lon := 0.0;
  dr_p^.flen_act := 0.0;
  dr_p^.flen_equiv := 0.0;
  dr_p^.flags := [];
  for i := 0 to cfaline_max_k do begin
    dr_p^.cfaline[i].dat_p := nil;
    dr_p^.cfaline[i].y := -1;
    end;

  next_image (di_p^, dr_p^, stat);     {find first suitable image in TIFF file}
  if sys_error(stat) then goto abort2;
{
*   The data for the first image is in DR_P^.IMAGE.  Now fill in IMG.
}
  img.x_size := dr_p^.image.x_size;
  img.y_size := dr_p^.image.y_size;
  img.aspect := dr_p^.image.aspect;
  img.next_y := 0;
  if dr_p^.image.comp_alpha > 0
    then begin                         {alpha is present}
      i_p := univ_ptr(                 {make adr of bits in alpha component}
        sys_int_adr_t(addr(dr_p^.image.bits_red)) +
        sizeof(i_p^) * (dr_p^.image.comp_alpha - 1));
      img.bits_alpha := i_p^;          {get bits in alpha component}
      end
    else begin                         {no alpha present}
      img.bits_alpha := 0;
      end
    ;
  img.bits_red := dr_p^.image.bits_red;
  if dr_p^.image.truecolor
    then begin
      img.bits_grn := dr_p^.image.bits_grn;
      img.bits_blu := dr_p^.image.bits_blu;
      end
    else begin
      img.bits_grn := dr_p^.image.bits_red;
      img.bits_blu := dr_p^.image.bits_red;
      end
    ;
  img.bits_max := max(img.bits_red, max(img.bits_grn, img.bits_blu));
  string_vstring (img.file_type, 'tif', 3);
  string_copy (dr_p^.conn.gnam, img.gnam);
  string_copy (dr_p^.conn.tnam, img.tnam);
  string_list_copy (dr_p^.comm, img.comm, di_p^.mem_p^);
{
*   IMG has been all filled in.  Now fill in the private IMG library
*   data for this file connection.
}
  di_p^.read_scan2 := addr(img_d_tif_read_scan2);
  di_p^.rewind := addr(img_d_tif_rewind);
  di_p^.close := addr(img_d_tif_close_read);
  return;                              {normal return, no errors}
{
*   Error exits.  STAT has already been set to indicate the error.
}
abort2:                                {return with error, TIFF file is open}
  file_close (dr_p^.conn);             {close the TIFF file}

abort1:                                {return with error, TIFF file is closed}
  util_mem_ungrab (dr_p, di_p^.mem_p^); {deallocate our private data block}
  end;
{
********************************************************************************
*
*   Local subroutine CFA_CACHE (IMG2, DR, CA, SY)
*
*   Make sure the source scan line of coordinate SY is in the CFALINE entry CA.
*   This routine must be called with requests for new scan lines in sequential
*   Y order.  If the scan line SY is not already in the cache, then it is
*   assumed to be the next unread line from the image file.  This is not
*   checked.
}
procedure cfa_cache (                  {make sure source line is cached}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  in      ca: sys_int_machine_t;       {0-N index of CFALINE entry to make sure is set}
  in      sy: sys_int_machine_t);      {Y of image scan line to put in CFALINE entry}
  val_param; internal;

var
  sz: sys_int_adr_t;                   {size of new memory to allocate}
  ii, jj: sys_int_machine_t;           {scratch integers}
  cfal: cfaline_t;                     {scratch cache entry}
  stat: sys_err_t;                     {completion status}

begin
  if sy < 0 then return;               {above image, ignore request ?}
  if sy >= dr.image.y_size then return; {below image, ignore request ?}
  if dr.cfaline[ca].y = sy then return; {the requested line is already cached ?}
{
*   Make sure memory is allocated for this cached line.
}
  if dr.cfaline[ca].dat_p = nil then begin {no memory currently allocated ?}
    sz := sizeof(dr.cfaline[ca].dat_p^[0]) * dr.image.x_size; {mem size of cached line}
    util_mem_grab (sz, dr.image.mem_p^, false, dr.cfaline[ca].dat_p); {alloc the memory}
    end;
{
*   Look for this scan line already cached in another entry.  If found, then
*   swap the entries.
}
  for ii := 0 to dr.cfal_last do begin {once for each cache entry in use}
    if dr.cfaline[ii].y = sy then begin {found this line elsewhere in the cache ?}
      cfal := dr.cfaline[ca];          {swap the entries}
      dr.cfaline[ca] := dr.cfaline[ii];
      dr.cfaline[ii] := cfal;
      return;
      end;
    end;                               {back to check next existing cache entry}
{
*   The requested source line has not yet been read from the file.  It is
*   assumed to be the next line from the image file.
}
  if dr.lines_left <= 0 then begin     {the current strip is exhausted ?}
    next_strip (                       {set up for reading the next strip}
      dr,                              {our private image file reading state}
      dr.red,                          {strip state to update}
      dr.strip_next_p^.ofs_red,        {file offset of the new strip}
      dr.strip_next_p^.len_red,        {length of the new strip}
      stat);
    sys_error_abort (stat, '', '', nil, 0);
    dr.lines_left := dr.image.rows_strip; {reset scan lines left this strip}
    dr.strip_next_p := univ_ptr(       {update pointer to data for next strip}
      sys_int_adr_t(dr.strip_next_p) + sizeof(dr.strip_next_p^));
    end;
  dr.lines_left := dr.lines_left - 1;  {there will be one less unused line this strip}

  uncompress_line (dr, dr.red);        {make uncompressed pixel data}
  pred_line (dr);                      {apply predictor to the uncompressed scan line}
{
*   Copy the fully expanded scan line data into the selected CFALINE entry.
}
  for ii := 0 to dr.image.x_size-1 do begin {once for each pixel in the scan line}
    jj := dr.red.pred_p^;              {fetch the source pixel value}
    dr.red.pred_p := succ(dr.red.pred_p); {update source pointer}
    if dr.image.bits_red < 16 then begin {not all bits in the word are used ?}
      jj := lshft(jj, 16 - dr.image.bits_red); {left justify the data}
      jj := jj ! rshft (jj, dr.image.bits_red); {replicate original data in low bits}
      end;
    dr.cfaline[ca].dat_p^[ii] := jj;   {save this pixel in CFALINE array}
    end;

  dr.cfaline[ca].y := sy;              {save Y coor of line in this entry}
  end;
{
********************************************************************************
*
*   Local subroutine READ_CFA (IMG2, DR, SCAN, STAT)
*
*   Return the next scan line in the image into SCAN.  IMG2 is the private IMG
*   library state for the connection to this image.  DR is the private driver
*   state for reading this image.  STAT has already been initialized to indicate
*   no error.  The NEXT_Y field in the public image file connection state is the
*   Y coordinate of the scan line to return.  NEXT_Y has already been checked to
*   be valid for this image.  Scan lines are guaranteed to be read in sequential
*   order from the start of the image.
*
*   This routine is called when photometric interpretation is CFA (color filter
*   array).  CFA data is handle structurally different from normal pixel data,
*   and so its interpretation has been put in this routine just for that
*   purpose.
}
procedure read_cfa (                   {return final image scan line from CFA data}
  in out  img2: img_conn2_t;           {internal IMG library data for this connection}
  in out  dr: data_read_t;             {private data for reading this TIFF file}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  val_param; internal;

var
  radx, rady: sys_int_machine_t;       {number of surrounding pixels needed in X and Y}
  ofsx, ofsy: sys_int_machine_t;       {offset in X and Y from current pixel}
  x: sys_int_machine_t;                {output pixel X coordinate}
  y: sys_int_machine_t;                {Y coordinate of the returned scan line}
  ca: sys_int_machine_t;               {CFALINE index}
  scfax, scfay: sys_int_machine_t;     {CFA pattern starting index this pixel}
  cfax, cfay: sys_int_machine_t;       {CFA pattern index at curr source pattern pixel}
  ccfax, ccfay: sys_int_machine_t;     {CFA pattern index at center of this pixel}
  colpx: sys_int_machine_t;            {center color ID of current output pixel}
  colid: sys_int_machine_t;            {color ID of current source pattern pixel}
  red, grn, blu: sys_int_machine_t;    {component value accumulators}
  nred, ngrn, nblu: sys_int_machine_t; {number of source pixels accumulated}
  sp: sys_int_machine_t;               {source pixel value}

label
  next_spx, next_spy;

begin
  y := img2.img_p^.next_y;             {get Y coordinate of the scan line to return}
  img2.img_p^.next_y := img2.img_p^.next_y + 1; {update Y for next scan line}
  radx := dr.image.cfadimx div 2;      {make max offsets for source pixels}
  rady := dr.image.cfadimy div 2;

  ca := 0;                             {init to first CFALINE entry}
  for ofsy := -rady to rady do begin   {make sure all source scan lines are cached}
    cfa_cache (dr, ca, y + ofsy);      {make sure this source line is cached}
    ca := ca + 1;                      {advance to next CFALINE entry}
    end;

  scfax := dr.image.cfadimx - radx;    {starting left edge CFA pattern index}
  scfay := y - rady;                   {starting top edge CFA pattern index}
  if scfay < 0 then scfay := scfay + dr.image.cfadimy;
  scfay := scfay mod dr.image.cfadimy;
  ccfax := 0;                          {init center CFA X index for this pixel}
  ccfay := y mod dr.image.cfadimy;     {center CFA Y index for this pixel}

  for x := 0 to dr.image.x_size - 1 do begin {accross the output scan line}
    colpx := dr.image.cfapatt[ccfax, ccfay]; {get color ID at this pixel}
    red := 0; nred := 0;               {init color accumulators for this pixel}
    grn := 0; ngrn := 0;
    blu := 0; nblu := 0;
    cfay := scfay;                     {init source CFA pattern Y index}
    for ofsy := -rady to rady do begin {down the CFA pattern}
      if (y + ofsy) < 0 then goto next_spy; {off the top of the image ?}
      if (y + ofsy) >= dr.image.y_size then next; {off the bottom of the image ?}
      cfax := scfax;                   {init source CFA pattern X index}
      for ofsx := -radx to radx do begin {accross this CFA pattern row}
        if (x + ofsx) < 0 then goto next_spx; {off the left edge ?}
        if (x + ofsx) >= dr.image.x_size then goto next_spx; {off the right edge ?}
        colid := dr.image.cfapatt[cfax, cfay]; {get color ID of this source pixel}
        if                             {ignore this source pixel ?}
            (colid = colpx) and        {same color as at destination pixel ?}
            ((ofsx <> 0) or (ofsy <> 0)) {but this isn't the destination pixel ?}
          then goto next_spx;
        sp := dr.cfaline[ofsy + rady].dat_p^[x + ofsx]; {fetch the source pixel value}
        case colid of                  {what is the color at this source pixel ?}
cfa_red_k: begin
            red := red + sp;
            nred := nred + 1;
            end;
cfa_grn_k: begin
            grn := grn + sp;
            ngrn := ngrn + 1;
            end;
cfa_blu_k: begin
            blu := blu + sp;
            nblu := nblu + 1;
            end;
          end;                         {end of color ID cases}
next_spx:                              {advance to next pixel accross this source row}
        cfax := cfax + 1;              {make next source pattern X index}
        if cfax >= dr.image.cfadimx then cfax := cfax - dr.image.cfadimx;
        end;                           {back for next source pixel accross in pattern row}
next_spy:                              {advance to next source pattern row down}
      cfay := cfay + 1;                {make next source pattern Y index}
      if cfay >= dr.image.cfadimy then cfay := cfay - dr.image.cfadimy;
      end;                             {back for next source pattern row down}

    if nred = 0                        {fill in this final destination pixel}
      then begin
        scan[x].red := 0;
        end
      else begin
        scan[x].red := red div nred;
        end
      ;
    if ngrn = 0
      then begin
        scan[x].grn := 0;
        end
      else begin
        scan[x].grn := grn div ngrn;
        end
      ;
    if nblu = 0
      then begin
        scan[x].blu := 0;
        end
      else begin
        scan[x].blu := blu div nblu;
        end
      ;
    scan[x].alpha := 65535;            {set to fully opaque}

    scfax := (scfax + 1) mod dr.image.cfadimx; {update starting X index to CFA pattern}
    ccfax := (ccfax + 1) mod dr.image.cfadimx; {update X index to CFA pattern a output pixel}
    end;                               {back for next pixel accross output scan line}
  end;
{
********************************************************************************
*
*   Global routine to return next scan line as format 2 pixels.
}
procedure img_d_tif_read_scan2 (       {read next scan line as format 2 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  dr_p: data_read_p_t;                 {pointer to our private TIFF read data}
  di_p: img_conn2_p_t;                 {pointer to IMG library private data}
  x: sys_int_machine_t;                {current scan line X coordinate}
  ent_p: lut_ent_p_t;                  {points to current LUT entry}
  comp:                                {value for each pixel component}
    array[1..8] of 0..65535;
  f: real;                             {scratch floating point number}

begin
  sys_error_none (stat);               {init to no errors}
  di_p := img.data_p;                  {get pointer to IMG library private data}
  dr_p := di_p^.data_p;                {get pointer to our private TIFF file data}

  if img.next_y >= dr_p^.image.y_size then begin {no more scan lines left ?}
    sys_stat_set (file_subsys_k, file_stat_eof_k, stat);
    return;                            {return with END OF FILE status}
    end;

  if dr_p^.image.photometric = photo_cfa_k then begin {data is from color filter array ?}
    read_cfa (di_p^, dr_p^, scan, stat); {call separate routine to handle CFA data}
    return;
    end;
{
*   Make sure the local state is set up for reading from the strip where
*   this scan line comes from.
}
  if dr_p^.lines_left <= 0 then begin  {no more scan lines left in curr strips ?}
    if dr_p^.image.rgb_sep
      then begin                       {each component is in a separate strip ?}
        if dr_p^.image.ncomp >= 1 then begin
          next_strip (
            dr_p^, dr_p^.red,
            dr_p^.strip_next_p^.ofs_red, dr_p^.strip_next_p^.len_red, stat);
            if sys_error(stat) then return;
          end;
        if dr_p^.image.ncomp >= 2 then begin
          next_strip (
            dr_p^, dr_p^.grn,
            dr_p^.strip_next_p^.ofs_grn, dr_p^.strip_next_p^.len_grn, stat);
            if sys_error(stat) then return;
          end;
        if dr_p^.image.ncomp >= 3 then begin
          next_strip (
            dr_p^, dr_p^.blu,
            dr_p^.strip_next_p^.ofs_blu, dr_p^.strip_next_p^.len_blu, stat);
            if sys_error(stat) then return;
          end;
        if dr_p^.image.ncomp >= 4 then begin
          next_strip (
            dr_p^, dr_p^.comp4,
            dr_p^.strip_next_p^.ofs_comp4, dr_p^.strip_next_p^.len_comp4, stat);
            if sys_error(stat) then return;
          end;
        if dr_p^.image.ncomp >= 5 then begin
          next_strip (
            dr_p^, dr_p^.comp5,
            dr_p^.strip_next_p^.ofs_comp5, dr_p^.strip_next_p^.len_comp5, stat);
            if sys_error(stat) then return;
          end;
        if dr_p^.image.ncomp >= 6 then begin
          next_strip (
            dr_p^, dr_p^.comp6,
            dr_p^.strip_next_p^.ofs_comp6, dr_p^.strip_next_p^.len_comp6, stat);
            if sys_error(stat) then return;
          end;
        if dr_p^.image.ncomp >= 7 then begin
          next_strip (
            dr_p^, dr_p^.comp7,
            dr_p^.strip_next_p^.ofs_comp7, dr_p^.strip_next_p^.len_comp7, stat);
            if sys_error(stat) then return;
          end;
        if dr_p^.image.ncomp >= 8 then begin
          next_strip (
            dr_p^, dr_p^.comp8,
            dr_p^.strip_next_p^.ofs_comp8, dr_p^.strip_next_p^.len_comp8, stat);
            if sys_error(stat) then return;
          end;
        end
      else begin                       {all components sequential per pixel}
        next_strip (
          dr_p^, dr_p^.red,
          dr_p^.strip_next_p^.ofs_red, dr_p^.strip_next_p^.len_red, stat);
          if sys_error(stat) then return;
        end
      ;
    dr_p^.lines_left := dr_p^.image.rows_strip; {reset scan lines left this strip}
    dr_p^.strip_next_p := univ_ptr(    {update pointer to data for next strips}
      sys_int_adr_t(dr_p^.strip_next_p) + sizeof(dr_p^.strip_next_p^));
    end;
  dr_p^.lines_left := dr_p^.lines_left - 1; {one less line left in curr strips}
{
*   Make sure the next scan line is available in uncompressed form.
}
  if dr_p^.image.rgb_sep
    then begin                         {separate strips for each component}
      if dr_p^.image.ncomp >= 1        {make uncompressed pixel data}
        then uncompress_line (dr_p^, dr_p^.red);
      if dr_p^.image.ncomp >= 2
        then uncompress_line (dr_p^, dr_p^.grn);
      if dr_p^.image.ncomp >= 3
        then uncompress_line (dr_p^, dr_p^.blu);
      if dr_p^.image.ncomp >= 4
        then uncompress_line (dr_p^, dr_p^.comp4);
      if dr_p^.image.ncomp >= 5
        then uncompress_line (dr_p^, dr_p^.comp5);
      if dr_p^.image.ncomp >= 6
        then uncompress_line (dr_p^, dr_p^.comp6);
      if dr_p^.image.ncomp >= 7
        then uncompress_line (dr_p^, dr_p^.comp7);
      if dr_p^.image.ncomp >= 8
        then uncompress_line (dr_p^, dr_p^.comp8);
      end
    else begin                         {component are sequential per pixel}
      uncompress_line (dr_p^, dr_p^.red); {make uncompressed pixel data}
      end
    ;

  pred_line (dr_p^);                   {apply predictor to uncompressed scan line}
{
*   Pixel loop.  Come back here each new pixel in this scan line.
}
  for x := 0 to dr_p^.image.x_size - 1 do begin {once for each pixel in scan line}
{
*   We will use local routine GET_LUT_ENT to get the pointer to the LUT
*   entry indicated by a raw pixel component.  Here we handle psuedo color
*   versus true color, and RGB true color values stored together or in
*   separate strips.
}
    if dr_p^.image.truecolor
      then begin                       {there are separate true color components}
        if dr_p^.image.rgb_sep
          then begin                   {component values come from separate strips}
            get_lut_ent (dr_p^, dr_p^.red, ent_p); {get RED LUT entry pointer}
            comp[1] := ent_p^.red;
            get_lut_ent (dr_p^, dr_p^.grn, ent_p);
            comp[2] := ent_p^.grn;
            get_lut_ent (dr_p^, dr_p^.blu, ent_p);
            comp[3] := ent_p^.blu;
            if dr_p^.image.ncomp >= 4 then
              comp[4] := comp_val(dr_p^, dr_p^.comp4);
            if dr_p^.image.ncomp >= 5 then
              comp[5] := comp_val(dr_p^, dr_p^.comp5);
            if dr_p^.image.ncomp >= 6 then
              comp[6] := comp_val(dr_p^, dr_p^.comp6);
            if dr_p^.image.ncomp >= 7 then
              comp[7] := comp_val(dr_p^, dr_p^.comp7);
            if dr_p^.image.ncomp >= 8 then
              comp[8] := comp_val(dr_p^, dr_p^.comp8);
            end
          else begin                   {component values come from same strip}
            get_lut_ent (dr_p^, dr_p^.red, ent_p); {get RED LUT entry pointer}
            comp[1] := ent_p^.red;
            get_lut_ent (dr_p^, dr_p^.red, ent_p);
            comp[2] := ent_p^.grn;
            get_lut_ent (dr_p^, dr_p^.red, ent_p);
            comp[3] := ent_p^.blu;
            if dr_p^.image.ncomp >= 4 then
              comp[4] := comp_val(dr_p^, dr_p^.red);
            if dr_p^.image.ncomp >= 5 then
              comp[5] := comp_val(dr_p^, dr_p^.red);
            if dr_p^.image.ncomp >= 6 then
              comp[6] := comp_val(dr_p^, dr_p^.red);
            if dr_p^.image.ncomp >= 7 then
              comp[7] := comp_val(dr_p^, dr_p^.red);
            if dr_p^.image.ncomp >= 8 then
              comp[8] := comp_val(dr_p^, dr_p^.red);
            end
          ;
        {
        *   The pixel component values are in the COMP[1-N] array.
        }
        case dr_p^.image.photometric of
photo_cmyk_k: begin                    {components 1-4 are C, M, Y, and K}
            scan[x].red := 65535 - comp[1] - comp[4];
            scan[x].grn := 65535 - comp[2] - comp[4];
            scan[x].blu := 65535 - comp[3] - comp[4];
            end;
otherwise                              {assume components 1-3 are regular R, G, B}
          scan[x].red := comp[1];
          scan[x].grn := comp[2];
          scan[x].blu := comp[3];
          end;
        end                            {done with true color case}
      else begin                       {there is only one component value}
        get_lut_ent (dr_p^, dr_p^.red, ent_p); {get RED LUT entry pointer}
        scan[x].red := ent_p^.red;     {get final pixel value}
        scan[x].grn := ent_p^.grn;
        scan[x].blu := ent_p^.blu;
        end                            {end of pseudo color case}
      ;                                {end of true/pseudo color cases}
{
*   The RGB pixel value has been set.  Now take care of alpha, if present.
}
    if dr_p^.image.comp_alpha > 0
      then begin                       {there is an alpha value}
        scan[x].alpha := comp[dr_p^.image.comp_alpha]; {fetch alpha value}
        if not dr_p^.image.alpha_premult then begin {color not pre-multiplied ?}
          f := scan[x].alpha / 65535.0; {make FP 0.0 to 1.0 alpha fraction}
          scan[x].red := trunc(scan[x].red * f + 0.5); {pass back premult RGB}
          scan[x].grn := trunc(scan[x].grn * f + 0.5);
          scan[x].blu := trunc(scan[x].blu * f + 0.5);
          end;                         {done handling unpremultiplied RGB}
        end
      else begin                       {no alpha value is present}
        scan[x].alpha := 65535;        {set alpha to totally opaque}
        end
      ;
    end;                               {back for next pixel in this scan}
  img.next_y := img.next_y + 1;        {update Y coor of next scan line to read}
  end;
{
*************************************************************************
*
*   Global routine to re-position to the first scan line in the image file.
}
procedure img_d_tif_rewind (           {reposition to start of image file}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}

var
  dr_p: data_read_p_t;                 {pointer to our private TIFF read data}
  di_p: img_conn2_p_t;                 {pointer to IMG library private data}

begin
  sys_error_none (stat);               {init to no errors}
  di_p := img.data_p;                  {get pointer to IMG library private data}
  dr_p := di_p^.data_p;                {get pointer to our private TIFF file data}

  dr_p^.lines_left := 0;               {number of scan lines left in curr strip}
  dr_p^.strip_next_p := dr_p^.image.strip0_p; {set pointer to next set of strips}

  if dr_p^.image.rgb_sep
    then begin                         {each component is in a separate strip ?}
      if dr_p^.image.ncomp >= 1 then reset_strip_state (dr_p^, dr_p^.red);
      if dr_p^.image.ncomp >= 2 then reset_strip_state (dr_p^, dr_p^.grn);
      if dr_p^.image.ncomp >= 3 then reset_strip_state (dr_p^, dr_p^.blu);
      if dr_p^.image.ncomp >= 4 then reset_strip_state (dr_p^, dr_p^.comp4);
      if dr_p^.image.ncomp >= 5 then reset_strip_state (dr_p^, dr_p^.comp5);
      if dr_p^.image.ncomp >= 6 then reset_strip_state (dr_p^, dr_p^.comp6);
      if dr_p^.image.ncomp >= 7 then reset_strip_state (dr_p^, dr_p^.comp7);
      if dr_p^.image.ncomp >= 8 then reset_strip_state (dr_p^, dr_p^.comp8);
      end
    else begin                         {all components sequential per pixel}
      reset_strip_state (dr_p^, dr_p^.red); {we use RED strip state for single strip}
      end
    ;

  img.next_y := 0;                     {reset Y for next scan line to be read}
  end;
{
*************************************************************************
*
*   Global routine to close TIFF file that was open for reading.
}
procedure img_d_tif_close_read (       {close TIFF file that was open for reading}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}

var
  dr_p: data_read_p_t;                 {pointer to our private TIFF read data}
  di_p: img_conn2_p_t;                 {pointer to IMG library private data}

begin
  sys_error_none (stat);               {init to no errors}
  di_p := img.data_p;                  {get pointer to IMG library private data}
  dr_p := di_p^.data_p;                {get pointer to our TIFF file private data}

  file_close (dr_p^.conn);             {close TIFF file}
  end;
{
*************************************************************************
*
*   Local subroutine BUF_CREATE (DW, LEN, BUF_P, STAT)
*
*   Create a new buffer descriptor and buffer.  LEN will be the length
*   of the new buffer.
*   BUF_P will be returned pointing to the new buffer descriptor.
*   The buffer will be mapped to the current end of the file, which
*   automatically extends the file.
}
procedure buf_create (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in      len: sys_int_adr_t;          {required size of new buffer}
  out     buf_p: obuf_p_t;             {pointer to new buffer descriptor}
  out     stat: sys_err_t);            {returned completion status code}
  val_param; internal;

var
  ofs: sys_int_adr_t;                  {file offset for start of buffer}

begin
  util_mem_grab (                      {allocate memory for buffer descriptor}
    sizeof(buf_p^), dw.di_p^.mem_p^, false, buf_p);
  ofs := (dw.ofs_next + 3) & ~3;       {align new buffer at multiple of 4}
  file_map (                           {map new buffer to file}
    dw.conn,                           {file connection handle}
    ofs,                               {file offset for start of buffer}
    len,                               {length of buffer}
    [file_rw_write_k],                 {we only need to write the new region}
    buf_p^.buf_p,                      {returned pointer to mapped buffer}
    buf_p^.len,                        {actual length mapped}
    buf_p^.maph,                       {returned handle to mapped region}
    stat);
  if sys_error(stat) then return;

  buf_p^.ofs_start := ofs;             {save file offset for start of buffer}
  buf_p^.ofs_curr := buf_p^.ofs_start; {init file offset for next byte to write}
  buf_p^.next_p := buf_p^.buf_p;       {init pointer to next byte to write}
  buf_p^.len := len;                   {set length of this buffer}
  buf_p^.cont := false;                {init to error if write past buffer end}

  dw.ofs_next := ofs + len;            {update file offset for right after EOF}
  end;
{
*************************************************************************
*
*   Local subroutine BUF_CLOSE (DW, BUF_P)
*
*   Close our use of the buffer pointed to by BUF_P.  BUF_P will be
*   returned NIL.
}
procedure buf_close (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in out  buf_p: obuf_p_t);            {pointer to buffer to close, returned NIL}
  val_param; internal;

begin
  file_map_done (buf_p^.maph);         {unmap the buffer from the file}
  buf_p := nil;
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_I8 (DW, I)
*
*   Write 8 bit integer value I to the current output buffer.
}
procedure write_i8 (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in      i: sys_int_conv8_t);         {value to write in low 8 bits}
  val_param; internal;

var
  buf_p: obuf_p_t;                     {points to new buffer on automatic extend}
  stat: sys_err_t;

label
  new_buf;

begin
new_buf:                               {back here on created new buffer}
  with dw.buf_write_p^: b do begin     {B is descriptor for current output buffer}
    if (b.ofs_curr - b.ofs_start) >= b.len then begin {no more room in this buffer ?}
      if b.cont then begin             {automatically make new buffer on overflow ?}
        if b.ofs_curr <> dw.ofs_next then begin
          sys_message_bomb ('img', 'tif_buf_not_end', nil, 0);
          end;
        buf_create (dw, b.len, buf_p, stat); {create new buffer}
        sys_error_abort (stat, 'img', 'tif_buf_create', nil, 0);
        buf_p^.cont := true;           {new buffer allows continuation too}
        if dw.buf_write_p = dw.buf_dir_p then begin {pixel data buffer ?}
          dw.buf_dir_p := buf_p;       {set master copy of buffer pointer also}
          end;
        buf_close (dw, dw.buf_write_p); {close old buffer}
        dw.buf_write_p := buf_p;       {make new buffer current}
        goto new_buf;                  {retry with the new buffer}
        end;
      sys_message_bomb ('img', 'tif_buf_overflow', nil, 0);
      end;                             {done handling buffer overflow}
    b.next_p^ := chr(i & 255);         {write the byte into the buffer}
    b.next_p := univ_ptr(              {advance write pointer to next byte to write}
      sys_int_adr_t(b.next_p) + 1);
    b.ofs_curr := b.ofs_curr + 1;      {update file offset for next byte to write}
    end;                               {done with B abbreviation}
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_BITS (DW, B, NB)
*
*   Write the low NB bits in B to the current output buffer.  B must
*   contain zeros in the unused bits.
}
procedure write_bits (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in      b: sys_int_machine_t;        {bits to write, low justified}
  in      nb: sys_int_machine_t);      {number of bits to write}
  val_param; internal;

var
  ab: sys_int_machine_t;               {all bits to write out}
  nab: sys_int_machine_t;              {number of bits in NAB}
  mask: sys_int_machine_t;             {mask for valid bits in AB}

label
  loop;

begin
  ab := lshft(dw.uwb, nb) ! b;         {old and new bits to write out}
  nab := dw.n_uwb + nb;                {number of bits in AB}
  mask := ~lshft(~1, nab);             {mask for valid bits in AB}

loop:                                  {back here to handle next chunk of bits}
  if nab >= 8 then begin               {we can write out a whole byte ?}
    write_i8 (dw, rshft(ab, nab - 8)); {write the byte}
    nab := nab - 8;                    {8 bits less left to write}
    mask := rshft(mask, 8);            {update valid bits mask for AB}
    ab := ab & mask;                   {mask in only remaining valid bits}
    goto loop;
    end;                               {done handling writing a whole byte}

  dw.uwb := ab;                        {save unwritable bits in private data}
  dw.n_uwb := nab;
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_STR (DW, S)
*
*   Write the string S into the current TIFF file output buffer.
*   Although S is a var string, the string will be formatted according to
*   TIFF file rules.
}
procedure write_str (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in      s: univ string_var_arg_t);   {input string}
  val_param; internal;

var
  i: sys_int_machine_t;                {loop counter}

begin
  for i := 1 to s.len do begin         {once for each character in the string}
    write_i8 (dw, ord(s.str[i]));      {write this character}
    end;
  write_i8 (dw, 0);                    {write terminating null byte}
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_I16 (DW, I)
*
*   Write 16 bit integer value I to the file.
}
procedure write_i16 (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in      i: sys_int_conv16_t);        {value to write in low 16 bits}
  val_param; internal;

begin
  write_i8 (dw, rshft(i, 8));
  write_i8 (dw, i);
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_I32 (DW, I)
*
*   Write 16 bit integer value I to the file.
}
procedure write_i32 (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in      i: sys_int_conv32_t);        {value to write in low 16 bits}
  val_param; internal;

begin
  write_i8 (dw, rshft(i, 24));
  write_i8 (dw, rshft(i, 16));
  write_i8 (dw, rshft(i, 8));
  write_i8 (dw, i);
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_RAT (DW, R)
*
*   Write floating point value R to TIFF file as a "rational" number.
}
procedure write_rat (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in      r: real);                    {input value}
  val_param; internal;

const
  maxval = 2 ** 29;

begin
  if r >= 1.0
    then begin                         {value > 1, use max for numerator}
      write_i32 (dw, maxval);
      write_i32 (dw, round(maxval / r));
      end
    else begin                         {value < 1, use max for denominator}
      write_i32 (dw, round(maxval / r));
      write_i32 (dw, maxval);
      end
    ;
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_TAG (DW, ID, DTYPE, CNT)
*
*   Write the next tag into the TIFF file "directory".  The low level
*   writing state will be set up for writing the data.  It is up to the
*   caller to write this data correctly.
}
procedure write_tag (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in      id: sys_int_machine_t;       {tag ID, use F_xxx_K}
  in      dtype: sys_int_machine_t;    {data type ID, use TYPE_xxx_K}
  in      cnt: sys_int_machine_t);     {number of data items in DATA}
  val_param; internal;

const
  max_msg_parms = 3;                   {max parameters we can pass to a message}

var
  dsize: sys_int_adr_t;                {total size of data for this tag}
  count: sys_int_adr_t;                {actual count value to write to entry}
  buf_p: obuf_p_t;                     {pointer to buffer for remote data, if any}
  msg_parm:                            {parameter references for messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
  stat: sys_err_t;                     {completion status code}

begin
  if id < dw.tag_id_last then begin    {trying to write tags in reverse order ?}
    sys_msg_parm_int (msg_parm[1], dw.tag_id_last);
    sys_msg_parm_int (msg_parm[2], id);
    sys_message_bomb ('img', 'tif_tag_write_bad_order', msg_parm, 2);
    end;
  dw.tag_id_last := id;                {update ID of last tag written}

  count := cnt;                        {init count value to write to tag}

  case dtype of                        {what kind of data is being written}
type_i8_k: begin
      dsize := cnt;
      end;
type_ascii_k: begin
      count := cnt + 1;
      dsize := count;
      end;
type_i16_k: begin
      dsize := cnt * 2;
      end;
type_i32_k: begin
      dsize := cnt * 4;
      end;
type_rat_k: begin
      dsize := cnt * 8;
      end;
otherwise
    sys_msg_parm_int (msg_parm[1], id);
    sys_msg_parm_int (msg_parm[2], dtype);
    sys_msg_parm_int (msg_parm[3], cnt);
    sys_message_bomb ('img', 'tif_dtype_unexpected2', msg_parm, 3);
    end;

  write_i16 (dw, id);                  {write the tag ID}
  write_i16 (dw, dtype);               {write data type ID}
  write_i32 (dw, count);               {write number of data items}

  if (dsize > 4) then begin            {indirect data in remote buffer ?}
    buf_create (dw, dsize, buf_p, stat);
    sys_error_abort (stat, 'img', 'tif_err_write', nil, 0);
    write_i32 (dw, buf_p^.ofs_curr);   {write file offset to remote data}
    dw.buf_write_p := buf_p;           {make tag data buffer the current out buffer}
    end;
  end;
{
*************************************************************************
*
*   Local subroutine CLOSE_TAG (DW)
*
*   Finish writing to the current tag.  Any neccessary padding will be
*   added so that the next byte to write is the start of the next tag.
*   If a remote data buffer was used, it will be closed, unless the
*   the current write pointer it NIL.  The current write buffer will be
*   the buffer where the next tag is to be written.
}
procedure close_tag (
  in out  dw: data_write_t);           {private data for writing TIFF file}
  internal;

begin
  if                                   {close old remote data buffer ?}
      (dw.buf_write_p <> dw.buf_dir_p) and
      (dw.buf_write_p <> nil)
      then begin
    buf_close (dw, dw.buf_write_p);    {close the buffer}
    end;
  dw.buf_write_p := dw.buf_dir_p;      {set write buffer to directory buffer}
  while dw.buf_write_p^.ofs_curr < dw.ofs_next_ent do begin {pad to next entry}
    write_i8 (dw, 0);
    end;
  dw.ofs_next_ent := dw.ofs_next_ent + 12; {make offset for next entry start}
  end;
{
*************************************************************************
*
*   Local subroutine LZW_NODE_NEW (DW, NODE_P)
*
*   Create a new node for the LZW codes tree.  It will be initialized
*   to NO CODE for all possible branches.
}
procedure lzw_node_new (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  out     node_p: branch_node_p_t);    {returned pointing to new node}
  internal;

var
  i: sys_int_machine_t;                {loop counter}

begin
  util_mem_grab (                      {allocate memory for the new node}
    sizeof(node_p^), dw.mem_p^, false, node_p);

  for i := 0 to 7 do begin             {once for each word in leaf flags array}
    node_p^.leaf[i] := 0;              {init all branches to be node pointers}
    end;

  for i := 0 to 255 do begin           {once for each branch pointer}
    node_p^.branch[i].node_p := nil;
    end;
  end;
{
*************************************************************************
*
*   Local subroutine LZW_CLEAR (DW)
*
*   Reset the LZW codes tree.  The top node will be all filled in, with
*   each BRANCH entry pointing to another node.  These nodes will all have
*   NIL pointers.
}
procedure lzw_clear (
  in out  dw: data_write_t);           {private data for writing TIFF file}
  internal;

var
  i: sys_int_machine_t;                {loop counter}

begin
  if dw.mem_p <> nil then begin        {an old memory context exists ?}
    util_mem_context_del (dw.mem_p);   {blow away old codes tree memory}
    end;

  util_mem_context_get (dw.di_p^.mem_p^, dw.mem_p); {create new mem context}
  dw.mem_p^.pool_size :=               {size pool to alloc at one time}
    sizeof(branch_node_t) * 64;
  dw.mem_p^.max_pool_chunk :=          {max size to alloc from pool at one time}
    sizeof(branch_node_t) * 2;

  dw.node_top.code_end := 0;           {should never be used}

  for i := 0 to 7 do begin             {once for each LEAF array entry in top node}
    dw.node_top.leaf[i] := 0;          {all BRANCH entries will be pointers}
    end;

  for i := 0 to 255 do begin           {once for each BRANCH array entry in top node}
    lzw_node_new (dw, dw.node_top.branch[i].node_p); {create initil branch here}
    dw.node_top.branch[i].node_p^.code_end := i;
    end;

  dw.code_next := lzw_code_first_var_k; {next code to create table entry for}
  dw.code_resize := 512;               {code value to trigger size change}
  dw.code_bits := lzw_bits_first_code_k; {initial size of codes to write}
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_STRIP_INIT (SW)
*
*   Init for writing the first pixel data to a new strip.
}
procedure write_strip_init (
  in out  dw: data_write_t);           {private data for writing TIFF file}
  internal;

begin
  dw.buf_write_p := dw.buf_dir_p;      {make pixel data buffer the current buffer}
  while (dw.buf_write_p^.ofs_curr & 3) <> 0 do begin {pad until aligned mult of 4}
    write_i8 (dw, 0);
    end;

  dw.buf_write_p := dw.buf_strofs_p;   {make strips offset buffer current}
  if dw.n_strips = 1 then begin        {strip offset directly in tag data ?}
    dw.buf_write_p^.next_p := dw.str0_ofs_p; {set up to write strip offset word}
    dw.buf_write_p^.ofs_curr :=
      sys_int_adr_t(dw.buf_write_p^.next_p) - sys_int_adr_t(dw.buf_write_p^.buf_p) +
      dw.buf_write_p^.ofs_start;
    end;
  write_i32 (dw, dw.buf_dir_p^.ofs_curr); {write file offset for this new strip}
  dw.strip_start := dw.buf_dir_p^.ofs_curr; {save file offset for start of strip}

  dw.buf_write_p := dw.buf_dir_p;      {make pixel data buffer the current buffer}
  dw.lines_left := dw.lines_strip;     {reset number of lines left this strip}

  lzw_clear (dw);                      {init LZW compression state}
  dw.uwb := 0;                         {init to no unwritten bits waiting}
  dw.n_uwb := 0;
  dw.node_p := addr(dw.node_top);      {init node to use with next input char}
  dw.str.len := 0;                     {init to no accumulated input string}

  write_bits (dw, lzw_code_clear_k, lzw_bits_first_code_k); {write CLEAR code}
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_STRIP_CLOSE (DW)
*
*   Close writing the current strip.
}
procedure write_strip_close (
  in out  dw: data_write_t);           {private data for writing TIFF file}
  internal;

begin
  if dw.str.len > 0 then begin         {unwritten input string remains ?}
    write_bits (dw, dw.node_p^.code_end, dw.code_bits); {write code for last string}
    dw.code_next := dw.code_next + 1;  {make value of next code to create}
    if dw.code_next >= dw.code_resize then begin {need to increase size of codes ?}
      dw.code_bits := dw.code_bits + 1; {codes now written one bit larger}
      end;
    end;
  write_bits (dw, lzw_code_eoi_k, dw.code_bits); {write END OF INFORMATION code}
  if dw.n_uwb > 0 then begin           {unwritten bits left over ?}
    write_i8 (dw, lshft(dw.uwb, 8 - dw.n_uwb)); {write last partial byte}
    end;

  dw.buf_write_p := dw.buf_strlen_p;   {make strip lengths buffer current}
  if dw.n_strips = 1 then begin        {strip offset directly in tag data ?}
    dw.buf_write_p^.next_p := dw.str0_len_p; {set up to write strip length word}
    dw.buf_write_p^.ofs_curr :=
      sys_int_adr_t(dw.buf_write_p^.next_p) - sys_int_adr_t(dw.buf_write_p^.buf_p) +
      dw.buf_write_p^.ofs_start;
    end;
  write_i32 (dw, dw.buf_dir_p^.ofs_curr - dw.strip_start); {write length of strip}
  end;
{
*************************************************************************
*
*   Global routine to open TIFF file for write.
*
*   We always write 8 bit RGB, LZW compressed with horizontal differencing.
*   We write 8 bit alpha whenever the PARMS command ALPHA indicates more
*   than zero bits.
}
procedure img_d_tif_open_write (       {open TIFF file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}

const
  n_ents_fixed_k = 15;                 {min number of entries we always write}
  strip_size_max_k = 64000;            {max uncompressed strip size}

var
  di_p: img_conn2_p_t;                 {points to private IMG library data}
  dw_p: data_write_p_t;                {points to private TIFF file data}
  n_ents: sys_int_machine_t;           {number of entries we will write}
  sz: sys_int_adr_t;                   {scratch size value}
  i: sys_int_machine_t;                {scratch integer and loop counter}
  p: string_index_t;                   {PARMS parse index}
  opt: string_var32_t;                 {PARMS option name}
  parm: string_var32_t;                {parameter for option name in OPT}
  pick: sys_int_machine_t;             {number of token picked from list}
  stat2: sys_err_t;                    {scratch status to prevent corrupting STAT}

label
  next_opt, bad_parm, done_opts, abort1, abort2;

begin
  opt.max := sizeof(opt.str);          {init local var strings}
  parm.max := sizeof(parm.str);

  di_p := img.data_p;                  {get pointer to IMG library private data}
  util_mem_grab (                      {allocate memory for private TIFF file data}
    sizeof(dw_p^), di_p^.mem_p^, true, dw_p);
  dw_p^.debug := debug_level;          {get debug level and save in state block}
  file_open_map (                      {open TIFF file for mapped writes}
    img.tnam,                          {file name}
    '',                                {optional file name suffixes}
    [file_rw_write_k],                 {we only need to write to this file}
    dw_p^.conn,                        {returned connection handle}
    stat);                             {returned completion status code}
  if sys_error(stat) then goto abort1;
{
*   Init our private TIFF file data.
}
  dw_p^.di_p := di_p;
  dw_p^.ofs_next_ent := 0;
  dw_p^.buf_write_p := nil;
  dw_p^.buf_dir_p := nil;
  dw_p^.buf_strofs_p := nil;
  dw_p^.buf_strlen_p := nil;
  dw_p^.tag_id_last := -1;
  dw_p^.ofs_next := 0;
  dw_p^.dx := img.x_size;
  dw_p^.mem_p :=  nil;
  dw_p^.str.max := sizeof(dw_p^.str.str);
  dw_p^.ncomp := 3;
  dw_p^.alpha := false;

  img.bits_alpha := 0;
  img.bits_red := 8;
  img.bits_grn := 8;
  img.bits_blu := 8;
  img.bits_max := 8;
{
*   Process PARMS string.
}
  p := 1;                              {init PARMS parse index}

next_opt:                              {back here each new option from PARMS}
  string_token (parms, p, opt, stat);  {extract next token from PARMS}
  if string_eos(stat) then goto done_opts; {reached end of PARMS string ?}
  string_upcase (opt);                 {make upper case for token matching}
  string_tkpick80 (opt,                {pick token from list of legal keywords}
    'ALPHA RED GREEN BLUE',
    pick);
  case pick of
{
*   ALPHA <bit precision>
}
1: begin
  string_token (parms, p, parm, stat);
  if sys_error(stat) then goto bad_parm;
  string_t_int (parm, i, stat);
  if sys_error(stat) then goto bad_parm;
  if i > 0
    then begin                         {alpha is enabled}
      img.bits_alpha := 8;
      dw_p^.alpha := true;
      end
    else begin                         {alpha is disabled}
      img.bits_alpha := 0;
      dw_p^.alpha := false;
      end
    ;
  end;
{
*   RED <bit precision>
}
2: begin
  string_token (parms, p, parm, stat);
  if sys_error(stat) then goto bad_parm;
  string_t_int (parm, i, stat);
  if sys_error(stat) then goto bad_parm;
  end;
{
*   GREEN <bit precision>
}
3: begin
  string_token (parms, p, parm, stat);
  if sys_error(stat) then goto bad_parm;
  string_t_int (parm, i, stat);
  if sys_error(stat) then goto bad_parm;
  end;
{
*   BLUE <bit precision>
}
4: begin
  string_token (parms, p, parm, stat);
  if sys_error(stat) then goto bad_parm;
  string_t_int (parm, i, stat);
  if sys_error(stat) then goto bad_parm;
  end;
{
*   Unrecognized option name.
}
otherwise
    sys_stat_set (img_subsys_k, img_err_bad_fmt_opt_k, stat);
    sys_stat_parm_vstr (opt, stat);
    sys_stat_parm_vstr (img.tnam, stat);
    return;
    end;                               {done with option name cases}
  goto next_opt;                       {back and process next option from PARMS}

bad_parm:                              {jump here on bad parameter to particular OPT}
  sys_stat_set (img_subsys_k, img_err_bad_fmt_parm_k, stat);
  sys_stat_parm_vstr (parm, stat);
  sys_stat_parm_vstr (opt, stat);
  sys_stat_parm_vstr (img.tnam, stat);
  return;
done_opts:                             {jump here when processed whole PARMS string}
{
*   Init more of our TIFF file writing state.
}
  if dw_p^.alpha
    then begin                         {we will write alpha component}
      dw_p^.ncomp := 4;
      end
    else begin                         {just write RGB}
      dw_p^.ncomp := 3;
      end
    ;
  dw_p^.lines_strip := max(1, min(img.y_size, {scan lines per TIFF file strip}
    strip_size_max_k div (img.x_size * dw_p^.ncomp)));
  dw_p^.n_strips :=                    {number of strips needed for the whole image}
    (img.y_size + dw_p^.lines_strip - 1) div dw_p^.lines_strip;
  dw_p^.lines_left := dw_p^.lines_strip; {lines left to write in current strip}
{
*   Create the buffer that will hold the file header and the "directory".
}
  n_ents := n_ents_fixed_k + img.comm.n; {total ents is fixed ents + comment lines}
  if dw_p^.alpha then begin            {need additional entries for alpha ?}
    n_ents := n_ents + 1;              {account for EXTRA_SAMPLES field}
    end;
  sz := (n_ents * 12) + 14;            {total size needed for file header buffer}
  buf_create (dw_p^, sz, dw_p^.buf_dir_p, stat);
  if sys_error(stat) then goto abort2;
  dw_p^.buf_write_p := dw_p^.buf_dir_p; {set new buffer as current write buffer}
{
*   Write file header.
}
  write_i8 (dw_p^, 77);                {indicate forward byte ordering}
  write_i8 (dw_p^, 77);
  write_i16 (dw_p^, 42);               {version number}
  write_i32 (dw_p^, 8);                {offset to first (and only) directory}
{
*   Write directory header.  This is only the number of entries in the
*   directory.
}
  write_i16 (dw_p^, n_ents);           {number of entries in this directory}
  dw_p^.ofs_next_ent :=                {init offset of next entry after next close}
    dw_p^.buf_write_p^.ofs_curr + 12;
{
*   Write the directory entries.  These must be written in ascending tag
*   order.
}
  write_tag (dw_p^, f_new_subfile_type_k, type_i32_k, 1);
  write_i32 (dw_p^, 0);
  close_tag (dw_p^);

  write_tag (dw_p^, f_image_width_k, type_i32_k, 1);
  write_i32 (dw_p^, img.x_size);
  close_tag (dw_p^);

  write_tag (dw_p^, f_image_length_k, type_i32_k, 1);
  write_i32 (dw_p^, img.y_size);
  close_tag (dw_p^);

  write_tag (dw_p^, f_bits_per_sample_k, type_i16_k, dw_p^.ncomp);
  for i := 1 to dw_p^.ncomp do begin   {once for each pixel component}
    write_i16 (dw_p^, 8);              {all pixel components will be 8 bits}
    end;
  close_tag (dw_p^);

  write_tag (dw_p^, f_compression_k, type_i16_k, 1);
  write_i16 (dw_p^, compress_lzw_k);
  close_tag (dw_p^);

  write_tag (dw_p^, f_photometric_interpretation_k, type_i16_k, 1);
  write_i16 (dw_p^, photo_rgb_k);
  close_tag (dw_p^);

  string_list_pos_start (img.comm);    {position to before first comment line}
  for i := 1 to img.comm.n do begin    {once for each comment line in image}
    string_list_pos_rel (img.comm, 1); {advance to next comment line}
    write_tag (dw_p^, f_image_description_k, type_ascii_k, img.comm.str_p^.len);
    write_str (dw_p^, img.comm.str_p^);
    close_tag (dw_p^);
    end;

  write_tag (dw_p^, f_strip_offsets_k, type_i32_k, dw_p^.n_strips);
  dw_p^.buf_strofs_p := dw_p^.buf_write_p; {save pointer to strips offsets buffer}
  dw_p^.str0_ofs_p := dw_p^.buf_write_p^.next_p; {save pointer to strip 0 data}
  dw_p^.buf_write_p := nil;            {prevent closing strips offsets buffer}
  close_tag (dw_p^);

  write_tag (dw_p^, f_samples_per_pixel_k, type_i16_k, 1);
  write_i16 (dw_p^, dw_p^.ncomp);
  close_tag (dw_p^);

  write_tag (dw_p^, f_rows_per_strip_k, type_i32_k, 1);
  write_i32 (dw_p^, dw_p^.lines_strip);
  close_tag (dw_p^);

  write_tag (dw_p^, f_strip_byte_counts_k, type_i32_k, dw_p^.n_strips);
  dw_p^.buf_strlen_p := dw_p^.buf_write_p; {save pointer to strip lengths buffer}
  dw_p^.str0_len_p := dw_p^.buf_write_p^.next_p; {save pointer to strip 0 data}
  dw_p^.buf_write_p := nil;            {prevent closing strip lengths buffer}
  close_tag (dw_p^);

  write_tag (dw_p^, f_x_resolution_k, type_rat_k, 1);
  write_rat (dw_p^, (img.x_size / img.y_size) / img.aspect);
  close_tag (dw_p^);

  write_tag (dw_p^, f_y_resolution_k, type_rat_k, 1);
  write_rat (dw_p^, 1.0);
  close_tag (dw_p^);

  write_tag (dw_p^, f_planar_configuration_k, type_i16_k, 1);
  write_i16 (dw_p^, planar_pix_k);
  close_tag (dw_p^);

  write_tag (dw_p^, f_resolution_unit_k, type_i16_k, 1);
  write_i16 (dw_p^, 1);
  close_tag (dw_p^);

  write_tag (dw_p^, f_predictor_k, type_i16_k, 1);
  write_i16 (dw_p^, pred_hdiff_k);     {horizontal differencing}
  close_tag (dw_p^);

  if dw_p^.alpha then begin
    write_tag (dw_p^, f_extra_samples_k, type_i16_k, 1);
    write_i16 (dw_p^, extra_alpha_k);
    close_tag (dw_p^);
    end;

  write_i32 (dw_p^, 0);                {indicate end of directories chain}
  if dw_p^.n_strips > 1 then begin     {don't need to write back into tag values ?}
    buf_close (dw_p^, dw_p^.buf_write_p); {all done writing file header}
    end;
{
*   Init for writing pixel data to the first strip.
}
  buf_create (dw_p^, 8192, dw_p^.buf_dir_p, stat); {create pixel data out buffer}
  if sys_error(stat) then goto abort2;
  dw_p^.buf_dir_p^.cont := true;       {this buffer gets continued indefinately}
  write_strip_init (dw_p^);            {set up state for writing first pixel data}
{
*   Fill in IMG library private data block.
}
  di_p^.data_p := dw_p;                {save pointer to private TIFF file data}
  di_p^.write_scan1 := addr(img_d_tif_write_scan1);
  di_p^.close := addr(img_d_tif_close_write);
  return;                              {normal return, no errors}

abort2:                                {error with file open}
  file_close (dw_p^.conn);             {try to close file}
  file_delete_name (img.tnam, stat2);  {delete file, if possible}

abort1:                                {error with file closed}
  util_mem_ungrab (dw_p, di_p^.mem_p^); {deallocate private TIFF file memory}
  end;
{
*************************************************************************
*
*   Local subroutine WRITE_SCAN (DW, SCAN)
*
*   Write the data from the scan line to the current strip.
}
procedure write_scan (
  in out  dw: data_write_t;            {private data for writing TIFF file}
  in      scan: univ img_scan1_arg_t); {scan line of pixels}
  internal;

var
  x: sys_int_machine_t;                {scan line X index}
  col: img_col_k_t;                    {next color component to read from scan pix}
  ci: sys_int_machine_t;               {component loop counter}
  c: sys_int_machine_t;                {0-255 value of current pixel component}
  last_red, last_grn, last_blu, last_alp: {previous values for hor differencing}
    sys_int_machine_t;
  i: sys_int_machine_t;                {scratch integer}
  node_p: branch_node_p_t;             {scratch pointer to LZW codes tree node}
  leaf_ind: sys_int_machine_t;         {LEAF array index for this character}
  mask: sys_int_conv32_t;              {1 bit mask for this leaf bit}
  leaf: boolean;                       {TRUE if curr node is leaf for curr string}

label
  next_char;

begin
  x := 0;                              {init X coordinate of next pixel}
  last_red := 0;                       {init horizontal differencing state}
  last_grn := 0;
  last_blu := 0;
  last_alp := 0;
  col := img_col_red_k;                {init component ID to extract next}

  for ci := 1 to dw.dx * dw.ncomp do begin {once for each uncompressed data byte}
    case col of                        {fetch right component from this pixel}
img_col_red_k: begin
        i := scan[x].red;
        c := (i - last_red) & 255;
        last_red := i;
        col := img_col_grn_k;
        end;
img_col_grn_k: begin
        i := scan[x].grn;
        c := (i - last_grn) & 255;
        last_grn := i;
        col := img_col_blu_k;
        end;
img_col_blu_k: begin
        i := scan[x].blu;
        c := (i - last_blu) & 255;
        last_blu := i;
        if dw.alpha
          then begin
            col := img_col_alpha_k;
            end
          else begin
            col := img_col_red_k;
            x := x + 1;
            end
          ;
       end;
img_col_alpha_k: begin
        i := scan[x].alpha;
        c := (i - last_alp) & 255;
        last_alp := i;
        col := img_col_red_k;
        x := x + 1;
        end;
      end;                             {C is value of this component}
{
**********************
*
*   The next byte to write is in C.
}
  leaf_ind := rshft(c, 5);             {make LEAF array index}
  mask := lshft(1, c & 31);            {make mask for particular LEAF bit}
  leaf := (dw.node_p^.leaf[leaf_ind] & mask) <> 0; {TRUE if hit leaf node}
{
*   Handle case where we have reached a leaf in the tree.  This means that
*   a code exists for the current string, but not for the string with any
*   other characters added to the end.  The leaf bit is really a short way
*   to indicate that all the pointers at the next node would be NIL.
*   Since a code will be created for a string that has one more character
*   than the current string, we will create the next node with all NIL
*   pointers.  From then on, it will be as if we had hit a tree node.
}
  if leaf then begin
    lzw_node_new (dw, node_p);         {create and init new node for LZW codes tree}
    node_p^.code_end := dw.node_p^.branch[c].code; {code val for string ending here}
    dw.node_p^.branch[c].node_p := node_p; {link new node to tree}
    dw.node_p^.leaf[leaf_ind] :=       {reset old LEAF bit, BRANCH now contains pnt}
      dw.node_p^.leaf[leaf_ind] & ~mask;
    end;                               {done handling leaf node case}
{
*   This is now definately not a leaf node for the current string.  This means
*   that the BRANCH array NODE_P field is valid instead of the CODE field.
*   The NODE_P field either points to the next node in the tree, or is NIL
*   to indicate that no code exists for that string.  The corresponding
*   string is DW.STR with C appended.
}
  node_p := dw.node_p^.branch[c].node_p; {get next node pointer, may be NIL}
  string_append1 (dw.str, chr(c));     {update string to include new character}
{
*   Now handle case where NODE_P is not NIL.  We just go to the next node in
*   the tree, and go back to handle the next input stream character.
}
  if node_p <> nil then begin          {code still exists for new string ?}
    dw.node_p := node_p;               {advance to next node in tree}
    goto next_char;                    {all done with this char, go to next}
    end;
{
*   The pointer to the next node in the tree is NIL.  This indicates that
*   the new string does not match any existing LZW code.  We must write
*   the code for the old string, and update the tree to contain the new string.
*   The old string is then reset to contain this current character, and
*   the current tree position is updated to reflect this.
}
  write_bits (dw, dw.node_p^.code_end, dw.code_bits); {write code for old string}
  dw.node_p^.leaf[leaf_ind] :=         {set LEAF bit for this string}
    dw.node_p^.leaf[leaf_ind] ! mask;
  dw.node_p^.branch[c].code := dw.code_next; {set LZW code value for new string}
  dw.str.str[1] := chr(c);             {reset accumulated string to new char}
  dw.str.len := 1;
  dw.node_p :=                         {set tree position to new 1-char string}
    dw.node_top.branch[c].node_p;
{
*   Update the next LZW code to make a table entry for.  This may cause
*   an increase in the size of the codes written to the output stream.
*   It may also cause a table overflow, in which case a reset code is
*   written, and the table is reset to initial conditions.
}
  dw.code_next := dw.code_next + 1;    {make value of next code to create}
  if dw.code_next >= dw.code_resize then begin {need to increase size of codes ?}
    dw.code_bits := dw.code_bits + 1;  {codes now written one bit larger}
    dw.code_resize := lshft(dw.code_resize, 1); {update next size change threshold}
    end;
  if dw.code_next >= 4095 then begin   {codes table all filled ?}
    write_bits (dw, lzw_code_clear_k, 12); {write CLEAR code}
    lzw_clear (dw);                    {reset codes tree to initial conditions}
    dw.node_p :=                       {init tree position to left-over character}
      dw.node_top.branch[c].node_p;
    end;

next_char:                             {jump here to advance to next input char}
    end;                               {back for next component this scan line}
  end;
{
*************************************************************************
*
*   Global routine to write line to TIFF file from format 1 scan line.
}
procedure img_d_tif_write_scan1 (      {write next scan line from format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {points to private IMG library data}
  dw_p: data_write_p_t;                {points to private TIFF file data}

begin
  di_p := img.data_p;                  {get pointer to IMG library private data}
  dw_p := di_p^.data_p;                {get pointer to TIFF file private data}

  if dw_p^.lines_left <= 0 then begin  {completely filled old strip ?}
    write_strip_close (dw_p^);         {close out old strip}
    write_strip_init (dw_p^);          {initialize new strip}
    end;

  write_scan (dw_p^, scan);            {write this scan line to current strip}

  dw_p^.lines_left := dw_p^.lines_left - 1; {one less scan line left in this strip}
  img.next_y := img.next_y + 1;        {update coordinate of next scan to write}
  end;
{
*************************************************************************
*
*   Global routine to close TIFF file that was open for writing.
}
procedure img_d_tif_close_write (      {close TIFF file that was open for writing}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {points to private IMG library data}
  dw_p: data_write_p_t;                {points to private TIFF file data}

begin
  di_p := img.data_p;                  {get pointer to IMG library private data}
  dw_p := di_p^.data_p;                {get pointer to TIFF file private data}

  write_strip_close (dw_p^);           {close writing current strip}
  file_close (dw_p^.conn);             {close TIFF file}
  end;
