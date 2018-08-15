{   Private insert file for IMG library.
}
%include 'sys.ins.pas';
%include 'util.ins.pas';
%include 'string.ins.pas';
%include 'file.ins.pas';
%include 'img.ins.pas';

type
{
*   Template for mandatory routines that are used to open connections.  Each
*   driver must have routines that look like these templates.
}
img_open_read_img_proc_t = ^procedure ( {image file driver routine to open for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}

img_open_write_img_proc_t = ^procedure ( {image driver routine to open for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}

img_open_read_filt_proc_t = ^procedure ( {filter routine to open for read}
  in      img_old: img_conn_t;         {image stream filter will use as input}
  in      parms: univ string_var_arg_t; {parameter string passed to filter}
  out     img: img_conn_t;             {connection to output of filter}
  out     stat: sys_err_t);            {completion status code}
{
*   Template for routines that get installed in the internal connection
*   block for a connection.  A driver must implement a minimum subset of these.
*
*   For drivers and filters that read, the OPEN procedure must fill in at least
*   one of the READ_SCANxxx routine pointers.  Drivers and filters that write
*   must fill in at least one of the WRITE_SCANxxx routine pointers.  All drivers
*   and filters must fill in the CLOSE routine pointer.
}
img_read_scan1_proc_t = ^procedure (   {read next scan line as format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

img_read_scan2_proc_t = ^procedure (   {read next scan line as format 2 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

img_rewind_proc_t = ^procedure (       {rewind so that first pixel is next transfer}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}

img_write_scan1_proc_t = ^procedure (  {write next scan line from format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

img_write_scan2_proc_t = ^procedure (  {write next scan line from format 2 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan2_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

img_next_read_proc_t = ^procedure (    {open next image for read in same image file}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     stat: sys_err_t);            {completion status code}

img_next_write_proc_t = ^procedure (   {open next image for write in same image file}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      x_size, y_size: sys_int_machine_t; {size of image in pixels}
  in      parms: univ string_var_arg_t; {additional format commands}
  out     stat: sys_err_t);            {completion status code}
  val_param;

img_close_proc_t = ^procedure (        {close this connection}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
{
*   End of routine templates.
}
  img_driver_t = record                {data about one image file driver}
    name: string_leafname_t;           {driver name, lower case}
    open_read_proc: img_open_read_img_proc_t; {procedure to open for read}
    open_write_proc: img_open_write_img_proc_t; {procedure to open for write}
    end;

  img_filter_t = record                {data about one conversion filter}
    name: string_leafname_t;           {filter driver name, lower case}
    open_read_proc: img_open_read_filt_proc_t; {procedure to open for read}
    end;

  img_conn2_t = record                 {private IMG library data about a connection}
    mem_p: util_mem_context_p_t;       {memory context this conn, deleted on CLOSE}
    data_p: univ_ptr;                  {pointer to private data for driver/filter}
    read_scan1: img_read_scan1_proc_t; {procedure to read scan of fmt 1 pixels}
    read_scan2: img_read_scan2_proc_t; {procedure to read scan of fmt 2 pixels}
    rewind: img_rewind_proc_t;         {procedure to rewind back to first pixel}
    write_scan1: img_write_scan1_proc_t; {procedure to write scan of fmt 1 pixels}
    write_scan2: img_write_scan2_proc_t; {procedure to write scan of fmt 2 pixels}
    next_read: img_next_read_proc_t;   {procedure to open next image for read}
    next_write: img_next_write_proc_t; {procedure to open next image for write}
    close: img_close_proc_t;           {procedure to close connection}
    scan1_p: img_scan1_arg_p_t;        {pointer to fmt 1 scan buffer, may be NIL}
    scan2_p: img_scan2_arg_p_t;        {pointer to fmt 2 scan buffer, may be NIL}
    img_p: img_conn_p_t;               {pointer to public connection data}
    end;
  img_conn2_p_t = ^img_conn2_t;

  img_parm_k_t = (                     {each different standard write open parm}
    img_parm_alpha_k,                  {-ALPHA}
    img_parm_red_k,                    {-RED}
    img_parm_grn_k,                    {-GREEN}
    img_parm_blu_k,                    {-BLUE}
    img_parm_anim_k,                   {-ANIM}
    img_parm_loop_k,                   {-LOOP}
    img_parm_rate_k,                   {-RATE}
    img_parm_gray_k,                   {-GRAY}
    img_parm_qual_k);                  {-QUAL}
  img_parm_t = set of img_parm_k_t;    {set of flags for each parameter}

  img_parms_t = record                 {descriptor for "standard" write open parms}
    expl: img_parm_t;                  {flgs for explicitly set, not default, fields}
    bits_alpha: sys_int_machine_t;     {bits per pixel for alpha values}
    bits_red: sys_int_machine_t;       {bits per pixel for each color component}
    bits_grn: sys_int_machine_t;
    bits_blu: sys_int_machine_t;
    qual: real;                        {1.0 = least loss, 0.0 = most compression}
    rate: real;                        {animation rate in frames per second}
    anim: boolean;                     {image file intended to contain animation seq}
    loop: boolean;                     {animation is infinite loop}
    gray: boolean;                     {image is gray scale (R=G=B each pixel)}
    rem: string_var8192_t;             {remaining parms that were not recognized}
    end;
{
*   Standard driver and filter entry points.  These come standard with the IMG
*   library, and must be listed here because they are referenced in the
*   initialization data of the common block below.
}
procedure img_d_ps_open_write (        {open PostScript file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_img_open_read (        {open IMG file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_img_open_write (       {open IMG file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_tga_open_read (        {open TGA file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_tga_open_write (       {open TGA file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_tif_open_read (        {open TIFF file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_tif_open_write (       {open TIFF file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_gif_open_read (        {open GIF file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_gif_open_write (       {open GIF file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_jpg_open_read (        {open JPG file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_jpg_open_write (       {open JPG file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_bmp_open_write (       {open BMP file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_f_warp_open_read (       {open WARP filter for read}
  in      img_old: img_conn_t;         {image stream filter will use as input}
  in      parms: univ string_var_arg_t; {parameter string passed to filter}
  out     img: img_conn_t;             {connection to output of filter}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_inf_open_read (        {open INF file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}
  extern;
{
*
*   Private IMG library entry point definitions.
}
procedure img_alloc_scan1 (            {allocate the format 1 scan line buffer}
  in out  conn2: img_conn2_t);         {internal image I/O connection}
  val_param; extern;

procedure img_alloc_scan2 (            {allocate the format 2 scan line buffer}
  in out  conn2: img_conn2_t);         {internal image I/O connection}
  val_param; extern;

procedure img_find_driver (            {find driver associated with image file name}
  in      fnam: univ string_var_arg_t; {file name with driver name extension}
  out     dr: sys_int_machine_t);      {driver number, = 0 if no match}
  extern;

procedure img_init_conn (              {init user conn, allocates private block}
  out     img: img_conn_t);            {properly initialized connection handle}
  extern;

procedure img_parms_skip (             {skip over PARMS parameters at curr position}
  in      pstr: univ string_var_arg_t; {driver PARMS string}
  in out  p: string_index_t);          {skip all parameters starting here, updated}
  val_param; extern;

procedure img_parms_read (             {read and process standard write open parms}
  in      pstr: univ string_var_arg_t; {write parameters string}
  out     parms: img_parms_t;          {returned interpreted parameter info}
  out     stat: sys_err_t);            {completion status code}
  val_param; extern;

procedure img_read_scan1_scan2 (       {read fmt 1 scan by converting from fmt 2}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_read_scan2_scan1 (       {read fmt 2 scan by converting from fmt 1}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_write_scan1_scan2 (      {write fmt 1 scan by converting to fmt 2}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_write_scan2_scan1 (      {write fmt 2 scan by converting to fmt 1}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan2_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;
{
*   Now declare system-dependent part of internal data structures and
*   routines.
}
%include 'img_sys.ins.pas';
