{   I/O driver for handling .IMG image files.
}
module img_driver_img;
define img_d_img_open_read;
define img_d_img_open_write;
%include 'img2.ins.pas';

const
  in_buf_runs = 2048;                  {number of runs to read from file at a time}
  out_buf_runs = 2048;                 {number of runs to write to file at a time}
{
*   Mnemonic constants for IMG file data formats.
}
  dform_lrgb8_k = 1;                   {8 bit runlen-1, red, green, blue}
  dform_lrgba8_k = 2;                  {8 bit runlen-1, red, green, blue, alpha}

type
  run_lrgb8_t = [aligned(0)] packed record {one run of RGB 8 bit values}
    len: 0..255;                       {length of run - 1}
    red, grn, blu: 0..255;             {pixel component values}
    end;

  run_lrgb8_p_t =                      {pointer to run of 8 bit RGB values}
    ^run_lrgb8_t;

  run_lrgba8_t = [aligned(0)] packed record {one run of RGBA 8 bit values}
    len: 0..255;                       {length of run - 1}
    red, grn, blu, alpha: 0..255;      {pixel component values}
    end;

  run_lrgba8_p_t =                     {pointer to run of 8 bit RGBA values}
    ^run_lrgba8_t;

  run_ps_t = record case integer of    {pointers to the different run types}
    0: (p: univ_ptr);                  {general pointer, but can't dereference}
    1: (lrgb8_p: run_lrgb8_p_t);       {points to run of 8 bit RGB data}
    2: (lrgba8_p: run_lrgba8_p_t);     {points to run of 8 bit RGBA data}
    end;

  data_read_t = record                 {private data for reading IMG file}
    dform: sys_int_machine_t;          {IMG file data format, use DFORM_xxx_K}
    run_pix_left: sys_int_machine_t;   {number of pixels left in current run}
    buf_p: ^char;                      {points to start of input buffer}
    run_ps: run_ps_t;                  {pointers to current run data}
    runs_left: sys_int_machine_t;      {number of runs after curr left in buffer}
    rewind_pos: file_pos_t;            {IMG file position for REWIND operation}
    conn: file_conn_t;                 {handle for connection to IMG file}
    run_size: sys_int_adr_t;           {size of one minimum chunk of data}
    buf_size: sys_int_adr_t;           {amount of data to read at one time}
    pix1_val: img_pixel1_t;            {current pixel value in format 1}
    pix2_val: img_pixel2_t;            {current pixel value in format 2}
    end;

  data_read_p_t =                      {pointer to private data for IMG file read}
    ^data_read_t;

  data_write_t = record                {private data for writing to an IMG file}
    dform: sys_int_machine_t;          {IMG file data format, use DFORM_xxx_K}
    run_size: sys_int_adr_t;           {size of one minimum chunk of data}
    buf_runs: sys_int_machine_t;       {number of runs in one full buffer}
    buf_size: sys_int_adr_t;           {amount of data to write at one time}
    buf_p: sys_int_machine_p_t;        {pointer to start of output buffer}
    run_ps: run_ps_t;                  {pointer to where next run will go}
    runs_left: sys_int_machine_t;      {runs left from RUN_PS to end of buffer}
    run_pix: sys_int_machine_t;        {number of pixels in current run}
    pix1_val: img_pixel1_t;            {current pixel value in format 1}
    pix2_val: img_pixel2_t;            {current pixel value in format 2}
    conn: file_conn_t;                 {handle to connection to IMG file}
    end;

  data_write_p_t =                     {pointer to private data for IMG file write}
    ^data_write_t;
{
*   IMG file format.  The first 32 bits of an IMG file is an integer specifying
*   the number of bytes in the rest of the header, which has the format
*   described below by IMG_FILE_HEADER_T.  The first data starts immediately
*   following the header.  The data is ordered in scan lines from top to bottom
*   of the image, and from left to right accross each scan line.  This means that
*   the first piece of data is for the top left pixel, the next for the one to
*   the right of it, etc.
*
*   Multi-byte fields (like 32 bit integers) are stored high byte first.  This
*   is like the Motorola 680x0 processors, and the reverse of the Intel 80x86
*   processors.
}
  img_file_header_t = record           {.IMG file header starting after len word}
    format: integer32;                 {what type of IMG file header format}
    case integer of                    {the different header formats}
    1:(
      f1_xsize: integer32;             {number of pixels accross the image}
      f1_ysize: integer32;             {number of scan lines in image}
      f1_aspect: single;               {Xsize/Ysize of displayed image}
      f1_dform: integer32;             {data format, use constants DFORM_xxx_K}
      f1_ncom: integer32;              {max F1_COM_OFS array index}
      f1_com_ofs:                      {comment offsets from IMG_FILE_HEADER_T start}
        array[0..0] of integer32;
      );                               {end of type 1 header format}
    end;

  img_file_header_p_t =                {pointer to IMG file header after length word}
    ^img_file_header_t;
{
*   Internal driver entry point templates.  The two external routines for
*   opening an IMG file for read and write are declared in IMG2.INS.PAS, since
*   this driver comes standard with the IMG library.
}
procedure img_d_img_read_scan1 (       {read a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure img_d_img_rewind (           {rewind so next read is first image pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}
  forward;

procedure img_d_img_close_read (       {close IMG file open for read}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure img_d_img_write_scan1 (      {write a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure img_d_img_close_write (      {close IMG file open for write}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  forward;
{
******************************************************************************
*
*   Subroutine IMG_D_IMG_OPEN_READ (FNAM, IMG, STAT)
*
*   Open a .img file for read.
*
*   FNAM is the name of the .img file to open.  It may already have the .img
*   appendix.  IMG is the user connection handle.  Its DATA_P field has been
*   filled in and the internal IMG library data block allocated.  All other
*   fields in IMG and IMG.DATA_P^ have been initialized to default or
*   illegal value, and generally need to be filled in.
*
*   The REWIND and CLOSE entry points, and at least one of the READ_SCAN1 or
*   READ_SCAN2 entry points need to be filled in.
}
procedure img_d_img_open_read (        {open IMG file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: data_read_p_t;          {pointer to IMG driver private data}
  head_len: integer32;                 {bytes in header after header length word}
  head_p: img_file_header_p_t;         {pointer to IMG file header data}
  olen: sys_int_adr_t;                 {amount of data actually read}
  i: sys_int_machine_t;                {loop counter}
  ncom: sys_int_machine_t;             {number of comment lines from file used}
  head_com_p: string_var_p_t;          {pointer to comment line from IMG header}

begin
  sys_error_none (stat);               {init to no error}
  data_p := img.data_p;                {make pointer to IMG library internal data}
  img_mem_alloc (                      {allocate our own private data block}
    img, sizeof(data_read_p^), data_read_p);
  with
      data_p^: d,                      {D is IMG library conn data block}
      data_read_p^: dr                 {DR is IMG file private conn data block}
      do begin

  d.data_p := data_read_p;             {save pointer to our private data block}

  string_fnam_extend (fnam, '.img', img.tnam); {require the .img suffix}
  file_open_read_bin (                 {open IMG file for READ operations}
    img.tnam,                          {file name}
    '',                                {file name extension string}
    dr.conn,                           {file connection handle}
    stat);
  if sys_error(stat) then return;      {error trying to open IMG file ?}
{
*   The IMG file has been successfully opened.  Now read in the file header.
}
  file_read_bin (                      {read header length word}
    dr.conn,                           {file connection handle}
    sizeof(head_len),                  {amount of data to read}
    head_len,                          {returned data}
    olen,                              {amount of data actually read}
    stat);
  if sys_error(stat) then return;

  if sys_byte_order_k <> sys_byte_order_fwd_k then begin {need to flip byte order ?}
    sys_order_flip (head_len, sizeof(head_len));
    end;

  sys_mem_alloc (head_len, head_p);    {allocate memory for reading file header}
  file_read_bin (                      {read rest of header}
    dr.conn,                           {file connection handle}
    head_len,                          {amount of data to point to}
    head_p^,                           {returned img file header data}
    olen,                              {amount of data actually read}
    stat);
  if sys_error(stat) then begin
    sys_mem_dealloc (head_p);
    return;
    end;
{
*   The IMG file header has been read in, and is being pointed to by HEAD_P.
*   Flip the byter order if the CPU byte order is reverse of the file byte
*   order.
}
  if sys_byte_order_k <> sys_byte_order_fwd_k then begin {need to flip byte order ?}
    sys_order_flip (head_p^.format, sizeof(head_p^.format));
    case head_p^.format of             {what is the file format type ?}
1:    begin                            {header format 1}
        sys_order_flip (head_p^.f1_xsize, sizeof(head_p^.f1_xsize));
        sys_order_flip (head_p^.f1_ysize, sizeof(head_p^.f1_ysize));
        sys_order_flip (head_p^.f1_aspect, sizeof(head_p^.f1_aspect));
        sys_order_flip (head_p^.f1_dform, sizeof(head_p^.f1_dform));
        sys_order_flip (head_p^.f1_ncom, sizeof(head_p^.f1_ncom));
        for i := 0 to head_p^.f1_ncom do begin {once for each comment line}
          sys_order_flip (head_p^.f1_com_ofs[i], sizeof(head_p^.f1_com_ofs[i]));
          head_com_p := univ_ptr(      {make pointer to this comment line in header}
            sys_int_adr_t(head_p) + head_p^.f1_com_ofs[i]);
          sys_order_flip (head_com_p^.max, sizeof(head_com_p^.max));
          sys_order_flip (head_com_p^.len, sizeof(head_com_p^.len));
          end;                         {back for next comment line in header}
        end;                           {end of format 1 header case}
      end;                             {end of header format cases}
    end;                               {multi-byte fields now all set to right order}
{
*   Fill in IMG, the user connection handle.
}
  if head_p^.format <> 1 then begin    {unrecognized IMG file header format ?}
    sys_stat_set (img_subsys_k, img_err_img_head_fmt_k, stat);
    sys_stat_parm_int (head_p^.format, stat);
    sys_mem_dealloc (head_p);
    return;
    end;

  img.x_size := head_p^.f1_xsize;      {image size in pixels}
  img.y_size := head_p^.f1_ysize;
  img.aspect := head_p^.f1_aspect;     {width/height aspect ratio}
  img.next_y := 0;                     {coordinate of next scan line to be read}
  img.bits_alpha := 0;                 {init BITS to default, will be fixed later}
  img.bits_red := 8;
  img.bits_grn := 8;
  img.bits_blu := 8;
  img.bits_max := 8;
  string_vstring (img.file_type, 'img', 3); {image file type name (driver name)}
  string_fnam_unextend (dr.conn.gnam, '.img', img.gnam); {generic image file name}
  string_copy (dr.conn.tnam, img.tnam); {full image file tree name}
{
*   Copy comments from file into the user image stream handle (IMG).
}
  ncom := head_p^.f1_ncom;             {init max useable F1_COM_OFS array index}
  for i := head_p^.f1_ncom downto 0 do begin {loop backwards thru comment lines}
    head_com_p := univ_ptr(            {make pointer to this comment line in header}
      sys_int_adr_t(head_p) + head_p^.f1_com_ofs[i]);
    if head_com_p^.len > 0 then exit;  {found last non-empty comment line ?}
    ncom := ncom - 1;                  {one less read comment line}
    end;                               {back and check previous comment line}

  for i := 0 to ncom do begin          {once for each useable comment line}
    head_com_p := univ_ptr(            {make pointer to this comment line in header}
      sys_int_adr_t(head_p) + head_p^.f1_com_ofs[i]);
    string_list_line_add (img.comm);   {create new user-visible comment line}
    string_copy (head_com_p^, img.comm.str_p^); {copy this comment line to IMG handle}
    string_unpad (img.comm.str_p^);    {delete all trailing blanks}
    end;
{
*   IMG is mostly filled in.  Now fill in the remainder of the IMG library private
*   connection data block.  This block is at DATA_P^, and D is the abbreviation
*   for it.
}
  d.read_scan1 := addr(img_d_img_read_scan1);
  d.rewind := addr(img_d_img_rewind);
  d.close := addr(img_d_img_close_read);
{
*   Fill in our own private data block for this connection.  It is pointed to
*   by DATA_READ_P, and DR has been set up as an abbreviation for it.
}
  dr.dform := head_p^.f1_dform;        {IMG file data format ID}
  dr.run_pix_left := 0;                {number of pixels remaining in current run}
  dr.run_ps.p := nil;                  {init pointer to current run data}
  dr.runs_left := 0;                   {runs left in current buffer}
  sys_mem_dealloc (head_p);            {done with IMG file header data}
  file_pos_get (dr.conn, dr.rewind_pos); {save file position at start of data}

  case dr.dform of                     {different code for each data type}
dform_lrgb8_k: begin                   {data is 8 bit RGB run}
      dr.run_size := 4;
      end;
dform_lrgba8_k: begin                  {data is 8 bit RGBA run}
      dr.run_size := 5;
      img.bits_alpha := 8;             {alpha component does exist here}
      end;
otherwise                              {unrecognized data format ID}
    sys_stat_set (img_subsys_k, img_err_img_dform_k, stat);
    sys_stat_parm_int (dr.dform, stat);
    return;
    end;                               {end of data format cases}

  dr.buf_size := dr.run_size * in_buf_runs; {amount of data to read each buffer full}
  util_mem_grab (dr.buf_size, d.mem_p^, false, dr.buf_p); {allocate input buffer}
  dr.run_ps.p := nil;                  {indicate no current input data exists}
  end;                                 {done with D and DR abbreviations}
  end;                                 {end of subroutine IMG_D_IMG_OPEN_READ}
{
******************************************************************************
*
*   Subroutine IMG_D_READ_SCAN1 (IMG, SCAN, STAT)
*
*   Read then next scan line from an IMG file.  This scan line will be an array
*   for format 1 pixels.
}
procedure img_d_img_read_scan1 (       {read a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: data_read_p_t;          {pointer to IMG driver private data}
  x: sys_int_machine_t;                {current pixel X coordinate}
  buflen: sys_int_adr_t;               {amount of data actually read in}

begin
  sys_error_none (stat);               {init to no error}

  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_read_p := data_p^.data_p;       {make pointer to our private connection data}
  with
      data_read_p^: dr                 {DR is IMG file private conn data block}
      do begin

  for x := 0 to img.x_size-1 do begin  {once for each pixel in scan line}
    if dr.run_pix_left <= 0 then begin {no pixels left in current run ?}

      if dr.runs_left <= 0 then begin  {no more runs left in input buffer ?}
        file_read_bin (                {read next buffer full from IMG file}
          dr.conn,                     {connection handle to IMG file}
          dr.buf_size,                 {amount of data to read in}
          dr.buf_p^,                   {input buffer}
          buflen,                      {amount of data actually read}
          stat);
        discard(file_eof_partial(stat)); {EOF after reading partial buffer is OK}
        if sys_error(stat) then return; {a real error ?}
        dr.run_ps.p := dr.buf_p;       {reset pointer to first run in buffer}
        dr.runs_left := buflen div dr.run_size; {set number of runs in new buffer}
        end;                           {DR.RUN_PS now points to a new run}

      case dr.dform of                 {different IMG file data format cases}
dform_lrgb8_k: begin                   {data format is 8 bit RGB run}
          dr.run_pix_left := dr.run_ps.lrgb8_p^.len + 1;
          dr.pix1_val.alpha := 255;
          dr.pix1_val.red := dr.run_ps.lrgb8_p^.red;
          dr.pix1_val.grn := dr.run_ps.lrgb8_p^.grn;
          dr.pix1_val.blu := dr.run_ps.lrgb8_p^.blu;
          end;
dform_lrgba8_k: begin                  {data format is 8 bit RGBA run}
          dr.run_pix_left := dr.run_ps.lrgba8_p^.len + 1;
          dr.pix1_val.alpha := dr.run_ps.lrgba8_p^.alpha;
          dr.pix1_val.red := dr.run_ps.lrgba8_p^.red;
          dr.pix1_val.grn := dr.run_ps.lrgba8_p^.grn;
          dr.pix1_val.blu := dr.run_ps.lrgba8_p^.blu;
          end;
otherwise
        sys_stat_set (img_subsys_k, img_err_img_dform_k, stat);
        sys_stat_parm_int (dr.dform, stat);
        return;
        end;                           {done with IMG file data format cases}

      dr.run_ps.p := univ_ptr(         {update to point to next run in buffer}
        sys_int_adr_t(dr.run_ps.p) + dr.run_size);
      dr.runs_left := dr.runs_left - 1; {one less run left in current buffer}
      end;                             {there is now at least one pixel in curr run}

    scan[x] := dr.pix1_val;
    dr.run_pix_left := dr.run_pix_left - 1; {one less pixel left in this run}
    end;                               {back to get next pixel in scan line}

  img.next_y := img.next_y + 1;        {make Y coordinate of next scan line}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_IMG_REWIND (IMG, STAT)
*
*   Rewind the image file so that the top scan line is returned on the next read.
}
procedure img_d_img_rewind (           {rewind so next read is first image pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: data_read_p_t;          {pointer to IMG driver private data}

begin
  sys_error_none (stat);               {init to no error}
  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_read_p := data_p^.data_p;       {make pointer to our private connection data}
  with
      data_read_p^: dr                 {DR is IMG file private conn data block}
      do begin
    file_pos_set (dr.rewind_pos, stat);
    img.next_y := 0;                   {indicate which scan line will be read next}
    dr.run_pix_left := 0;              {reset to force read file on next READ_SCAN}
    dr.runs_left := 0;
    end;                               {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMD_D_IMG_CLOSE_READ (IMG, STAT)
*
*   Close this connection to an IMG file.  The connection must be open for read.
}
procedure img_d_img_close_read (       {close IMG file open for read}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: data_read_p_t;          {pointer to IMG driver private data}

begin
  sys_error_none (stat);               {init to no error}
  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_read_p := data_p^.data_p;       {make pointer to our private connection data}
  with
      data_read_p^: dr                 {DR is IMG file private conn data block}
      do begin
    file_close (dr.conn);              {close our connection to the IMG file}
    end;                               {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_IMG_OPEN_WRITE (IMG, PARMS, STAT)
*
*   Open an IMG file for write.  The entire IMG data structure has already been
*   filled in, and the internal IMG library connection block allocated and
*   partially filled in.  PARMS is a string of optional parameters that the
*   application may pass.  It may contain a list of keywords followed by
*   parameters.  The keywords and parameters are:
*
*   ALPHA <bits precision>
*
*     Set the minimum desired bits of precision for ALPHA.  The driver
*     should try to accomodate this, but does not have to.  The bits of precision
*     actually used will be set in IMG.  The default is 0.
*
*   RED <bits precision>
*
*     Set the minimum desired bits of precision for RED.  The driver
*     should try to accomodate this, but does not have to.  The bits of precision
*     actually used will be set in IMG.  The default is 8.
*
*   GREEN <bits precision>
*
*     Set the minimum desired bits of precision for GRN.  The driver
*     should try to accomodate this, but does not have to.  The bits of precision
*     actually used will be set in IMG.  The default is 8.
*
*   BLUE <bits precision>
*
*     Set the minimum desired bits of precision for BLU.  The driver
*     should try to accomodate this, but does not have to.  The bits of precision
*     actually used will be set in IMG.  The default is 8.
}
procedure img_d_img_open_write (       {open IMG file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_write_p: data_write_p_t;        {pointer to IMG driver private data}
  hdlen: integer32;                    {IMG file header length word}
  com: string_var132_t;                {one comment line buffer}
  head_p: img_file_header_p_t;         {pointer to IMG file header block}
  head_size: sys_int_adr_t;            {size of IMG header except comment lines}
  com_ofs: sys_int_adr_t;              {offset from header start of current comment}
  i: sys_int_machine_t;                {loop counter}
  p: string_index_t;                   {PARMS parse index}
  opt: string_var32_t;                 {PARMS option name}
  parm: string_var32_t;                {parameter for option name in OPT}
  pick: sys_int_machine_t;             {number of token picked from list}

label
  next_opt, bad_parm, done_opts;

begin
  com.max := sizeof(com.str);          {init var strings}
  opt.max := sizeof(opt.str);
  parm.max := sizeof(parm.str);
  sys_error_none (stat);               {init to no error}

  data_p := img.data_p;                {make pointer to IMG library internal data}
  img_mem_alloc (                      {allocate our own private data block}
    img, sizeof(data_write_p^), data_write_p);
  with
      data_p^: d,                      {D is IMG library conn data block}
      data_write_p^: dr                {DR is IMG file private conn data block}
      do begin

  d.data_p := data_write_p;            {save pointer to our private data block}

  head_size :=                         {size of header except for comment lines}
    sizeof(img_file_header_t) +        {static part of header}
    ((img.comm.n - 1) * sizeof(integer32)); {comments index array}
  util_mem_grab (head_size, d.mem_p^, true, head_p); {allocate IMG file header memory}
  hdlen := head_size +                 {find total size of IMG file header}
    (img.comm.n * sizeof(com));        {actual comment lines}
{
*   HEAD_P points to the IMG file header after the length word, up to the end of the
*   comment lines index array.  Now fill in the header.
}
  head_p^.format := 1;                 {header format ID}
  head_p^.f1_xsize := img.x_size;      {size of image in pixels}
  head_p^.f1_ysize := img.y_size;
  head_p^.f1_aspect := img.aspect;     {width/height aspect ratio}
  head_p^.f1_dform := dform_lrgb8_k;   {default to 8 bit runlen RGB format}
  head_p^.f1_ncom := img.comm.n - 1;   {max F1_COM_OFS array index}

  com_ofs := head_size;                {init offset for first comment line}
  for i := 0 to img.comm.n-1 do begin  {once for each comment line}
    head_p^.f1_com_ofs[i] := com_ofs;  {set offset for this comment start}
    com_ofs := com_ofs + sizeof(com);  {make offset for next comment line start}
    end;
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
  string_t_int (parm, img.bits_alpha, stat);
  if sys_error(stat) then goto bad_parm;
  end;
{
*   RED <bit precision>
}
2: begin
  string_token (parms, p, parm, stat);
  if sys_error(stat) then goto bad_parm;
  string_t_int (parm, img.bits_red, stat);
  if sys_error(stat) then goto bad_parm;
  end;
{
*   GREEN <bit precision>
}
3: begin
  string_token (parms, p, parm, stat);
  if sys_error(stat) then goto bad_parm;
  string_t_int (parm, img.bits_grn, stat);
  if sys_error(stat) then goto bad_parm;
  end;
{
*   BLUE <bit precision>
}
4: begin
  string_token (parms, p, parm, stat);
  if sys_error(stat) then goto bad_parm;
  string_t_int (parm, img.bits_blu, stat);
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
*   The BITS fields have been filled in from the either the default values
*   set in IMG_INIT_CONN, or directly from the user requested values from the
*   PARMS string.  Now use this information to decide on the IMG file format,
*   and set the BITS fields to the actual values used.
}
  if img.bits_alpha > 0
    then begin                         {we WILL store alpha values}
      img.bits_alpha := 8;
      img.bits_red := 8;
      img.bits_grn := 8;
      img.bits_blu := 8;
      img.bits_max := 8;
      dr.dform := dform_lrgba8_k;
      end
    else begin                         {we will NOT store alpha values}
      img.bits_alpha := 0;
      img.bits_red := 8;
      img.bits_grn := 8;
      img.bits_blu := 8;
      img.bits_max := 8;
      dr.dform := dform_lrgb8_k;
      end
    ;
  head_p^.f1_dform := dr.dform;        {set data format in IMG file header}
{
*   The IMG file header length word and the header except for the comment lines
*   is all filled in.  Now open the IMG file and write the IMG file header.
}
  if sys_byte_order_k <> sys_byte_order_fwd_k then begin {need to flip byte order ?}
    sys_order_flip (hdlen, sizeof(hdlen));
    sys_order_flip (head_p^.format, sizeof(head_p^.format));
    sys_order_flip (head_p^.f1_xsize, sizeof(head_p^.f1_xsize));
    sys_order_flip (head_p^.f1_ysize, sizeof(head_p^.f1_ysize));
    sys_order_flip (head_p^.f1_aspect, sizeof(head_p^.f1_aspect));
    sys_order_flip (head_p^.f1_dform, sizeof(head_p^.f1_dform));
    for i := 0 to head_p^.f1_ncom do begin {once for each comment line}
      sys_order_flip (head_p^.f1_com_ofs[i], sizeof(head_p^.f1_com_ofs[i]));
      end;
    sys_order_flip (head_p^.f1_ncom, sizeof(head_p^.f1_ncom));
    end;

  file_open_write_bin (                {open IMG file for write}
    img.tnam,                          {complete image file name}
    '',                                {file name appendix}
    dr.conn,                           {connection handle to IMG file}
    stat);
  if sys_error(stat) then return;      {error opening IMG file for write ?}

  file_write_bin (                     {write IMG file header length word}
    hdlen,                             {data to write}
    dr.conn,                           {connection handle to IMG file}
    sizeof(hdlen),                     {size of data to write}
    stat);
  if sys_error(stat) then return;

  file_write_bin (                     {write IMG file header up to comment lines}
    head_p^,                           {data to write}
    dr.conn,                           {connection handle to IMG file}
    head_size,                         {amount of data to write}
    stat);
  if sys_error(stat) then return;
  util_mem_ungrab (head_p, d.mem_p^);  {deallocate IMG file header memory}

  string_list_pos_abs (img.comm, 1);   {position to first comment line}
  for i := 1 to img.comm.n do begin    {once for each comment line}
    com.max := sizeof(com.str);        {reset max string length field}
    string_copy (img.comm.str_p^, com); {make local copy of this comment line}
    string_fill (com);                 {fill unused space with blanks}
    if sys_byte_order_k <> sys_byte_order_fwd_k then begin {need to flip byte order ?}
      sys_order_flip (com.max, sizeof(com.max));
      sys_order_flip (com.len, sizeof(com.len));
      end;
    file_write_bin (com, dr.conn, sizeof(com), stat); {write this comment line to file}
    if sys_error(stat) then return;    {error writing comment line ?}
    string_list_pos_rel (img.comm, 1); {advance to next source comment line}
    end;                               {back and process next comment line}
{
*   The IMG file header has been completely written, including the comment lines.
*   Now fill in the remainder of the internal IMG library data structure for
*   this image file connection.
}
  d.write_scan1 := addr(img_d_img_write_scan1);
  d.close := addr(img_d_img_close_write);
{
*   Fill in the remainder of our own private data structure for this connection.
}
  case dr.dform of
dform_lrgb8_k: begin                   {data format is 8 bit RGB runs}
      dr.run_size := 4;
      end;
dform_lrgba8_k: begin                  {data format is 8 bit RGBA runs}
      dr.run_size := 5;
      end;
    end;                               {done with IMG file data format cases}

  dr.buf_runs := out_buf_runs;         {number of runs to fit in output buffer}
  dr.buf_size := dr.buf_runs * dr.run_size; {size of output buffer}
  img_mem_alloc (img, dr.buf_size, dr.buf_p); {allocate the output buffer}
  dr.run_ps.p := dr.buf_p;             {init where next run will go in output buffer}
  dr.runs_left := dr.buf_runs;         {the whole buffer is empty}
  dr.run_pix := 0;                     {init to no pixels in current run}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Procedure IMG_D_IMG_WRITE_SCAN1 (IMG, SCAN, STAT)
*
*   Write the next scan line to an IMG file.  The scan line is an array of
*   format 1 pixels.
}
procedure img_d_img_write_scan1 (      {write a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_write_p: data_write_p_t;        {pointer to IMG driver private data}
  x: sys_int_machine_t;                {pixel X coordinate accross scan line}
  pix: img_pixel1_t;                   {local "sanitized" copy of current pixel}
  run_len: sys_int_machine_t;          {number of pixels in run written to buffer}

label
  retest_run;

begin
  sys_error_none (stat);               {init to no error}

  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_write_p := data_p^.data_p;      {make pointer to our private data block}
  with
      data_write_p^: dr                {DR is IMG file private conn data block}
      do begin

  for x := 0 to img.x_size-1 do begin  {once for each pixel in scan line}

    pix := scan[x];                    {make local copy of this pixel}
    case dr.dform of                   {sanitize local pixel if necessary}
dform_lrgb8_k: begin                   {data format is 8 bit RGB runs}
        pix.alpha := 255;
        end;
      end;                             {PIX contains sanitized pixel value}

    if pix.all <> dr.pix1_val.all then begin {this pixel starts a new run ?}

retest_run:                            {jump back here after writing partial run}
      if dr.run_pix > 0 then begin     {there is a previous run to write out ?}
        run_len := min(dr.run_pix, 256); {clip to max size run allowed in file}
        case dr.dform of               {all the different IMG file data formats}
dform_lrgb8_k: begin                   {data format is 8 bit RGB runs}
            dr.run_ps.lrgb8_p^.len := run_len - 1;
            dr.run_ps.lrgb8_p^.red := dr.pix1_val.red;
            dr.run_ps.lrgb8_p^.grn := dr.pix1_val.grn;
            dr.run_ps.lrgb8_p^.blu := dr.pix1_val.blu;
            end;
dform_lrgba8_k: begin                  {data format is 8 bit RGBA runs}
            dr.run_ps.lrgba8_p^.len := run_len - 1;
            dr.run_ps.lrgba8_p^.red := dr.pix1_val.red;
            dr.run_ps.lrgba8_p^.grn := dr.pix1_val.grn;
            dr.run_ps.lrgba8_p^.blu := dr.pix1_val.blu;
            dr.run_ps.lrgba8_p^.alpha := dr.pix1_val.alpha;
            end;
          end;                         {done writing run value into buffer}
        dr.run_ps.p := univ_ptr(       {point to where new run will be written}
          sys_int_adr_t(dr.run_ps.p) + dr.run_size);
        dr.runs_left := dr.runs_left - 1; {one less available run in buffer}
        if dr.runs_left <= 0 then begin {just filled up this buffer ?}
          file_write_bin (dr.buf_p^, dr.conn, dr.buf_size, stat); {write buffer to file}
          if sys_error(stat) then return;
          dr.run_ps.p := dr.buf_p;     {next run gets put at start of buffer}
          dr.runs_left := dr.buf_runs; {reset number of runs left in buffer}
          end;                         {done handling full buffer}
        dr.run_pix := dr.run_pix - run_len; {subtract out pixels actually written}
        goto retest_run;               {check for more pixels left in run}
        end;                           {done handling writing of previous run}

      dr.pix1_val := pix;              {set pixel value of this new run}
      dr.run_pix := 0;                 {init new run to empty}
      end;                             {done handling new pixel different from last}
    dr.run_pix := dr.run_pix + 1;      {one more pixel in this run}
    end;                               {back and process next pixel from SCAN}
  img.next_y := img.next_y + 1;        {update Y coordinate of next scan line}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_IMG_CLOSE_WRITE (IMG, STAT)
*
*   Close this connetion to an IMG file.  The IMG file is assumed to be open
*   for write.
}
procedure img_d_img_close_write (      {close IMG file open for write}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_write_p: data_write_p_t;        {pointer to IMG driver private data}
  run_len: sys_int_machine_t;          {number of pixels in run written to buffer}
  buf_len: sys_int_adr_t;              {size of buffer left to write to file}

label
  retest_run;

begin
  sys_error_none (stat);               {init to no error}

  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_write_p := data_p^.data_p;      {make pointer to our private data block}
  with
      data_write_p^: dr                {DR is IMG file private conn data block}
      do begin
{
*   Make sure the last run is written to the buffer.
}
retest_run:                            {jump back here after writing partial run}
  if dr.run_pix > 0 then begin         {there is a previous run to write out ?}
    run_len := min(dr.run_pix, 256);   {clip to max size run allowed in file}
    case dr.dform of                   {all the different IMG file data formats}
dform_lrgb8_k: begin                   {data format is 8 bit RGB runs}
        dr.run_ps.lrgb8_p^.len := run_len - 1;
        dr.run_ps.lrgb8_p^.red := dr.pix1_val.red;
        dr.run_ps.lrgb8_p^.grn := dr.pix1_val.grn;
        dr.run_ps.lrgb8_p^.blu := dr.pix1_val.blu;
        end;
dform_lrgba8_k: begin                  {data format is 8 bit RGBA runs}
        dr.run_ps.lrgba8_p^.len := run_len - 1;
        dr.run_ps.lrgba8_p^.red := dr.pix1_val.red;
        dr.run_ps.lrgba8_p^.grn := dr.pix1_val.grn;
        dr.run_ps.lrgba8_p^.blu := dr.pix1_val.blu;
        dr.run_ps.lrgba8_p^.alpha := dr.pix1_val.alpha;
        end;
      end;                             {done writing run value into buffer}
    dr.run_ps.p := univ_ptr(           {point to where new run will be written}
      sys_int_adr_t(dr.run_ps.p) + dr.run_size);
    dr.runs_left := dr.runs_left - 1;  {one less available run in buffer}
    if dr.runs_left <= 0 then begin    {just filled up this buffer ?}
      file_write_bin (dr.buf_p^, dr.conn, dr.buf_size, stat); {write buffer to file}
      if sys_error(stat) then return;
      dr.run_ps.p := dr.buf_p;         {next run gets put at start of buffer}
      dr.runs_left := dr.buf_runs;     {reset number of runs left in buffer}
      end;                             {done handling full buffer}
    dr.run_pix := dr.run_pix - run_len; {subtract out pixels actually written}
    goto retest_run;                   {check for more pixels left in run}
    end;                               {done handling writing of previous run}
{
*   Make sure the current buffer is written to the IMG file.
}
  buf_len :=                           {find size of unwritten data in buffer}
    sys_int_adr_t(dr.run_ps.p) - sys_int_adr_t(dr.buf_p);
  if buf_len > 0 then begin            {unwritten data left in buffer ?}
    file_write_bin (dr.buf_p^, dr.conn, buf_len, stat); {write partial buffer to file}
    if sys_error(stat) then return;
    end;                               {done handling full buffer}

  file_close (dr.conn);                {close IMG output file}
  end;                                 {done with D and DR abbreviations}
  end;
