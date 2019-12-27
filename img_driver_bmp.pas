{   IMG library for handling BMP image files.  These are Microsoft
*   bitmap files.
*
*   This version only implements writing BMP files.
}
module img_driver_bmp;
define img_d_bmp_open_write;
%include 'img2.ins.pas';

type
  dibcompress_k_t = integer32 (        {DIB compression strategy IDs}
    dibcompress_rgb_k = 0,             {uncompressed}
    dibcompress_rle8_k = 1,            {8 bit RLE, bottom-up only}
    dibcompress_rle4_k = 2,            {4 bit RLE, bottom-up only}
    dibcompress_bitfields_k = 3);      {masks for each comp, 16 and 32 bpp only}
{
*   The Windows bitmap file header info contains 32 bit fields aligned on
*   16 bit boundaries.  To get around this, we declare all the real field
*   names with 16 bit size, then add additional 16 bit padding fields
*   named Pn as required.  Yes, this is a kludge because the translator
*   doesn't currently propagate alignment properly.  Note that the multi-byte
*   fields in this structure always have backwards byte ordering.
}
  bmp_t = record                       {BMP file fixed fields layout}
    {
    *   This is the Windows BITMAPFILEHADER structure.
    }
    sig1, sig2: int8u_t;               {signature bytes, must be "B", "M"}
    filesize: int16u_t;                {size of whole file in bytes}
      p1: int16u_t;
    reserved1: int16u_t;               {set to 0}
    reserved2: int16u_t;               {set to 0}
    offset_pix: int16u_t;              {byte offset from rec start to pixel data}
      p2: int16u_t;
    {
    *   This is the Windows BITMAPINFOHEADER structure.
    }
    size: int16u_t;                    {number of bytes required for whole structure}
      p3: int16u_t;
    width: int16u_t;                   {width of bitmap in pixels}
      p4: int16u_t;
    height: int16u_t;                  {positive for bottom-up, neg for top-down}
      p5: int16u_t;
    planes: int16u_t;                  {number of planes in device, must be 1}
    bits_pix: int16u_t;                {bits per pixel, 1, 4, 8, 16, 24, or 32}
    compress: int16u_t;                {compression strategy, only bott-up compress}
      p6: int16u_t;
    size_img: int16u_t;                {image byte size, may =0 for DIBCOMPRESS_RGB_K}
      p7: int16u_t;
    ppm_x: int16u_t;                   {pixels per meter horizontally}
      p8: int16u_t;
    ppm_y: int16u_t;                   {pixels per meter vertically}
      p9: int16u_t;
    clr_used: int16u_t;                {size of color table actually used, 0 = max}
      p10: int16u_t;
    clr_important: int16u_t;           {num of important color, 0 = all important}
      p11: int16u_t;
    end;
  bmp_p_t = ^bmp_t;

  data_write_t = record                {complete state for writing to a BMP file}
    conn: file_conn_t;                 {handle to mapped BMP file}
    size_x, size_y: sys_int_machine_t; {image size in pixels}
    bmp_p: bmp_p_t;                    {pointer to start of BMP file mapped to mem}
    size: sys_int_adr_t;               {exact size of whole BMP file}
    adr_pix: sys_int_adr_t;            {mapped address of pixel data start}
    adr_scan0: sys_int_adr_t;          {mapped address of scan line 0 start}
    stride_scan: sys_int_adr_t;        {file offset from one scan line to the next}
    end;
  data_write_p_t = ^data_write_t;

  pixcomp_t = int8u_t;                 {format of one pixel component}
  pixcomp_p_t = ^pixcomp_t;

procedure w_scan1 (                    {write next scan line from format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  internal; forward;

procedure w_close (                    {close this connection}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  internal; forward;
{
****************************************************
*
*   Local subroutine STORE16 (VAL, FIELD)
*
*   Store the low 16 bits of VAL into the 16 bits starting at FIELD in
*   backwards byte order.
}
procedure store16 (                    {store into 16 bit Windows field}
  in      val: sys_int_conv16_t;       {value to store, only low 16 bits used}
  out     field: univ array[0..1] of int8u_t); {arbitrary field to store value into}
  val_param; internal;

begin
  field[0] := val & 16#FF;
  field[1] := rshft(val, 8) & 16#FF;
  end;
{
****************************************************
*
*   Local subroutine STORE32 (VAL, FIELD)
*
*   Store the low 32 bits of VAL into the 32 bits starting at FIELD in
*   backwards byte order.
}
procedure store32 (                    {store into 32 bit Windows field}
  in      val: sys_int_conv32_t;       {value to store, only low 32 bits used}
  out     field: univ array[0..3] of int8u_t); {arbitrary field to store value into}
  val_param; internal;

begin
  field[0] := val & 16#FF;
  field[1] := rshft(val, 8) & 16#FF;
  field[2] := rshft(val, 16) & 16#FF;
  field[3] := rshft(val, 24) & 16#FF;
  end;
{
****************************************************
*
*   Subroutine IMG_D_BMP_OPEN_WRITE (IMG, PARMS, STAT)
*
*   Open a BMP file for write.  The entire IMG data structure has already been
*   filled in, and the internal IMG library connection block allocated and
*   partially filled in.  PARMS is a string of optional parameters that the
*   application may pass.  It may contain a list of keywords followed by
*   parameters.  The keywords unique to this driver are:
*
*     -- none --
}
procedure img_d_bmp_open_write (       {open BMP file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {pointer to IMG library internal conn data}
  d_p: data_write_p_t;                 {pointer to GIF driver private data}
  offset_pix: sys_int_adr_t;           {file offset for start of pixel data}
  map_h: file_map_handle_t;            {handle to mapped file window}
  len: sys_int_adr_t;                  {actual length of mapped region}

label
  abort1;

begin
  di_p := img.data_p;                  {get pointer to internal IMG library data}
  util_mem_grab (                      {allocate private state for this connection}
    sizeof(d_p^),                      {amount of memory to allocate}
    di_p^.mem_p^,                      {parent memory context}
    false,                             {we won't individually deallocate this}
    d_p);                              {returned pointer to new memory}

  di_p^.data_p := d_p;                 {save pointer to our private data block}
  di_p^.write_scan1 := addr(w_scan1);  {set pointer to our write scan line routine}
  di_p^.close := addr(w_close);        {set pointer to our close routine}
{
*   Open the image file.
}
  file_open_map (                      {open image file for mem mapped write access}
    img.tnam,                          {file name}
    '.bmp',                            {mandatory file name suffix}
    [file_rw_read_k, file_rw_write_k], {read/write access required}
    d_p^.conn,                         {returned connection to output file}
    stat);
  if sys_error(stat) then return;

  file_map_truncate (d_p^.conn, 0);    {delete everything in the file}
  file_close (d_p^.conn);              {close file}

  file_open_map (                      {re-open with no existing data in file}
    img.tnam,                          {file name}
    '.bmp',                            {mandatory file name suffix}
    [file_rw_read_k, file_rw_write_k], {read/write access required}
    d_p^.conn,                         {returned connection to output file}
    stat);
  if sys_error(stat) then return;
{
*   Calculate the BMP file size, and map the whole file.
}
  d_p^.size_x := img.x_size;           {save image file size in pixels}
  d_p^.size_y := img.y_size;
  offset_pix := size_min(bmp_t);       {file offset for start of pixel data}
  offset_pix := (offset_pix + 3) & ~3; {round up to next multiple of 4}
  d_p^.stride_scan := img.x_size * 3;  {raw size of data for each scan line}
  d_p^.stride_scan := (d_p^.stride_scan + 3) & ~3; {round up to next multiple of 4}
  d_p^.size :=                         {complete BMP file size}
    offset_pix + d_p^.stride_scan * img.y_size;

  file_map (                           {map the whole file into memory}
    d_p^.conn,                         {connection to file}
    0,                                 {offset to start of mapped region}
    d_p^.size,                         {size of mapped region}
    [file_rw_read_k, file_rw_write_k], {read/write access required}
    d_p^.bmp_p,                        {returned pointer to start of mapped region}
    len,                               {returned actual length of mapped region}
    map_h,                             {returned handle to mapped region}
    stat);
  if sys_error(stat) then goto abort1;

  d_p^.adr_pix :=                      {save address of start of pixel data}
    sys_int_adr_t(d_p^.bmp_p) + offset_pix;
  d_p^.adr_scan0 :=                    {save address of start of scan line 0}
    d_p^.adr_pix + (img.y_size - 1) * d_p^.stride_scan; {scan 0 is last in memory}
{
*   Fill in the BMP file header info.
}
  with d_p^.bmp_p^: h do begin         {H is BMP file header}
    h.sig1 := int8u_t('B');            {mandatory signature}
    h.sig2 := int8u_t('M');
    store32 (d_p^.size, h.filesize);   {file size in bytes}
    store16 (0, h.reserved1);
    store16 (0, h.reserved2);
    store32 (offset_pix, h.offset_pix); {file offset to start of pixel data}

    store32 (                          {size of BITMAPINFOHEADER structure}
      size_min(h) - offset(bmp_t.size), h.size);
    store32 (img.x_size, h.width);     {horizontal image size in pixels}
    store32 (img.y_size, h.height);    {vertical image size in pixels}
    store16 (1, h.planes);             {must be 1}
    store16 (24, h.bits_pix);          {bits per pixel}
    store32 (ord(dibcompress_rgb_k), h.compress); {no compression}
    store32 (d_p^.stride_scan * img.y_size, h.size_img); {pixel data size in bytes}
    store32 (0, h.ppm_x);              {pixels per meter}
    store32 (0, h.ppm_y);
    store32 (0, h.clr_used);           {no color table present}
    store32 (0, h.clr_important);
    end;
{
*   Fix up valued in IMG.
}
  img.bits_alpha := 0;                 {indicate pixel components precision}
  img.bits_red := 8;
  img.bits_grn := 8;
  img.bits_blu := 8;
  img.bits_max := 8;
  string_copy (d_p^.conn.tnam, img.tnam); {update exact file name opened}

  return;                              {normal return point}
{
*   Error abort points.  STAT is already set.
}
abort1:                                {output file open}
  file_close (d_p^.conn);
  end;
{
****************************************************
*
*   Subroutine W_SCAN1 (IMG, SCAN, STAT)
*
*   Write the scan line SCAN to the image file.
}
procedure w_scan1 (                    {write next scan line from format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  internal;

var
  di_p: img_conn2_p_t;                 {pointer to IMG library internal conn data}
  d_p: data_write_p_t;                 {pointer to our private BMP writing data}
  p: pixcomp_p_t;                      {pointer to pixel component in BMP file}
  x: sys_int_machine_t;                {scan line pixel index}
  y: sys_int_machine_t;                {scan line Y coordinate}

begin
  sys_error_none (stat);               {init to no error occurred}
  di_p := img.data_p;                  {get pointer to IMG library internal data}
  d_p := di_p^.data_p;                 {get pointer to our private write state}

  y := max(0, min(d_p^.size_y - 1, img.next_y)); {make clipped Y coordinate}
  p := univ_ptr(                       {compute start address of this scan line}
    d_p^.adr_scan0 - y * d_p^.stride_scan);
  for x := 0 to d_p^.size_x-1 do begin {once for each pixel in the scan line}
    p^ := scan[x].blu;                 {copy pixel data to mem mapped BMP file}
    p := succ(p);
    p^ := scan[x].grn;
    p := succ(p);
    p^ := scan[x].red;
    p := succ(p);
    end;                               {back for next pixel in scan line}

  img.next_y := y + 1;                 {update Y coordinate of next scan line}
  end;
{
****************************************************
*
*   Subroutine W_CLOSE (IMG, STAT)
*
*   Close the image file.
}
procedure w_close (                    {close this connection}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  internal;

var
  di_p: img_conn2_p_t;                 {pointer to IMG library internal conn data}
  d_p: data_write_p_t;                 {pointer to our private BMP writing data}

begin
  di_p := img.data_p;                  {get pointer to IMG library internal data}
  d_p := di_p^.data_p;                 {get pointer to our private write state}

  file_map_truncate (d_p^.conn, d_p^.size); {make sure file length is correct}
  file_close (d_p^.conn);              {close the BMP file}
  end;
