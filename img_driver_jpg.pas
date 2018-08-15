{   IMG library driver for handling JPEG image files.  These file names
*   end in ".jpg".  This driver is a thin layer on top of the IJG
*   (International JPEG Group) JPEG library.  This library is distributed
*   with source code and may be used in applications free of charge.
}
module img_driver_jpg;
define img_d_jpg_open_read;
define img_d_jpg_open_write;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';
%include '/cognivision_links/dsee_libs/img/jpeg_img.ins.pas';

const
  out_bufsize_k = 8192;                {max output bytes to buffer before writing}
  in_bufsize_k = 8192;                 {max input bytes to buffer}

type
  data_read_t = record                 {private data block when reading JPG file}
    j: jpeg_appin_t;                   {data required by our JPEG glue routines}
    conn: file_conn_t;                 {connection to raw input file}
    scan_p: ^int8u_t;                  {pointer to one scan line in JPEG lib format}
    stat: sys_err_t;                   {filled in by routines called from JPEG lib}
    buf: array[1..in_bufsize_k] of int8u_t; {output buffer, read by JPEG library}
    end;
  data_read_p_t = ^data_read_t;

  data_write_t = record                {private data block when writing JPG file}
    jpegout: jpeg_appout_t;            {data required by our JPEG glue routines}
    conn: file_conn_t;                 {connection to raw output file}
    pixform: jpeg_pixform_k_t;         {JPEG library pixel format}
    pixsize: sys_int_adr_t;            {storage size for one JPEG library pixel}
    scan_p: ^int8u_t;                  {pointer to one scan line in JPEG lib format}
    stat: sys_err_t;                   {filled in by routines called from JPEG lib}
    buf: array[1..out_bufsize_k] of int8u_t; {output buffer, filled by JPEG library}
    end;
  data_write_p_t = ^data_write_t;
{
*   Internal driver entry point templates.  The two external routines for
*   opening a JPG file for read and write are declared in IMG2.INS.PAS, since
*   this driver comes standard with the IMG library.
}
procedure img_d_jpg_rewind (           {rewind so next read is first image pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}
  forward;

procedure img_d_jpg_read_scan1 (       {read a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure readbuf (                    {read next chunk into input buffer}
  in out  dr: data_read_t);            {image input reading state}
  val_param; internal; forward;

procedure img_d_jpg_close_read (       {close JPG file open for read}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure writebuf (                   {write one buffer full to output file}
  in out  dr: data_write_t);           {image output writing state}
  val_param; internal; forward;

procedure img_d_jpg_write_scan1 (      {write a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure img_d_jpg_close_write (      {close JPG file open for write}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  forward;
{
******************************************************************************
*
*   Subroutine IMG_D_JPG_OPEN_READ (FNAM, IMG, STAT)
*
*   Open a .jpg file for read.
*
*   FNAM is the name of the .jpg file to open.  It may already have the .jpg
*   appendix.  IMG is the user connection handle.  Its DATA_P field has been
*   filled in and the internal IMG library data block allocated.  All other
*   fields in IMG and the IMG.DATA_P^ have been initialized to default or
*   illegal value, and generally need to be filled in.
*
*   The REWIND and CLOSE entry points, and at least one of the READ_SCAN1 or
*   READ_SCAN2 entry points need to be filled in.
}
procedure img_d_jpg_open_read (        {open JPG file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library data about this conn}
  data_read_p: data_read_p_t;          {pointer to our private data when reading}

label
  abort1;

begin
  data_p := img.data_p;                {make pointer to IMG library internal data}
  img_mem_alloc (                      {allocate our own private data block}
    img, sizeof(data_read_p^), data_read_p);
  with
      data_p^: d,                      {D is IMG library conn data block}
      data_read_p^: dr                 {DR is JPG file private conn data block}
      do begin

  d.data_p := data_read_p;             {save pointer to our private data block}

  file_open_read_bin (                 {open image file for raw binary read}
    fnam, '.jpg',                      {file name and mandatory suffix}
    dr.conn,                           {returned file connection}
    stat);
  if sys_error(stat) then return;

  dr.j.buf_p := addr(dr.buf);          {set pointer to input buffer}
  dr.j.bufsize := sizeof(dr.buf);      {set input buffer size}
  dr.j.bufn := 0;                      {indicate the input buffer is empty}
  dr.j.readbuf := univ_ptr(addr(readbuf)); {set buffer read routine}
  sys_error_none (dr.stat);            {indicate no error in called back routine}

  jpg_open_in (dr.j);                  {open JPEG decompression stream}
  if sys_error(dr.stat) then begin
    stat := dr.stat;
    goto abort1;
    end;
{
*   The JPEG decompression input stream has been established.
*
*   Allocate and initialize state now that the image parameters are known.
}
  img_mem_alloc (                      {allocate buffer for one JPEG lib scan line}
    img, dr.j.pixx * dr.j.ncomp, dr.scan_p);
{
*   Fill in the image information passed back to the application.
}
  img.x_size := dr.j.pixx;             {fill in image size}
  img.y_size := dr.j.pixy;
  img.aspect := img.x_size / img.y_size; {assume square pixels}
  img.bits_alpha := 0;
  img.bits_red := 8;
  img.bits_grn := 8;
  img.bits_blu := 8;
  img.bits_max := 8;
  string_copy (dr.conn.gnam, img.gnam); {pass back image file generic leafname}
  string_copy (dr.conn.tnam, img.tnam); {pass back full image file pathname}
{
*   Fill in the internal IMG library state for this image connection.
}
  d.read_scan1 := addr(img_d_jpg_read_scan1); {routine to read scan lines}
  d.rewind := addr(img_d_jpg_rewind);  {routine to reset to reading top scan line}
  d.close := addr(img_d_jpg_close_read); {routine to close image file connection}
  return;
{
*   Error exits.  STAT must already be set.
}
abort1:                                {JPEG lib resources allocated, file open}
  jpg_close_in (dr.j);                 {deallocate JPEG library resources}
  file_close (dr.conn);                {close the image input file}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_JPG_REWIND (IMG, STAT)
*
*   Reset so that next scan line read will be top scan line (Y = 0).
*
*   The JPEG library doesn't have a rewind feature, so we just close and
*   re-open the stream.  Nothing bad can happen to the image file during
*   that time because we keep it open.
}
procedure img_d_jpg_rewind (           {rewind so next read is first image pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: data_read_p_t;          {pointer to IMG driver private data}

label
  abort1, abort2;

begin
  sys_error_none (stat);               {init to no error}

  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_read_p := data_p^.data_p;       {make pointer to our private connection data}
  with
      data_read_p^: dr                 {DR is IMG file private conn data block}
      do begin

  jpg_close_in (dr.j);                 {close JPEG stream, deallocate resources}

  file_pos_start (dr.conn, stat);      {rewind the image input file}
  if sys_error(stat) then goto abort1;

  jpg_open_in (dr.j);                  {re-open JPEG input stream}
  if sys_error(dr.stat) then begin
    stat := dr.stat;
    goto abort2;
    end;

  img.next_y := 0;
  return;
{
*   Error exits.  STAT must already be set.
}
abort2:                                {JPEB lib state allocated, input file open}
  jpg_close_in (dr.j);                 {deallocate JPEG lib resources}

abort1:                                {input file is open}
  file_close (dr.conn);                {close image input file}

  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Local subroutine READBUF (DR)
*
*   Read the next buffer full of input data into the input buffer.
}
procedure readbuf (                    {read next chunk into input buffer}
  in out  dr: data_read_t);            {image input reading state}
  val_param; internal;

var
  i: sys_int_machine_t;                {loop counter}

begin
  file_read_bin (                      {read binary data from the input file}
    dr.conn,                           {connection to the file}
    dr.j.bufsize,                      {amount of data to read}
    dr.buf,                            {buffer to read the data into}
    dr.j.bufn,                         {amount of data actually read}
    dr.stat);
  discard( file_eof_partial(dr.stat) ); {ignore EOF part way thru buffer}
  if not sys_error(dr.stat) then return; {no error, return normally ?}
{
*   An error ocurred on the attempt to read the input file.  We can't really
*   continue, but there is no good way of communicating the error to the JPEG
*   library.  DR.STAT has already been set to the error status code.  We just
*   keep feeding the JPEG library zeros in the hope that it will eventually
*   return to the caller who will notice the DR.STAT error status.
}
  for i := 1 to in_bufsize_k do begin  {zero the whole input buffer}
    dr.buf[i] := 0;
    end;

  dr.j.bufn := dr.j.bufsize;           {pass back a full buffer}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_JPG_READ_SCAN1 (IMG, SCAN, STAT)
*
*   Return the next scan line of JPG file data.
}
procedure img_d_jpg_read_scan1 (       {read a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: data_read_p_t;          {pointer to IMG driver private data}
  x: sys_int_machine_t;                {pixel X coordinate}
  in_p: ^int8u_t;                      {pointer to next JPEB library data value}

begin
  sys_error_none (stat);               {init to no error}

  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_read_p := data_p^.data_p;       {make pointer to our private connection data}
  with
      data_read_p^: dr                 {DR is IMG file private conn data block}
      do begin

  jpg_read_scan (dr.j, dr.scan_p^);    {read next scan line into our buffer}
  if sys_error(dr.stat) then begin     {error from called back routine ?}
    stat := dr.stat;                   {pass back the error}
    jpg_close_in (dr.j);               {deallocate JPEG library resources}
    file_close (dr.conn);              {close input file}
    return;
    end;

  in_p := dr.scan_p;                   {init scan line source pointer}

  case dr.j.pixform of                 {what it the JPEG library pixel format ?}
jpeg_pixform_rgb24_k: begin            {pixel format is 24 bit RGB values}
      for x := 0 to img.x_size-1 do begin {once for each pixel in the scan line}
        scan[x].alpha := 255;
        scan[x].red := in_p^;
        in_p := succ(in_p);
        scan[x].grn := in_p^;
        in_p := succ(in_p);
        scan[x].blu := in_p^;
        in_p := succ(in_p);
        end;
      end;
jpeg_pixform_gray8_k: begin            {pixel format is 8 bit gray scale value}
      for x := 0 to img.x_size-1 do begin {once for each pixel in the scan line}
        scan[x].alpha := 255;
        scan[x].red := in_p^;
        scan[x].grn := in_p^;
        scan[x].blu := in_p^;
        in_p := succ(in_p);
        end;
      end;
    end;                               {end of pixel format cases}

  img.next_y := img.next_y + 1;        {update Y coordinate of next scan line}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_JPG_CLOSE_READ (IMG, STAT)
*
*   Close this use of JPG file for read.
}
procedure img_d_jpg_close_read (       {close JPG file open for read}
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

  jpg_close_in (dr.j);                 {deallocate JPEG library resources}
  file_close (dr.conn);                {close connection to input file}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_JPG_OPEN_WRITE (IMG, PARMS, STAT)
*
*   Open a .JPG image file for write.
}
procedure img_d_jpg_open_write (       {open JPG file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library data about this conn}
  data_write_p: data_write_p_t;        {pointer to our private data when writing}
  iparm: img_parms_t;                  {interpreted data for standard parms}

label
  abort1;

begin
  img_parms_read (parms, iparm, stat); {interpret standard PARMS}
  if sys_error(stat) then return;

  if not (img_parm_qual_k in iparm.expl) then begin {using default quality factor ?}
    iparm.qual := 1.0;                 {set quality factor to default}
    end;

  data_p := img.data_p;                {make pointer to IMG library internal data}
  img_mem_alloc (                      {allocate our own private data block}
    img, sizeof(data_write_p^), data_write_p);
  with
      data_p^: d,                      {D is IMG library conn data block}
      data_write_p^: dr                {DR is JPG file private conn data block}
      do begin
  d.data_p := data_write_p;            {save adr of our data in IMG lib data block}
{
*   Open the image output file.
}
  file_open_write_bin (                {open output file}
    img.tnam,                          {complete pathname}
    '',                                {file name suffix}
    dr.conn,                           {returned connection state}
    stat);
  if sys_error(stat) then return;
{
*   Set up the JPEG library compression output stream.
}
  dr.jpegout.buf_p := addr(dr.buf);    {set address to output buffer}
  dr.jpegout.bufsize := sizeof(dr.buf); {set output buffer size}
  dr.jpegout.writebuf := univ_ptr(addr(writebuf)); {set buffer write routine}
  sys_error_none (dr.stat);            {init to no errors in called back routines}

  if iparm.gray
    then begin                         {make gray scale image}
      dr.pixform := jpeg_pixform_gray8_k; {select pixel format}
      dr.pixsize := 1;
      end
    else begin                         {make RGB color image}
      dr.pixform := jpeg_pixform_rgb24_k; {select pixel format}
      dr.pixsize := 3;
      end
    ;

  jpg_open_out (                       {open JPEG library output stream}
    dr.jpegout,                        {our private data for this JPEG output}
    iparm.qual,                        {0.0 to 1.0 quality level}
    img.x_size, img.y_size,            {image dimension in pixels}
    img.aspect * img.y_size / img.x_size, {width/height aspect ratio of each pixel}
    dr.pixform);                       {pixel format to pass to JPEG library}

  if sys_error(dr.stat) then begin     {error occurred in a called back routine ?}
    stat := dr.stat;
    goto abort1;
    end;

  img_mem_alloc (                      {allocate JPEG library scan line buffer}
    img, img.x_size * dr.pixsize, dr.scan_p);

  d.write_scan1 := addr(img_d_jpg_write_scan1);
  d.close := addr(img_d_jpg_close_write);
  return;
{
*   Error exits.  STAT must already be filled in.
}
abort1:                                {output file is open, JPEG resources alloc}
  jpg_close_out (dr.jpegout);          {deallocate JPEG library resources}
  file_close (dr.conn);                {close connection to output file}
  file_delete_name (dr.conn.tnam, dr.stat); {try to delete output file}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Local subroutine WRITEBUF (DR)
*
*   Write whatever data is in the output buffer to the output file.  Note
*   that the output buffer may be empty, in which case this routine does
*   nothing.
*
*   This routine is called from the JPEG library whenever it fills the output
*   buffer, or the output stream has ended.
}
procedure writebuf (                   {write output buffer to file}
  in out  dr: data_write_t);           {our private image connection data}
  internal; val_param;

begin
  if dr.jpegout.bufn <= 0 then return; {nothing to do ?}

  file_write_bin (                     {write the buffer full to the output file}
    dr.buf,                            {output buffer}
    dr.conn,                           {connection to output file}
    dr.jpegout.bufn,                   {amount of data to write}
    dr.stat);
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_JPG_WRITE_SCAN1 (IMG, SCAN, STAT)
*
*   Write one scan line of pixels to JPG output file.  IMG is the connection
*   handle for this image file connection.  SCAN is the array of format 1 pixels
*   to write to the file.  STAT is returned as the completion status code.
}
procedure img_d_jpg_write_scan1 (      {write a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_write_p: data_write_p_t;        {pointer to IMG driver private data}
  x: sys_int_machine_t;                {pixel X coordinate}
  put_p: ^int8u_t;                     {pointer to JPEG scan line pixel component}

begin
  sys_error_none (stat);               {init to no errors occurred}

  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_write_p := data_p^.data_p;      {make pointer to our private data block}
  with
      data_write_p^: dr                {DR is IMG file private conn data block}
      do begin
{
*   Copy the scan line data to JPEG library format.
}
  put_p := dr.scan_p;                  {init pointer to where to write next byte}
  case dr.pixform of                   {what is the pixel format ?}

jpeg_pixform_rgb24_k: begin            {pixel format is 24 bit RGB}
      for x := 0 to img.x_size-1 do begin {once for each pixel in the scan line}
        put_p^ := scan[x].red;
        put_p := succ(put_p);
        put_p^ := scan[x].grn;
        put_p := succ(put_p);
        put_p^ := scan[x].blu;
        put_p := succ(put_p);
        end;
      end;

jpeg_pixform_gray8_k: begin            {pixel format is 8 bit gray scale}
      for x := 0 to img.x_size-1 do begin {once for each pixel in the scan line}
        put_p^ := trunc(0.5 +
          scan[x].red * 0.299 +
          scan[x].grn * 0.587 +
          scan[x].blu * 0.114);
        put_p := succ(put_p);
        end;
      end;

    end;                               {end of pixel format cases}
{
*   Write the scan line.
}
  jpg_write_scan (dr.jpegout, dr.scan_p^); {send the scan line to the JPEG library}

  if sys_error(dr.stat) then begin     {error occurred in a called back routine ?}
    stat := dr.stat;                   {pass back error status}
    jpg_close_out (dr.jpegout);        {deallocate JPEG library resources}
    file_close (dr.conn);              {close output file}
    file_delete_name (dr.conn.tnam, dr.stat); {try to delete output file}
    end;

  img.next_y := img.next_y + 1;        {update Y coordinate of next scan line}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   IMG_D_JPG_CLOSE_WRITE (IMG, STAT)
*
*   Close connection to JPG output file.  IMG is the user connection handle.
*   STAT is returned as the completion status code.
}
procedure img_d_jpg_close_write (      {close JPG file open for write}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_write_p: data_write_p_t;        {pointer to IMG driver private data}

begin
  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_write_p := data_p^.data_p;      {make pointer to our private data block}
  with
      data_write_p^: dr                {DR is IMG file private conn data block}
      do begin

  jpg_close_out (dr.jpegout);          {close JPEG output stream and deallocate}
  file_close (dr.conn);                {close output file}
  stat := dr.stat;                     {pass back any error from called back routine}

  end;                                 {done with D and DR abbreviations}
  end;
