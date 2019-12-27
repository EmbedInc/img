{   I/O driver for handling .TGA (Truevision TARGA) image files.
}
module img_driver_tga;
define img_d_tga_open_read;
define img_d_tga_open_write;
%include 'img2.ins.pas';

const
  inbuf_size = 4096;                   {size of chunk to read at one time}
  outbuf_size = 16384;                 {size of chunk to write at one time}

type
  scan_p_ar_t =                        {array of scan line start pointers}
    array[0..0] of img_scan1_arg_p_t;

  scan_p_ar_p_t =                      {pointer to an array of scan line pointers}
    ^scan_p_ar_t;

  data_read_t = record                 {private data block when reading TGA file}
    scan_p_ar_p: scan_p_ar_p_t;        {pointer to array of pointers to scan starts}
    end;

  data_read_p_t =                      {pointer to our private data when reading TGA}
    ^data_read_t;

  data_write_t = record                {private data block when writing TGA file}
    buf_p: univ_ptr;                   {points to start of output buffer}
    buf_size: sys_int_adr_t;           {total size of output buffer}
    wr_p: ^char;                       {pointer to next available byte in out buffer}
    buf_left: sys_int_adr_t;           {amount left in output buffer}
    run_val: img_pixel1_t;             {pixel value for current run}
    run_n: sys_int_machine_t;          {number of pixels in current run}
    conn: file_conn_t;                 {TGA file connection handle}
    end;

  data_write_p_t =                     {pointer to private data when writing TGA}
    ^data_write_t;
{
*   Internal driver entry point templates.  The two external routines for
*   opening an TGA file for read and write are declared in IMG2.INS.PAS, since
*   this driver comes standard with the IMG library.
}
procedure img_d_tga_read_scan1 (       {read a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure img_d_tga_rewind (           {rewind so next read is first image pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}
  forward;

procedure img_d_tga_close_read (       {close TGA file open for read}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure img_d_tga_write_scan1 (      {write a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure img_d_tga_close_write (      {close TGA file open for write}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  forward;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_OPEN_READ (FNAM, IMG, STAT)
*
*   Open a .tga file for read.
*
*   FNAM is the name of the .tga file to open.  It may already have the .tga
*   appendix.  IMG is the user connection handle.  Its DATA_P field has been
*   filled in and the internal IMG library data block allocated.  All other
*   fields in IMG and the IMG.DATA_P^ have been initialized to default or
*   illegal value, and generally need to be filled in.
*
*   The REWIND and CLOSE entry points, and at least one of the READ_SCAN1 or
*   READ_SCAN2 entry points need to be filled in.
}
procedure img_d_tga_open_read (        {open TGA file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}

const
  max_msg_parms = 4;                   {max number of msg parameters in MSG_PARMS}

var
  id_len: sys_int_machine_t;           {length of TGA file ID field}
  lut_present: boolean;                {TRUE if field 2 indicates LUT present}
  lut_mapped: boolean;                 {TRUE if using LUT}
  color: boolean;                      {TRUE if color, not black and white}
  runlen: boolean;                     {TRUE if run length compressed, not verbatim}
  lut_start: sys_int_machine_t;        {first LUT entry in file}
  lut_len_file: sys_int_machine_t;     {number of LUT entries in file}
  bits_lut: sys_int_machine_t;         {number of used bits / LUT entry in file}
  x_size, y_size: sys_int_machine_t;   {image size in pixels}
  bits_pix: sys_int_machine_t;         {number of total bits / TGA file pixel}
  bits_attr: sys_int_machine_t;        {number of attribute bits / TGA file pixel}
  bits_pix_rgb: sys_int_machine_t;     {number of RGB bits / TGA file pixel}
  shift_red: sys_int_machine_t;        {bits to shift right to get red data}
  shift_grn: sys_int_machine_t;        {bits to shift right to get green data}
  shift_blu: sys_int_machine_t;        {bits to shift right to get blue data}
  shift_gra: sys_int_machine_t;        {bits to shift right to get gray data}
  mask_red: sys_int_machine_t;         {to mask in only red data after shift}
  mask_grn: sys_int_machine_t;         {to mask in only green data after shift}
  mask_blu: sys_int_machine_t;         {to mask in only blue data after shift}
  mask_gra: sys_int_machine_t;         {to mask in only gray data from pixel}
  mask_psc: sys_int_machine_t;         {mask for reading pseudo-color pixels}
  bytes_pix: sys_int_adr_t;            {number of bytes to read next pixel}
  bits_rgb: sys_int_machine_t;         {user-visible RGB precision in bits}
  down: boolean;                       {TRUE if file data stored top-to-bottom}
  right: boolean;                      {TRUE if file data stored left-to-right}

  data_p: img_conn2_p_t;               {pointer to IMG library data about this conn}
  data_read_p: data_read_p_t;          {pointer to our private data when reading}
  conn: file_conn_t;                   {connection handle to TGA file}
  buf_p: ^char;                        {pointer to next input byte}
  buf_left: sys_int_adr_t;             {amount of data left in input buffer}
  i: sys_int_machine_t;                {scratch integer}
  p: univ_ptr;                         {scratch pointer for adr calculations}
  size: sys_int_adr_t;                 {scratch for amount of memory or data}
  lut_p: img_scan1_arg_p_t;            {pointer to pixel value lookup table}
  y: sys_int_machine_t;                {current scan line number}
  dy: sys_int_machine_t;               {increment for next scan line number}
  pix_p: img_pixel1_p_t;               {pointer to first pixel in a scan}
  dpix_p: sys_int_adr_t;               {PIX_P increment to get to next pixel}
  pix_p_start_ofs: sys_int_adr_t;      {PIX_P starting offset from scan line start}
  run_val: img_pixel1_t;               {value of current run of pixels}
  run_left: sys_int_machine_t;         {number of pixels left in RUN_VAL}
  run_verbatim: boolean;               {TRUE if run is list of verbatim pixels}
  buf:                                 {input buffer}
    array[1..inbuf_size] of char;
  msg_parms:                           {parameter indicies for passing to messages}
    array[1..max_msg_parms] of sys_parm_msg_t;
{
*****************************
*
*   Local subroutine READ_BYTE (C, STAT)
*   This subroutine is local to IMG_D_TGA_OPEN_READ.
*
*   Read the next byte from the TGA file and return it as a CHAR in C.
*   STAT is the completion status code.
}
procedure read_byte (
  out     c: char;                     {8 bit value of next TGA file byte}
  out     stat: sys_err_t);            {completion status code}

begin
  sys_error_none (stat);               {init to no error}

  if buf_left <= 0 then begin          {need to read a new buffer full ?}
    file_read_bin (                    {read next chunk from TGA file}
      conn,                            {connection handle to TGA file}
      inbuf_size,                      {amount of data we want to have available}
      buf,                             {input buffer}
      buf_left,                        {amount of data actually returned}
      stat);
    discard(file_eof_partial(stat));   {EOF is OK after reading only partial buffer}
    if sys_error(stat) then return;
    buf_p := univ_ptr(addr(buf));
    end;                               {we now have new buffer full}

  c := buf_p^;                         {grab this byte from buffer}
  buf_p := univ_ptr(                   {advance buffer pointer to next byte}
    sys_int_adr_t(buf_p) + sizeof(buf_p^));
  buf_left := buf_left - 1;            {one less byte left in buffer}
  end;
{
*****************************
*
*   Local subroutine SKIP (N, STAT)
*   This subroutine is local to IMG_D_TGA_OPEN_READ.
*
*   Skip over the next N bytes in the TGA file.
}
procedure skip (
  in      n: sys_int_adr_t;            {number of bytes to skip over}
  out     stat: sys_err_t);

var
  left: sys_int_adr_t;                 {number of bytes left to skip}
  skip: sys_int_adr_t;                 {number of bytes to skip this chunk}

label
  skip_loop;

begin
  sys_error_none (stat);               {init to no error}
  left := n;                           {init number of bytes left to skip}

skip_loop:                             {back here if need to skip another chunk}
  skip := min(left, buf_left);         {amount we can skip in this buffer}
  buf_p := univ_ptr(                   {advance buffer pointer by skip amount}
    sys_int_adr_t(buf_p) + skip);
  buf_left := buf_left - skip;         {buffer now has fewer unread bytes in it}
  left := left - skip;                 {update how many bytes left to skip}
  if left <= 0 then return;            {skipped all the required bytes ?}

  file_read_bin (                      {read next chunk from TGA file}
    conn,                              {connection handle to TGA file}
    inbuf_size,                        {amount of data we want to have available}
    buf,                               {input buffer, data read in will be ignored}
    buf_left,                          {amount of data actually returned}
    stat);
  discard(file_eof_partial(stat));     {EOF is OK after reading only partial buffer}
  if sys_error(stat) then return;
  buf_p := univ_ptr(addr(buf));
  goto skip_loop;                      {back and skip next chunk of bytes}
  end;
{
*****************************
*
*   Local subroutine READ8 (I, STAT)
*   This subroutine is local to IMG_D_TGA_OPEN_READ.
*
*   Read the next byte from the TGA file and return its value in I.
*   I will be in the range 0-255.
}
procedure read8 (
  out     i: sys_int_machine_t;        {returned 0-255 byte value}
  out     stat: sys_err_t);

var
  c: char;

begin
  read_byte (c, stat);
  i := ord(c);
  end;
{
*****************************
*
*   Local subroutine READ16 (I, STAT)
*   This subroutine is local to IMG_D_TGA_OPEN_READ.
*
*   Read the next two bytes in the TGA file and return their value as an integer.
*   The low byte is first, the high byte second.
}
procedure read16 (
  out     i: sys_int_machine_t;        {returned value in range -32768 to 32767}
  out     stat: sys_err_t);

var
  c1, c2: char;

begin
  read_byte (c1, stat);                {read low byte}
  read_byte (c2, stat);                {read high byte}
  i := lshft(ord(c2), 8) ! ord(c1);    {pass back assembled integer}
  end;
{
*****************************
*
*   Local subroutine READ_PIX (PIX, STAT)
*   This subroutine is local to IMG_D_TGA_OPEN_READ.
*
*   Read the next pixel value from the TGA file, and return it in PIX.
*   No LUT mapping is performed.
*   The following state must have been previously set up so that the TGA file
*   file pixel can be properly read and interpreted:
*
*   BYTES_PIX  -  Number of bytes used to store the next pixel value.
*
*   SHIFT_RED, SHIFT_GRN, SHIFT_BLU, SHIFT_GRA
*     Once the all the TGA file bytes in the
*     pixel have been assembled into a 32 bit integer, these variables indicate
*     the number of bits to shift right to position each color component into
*     the low 8 bits.
*
*   MASK_RED, MASK_GRN, MASK_BLU, MASK_GRA
*     Mask applied to 32 bit integer after
*     the color component has been shifted into place.  This masks in only
*     the valid data for that color component.  Once the shift and mask
*     operation have been done, the 0-255 color component value will be left.
}
procedure read_pix (
  out     pix: img_pixel1_t;           {returned pixel value}
  out     stat: sys_err_t);            {completion status code}

var
  i: sys_int_machine_t;                {loop counter}
  pval: sys_int_conv32_t;              {assembled pixel value}
  n: sys_int_machine_t;                {number of bytes to read for LUT index}
  b_p: ^char;                          {pointer to where to put next byte}

begin
  pval := 0;                           {init all bits of pixel value}
  n := min(bytes_pix, sizeof(pval));   {number of pixel value bytes to actually use}

  case sys_byte_order_k of             {what is our machine byte order ?}
sys_byte_order_fwd_k: begin            {forwards byte order (high byte first)}
      b_p := univ_ptr(                 {init pointer to low PVAL byte}
        sys_int_adr_t(addr(pval)) + sizeof(pval) - 1);
      for i := 1 to n do begin         {once for each byte to use for pixel value}
        read_byte (b_p^, stat);        {read this byte into PVAL}
        if sys_error(stat) then return;
        b_p := univ_ptr(sys_int_adr_t(b_p) - 1); {to next most significant byte}
        end;
      end;
sys_byte_order_bkw_k: begin            {backwards byte order (low byte first)}
      b_p := univ_ptr(addr(pval));     {init pointer to low PVAL byte}
      for i := 1 to n do begin         {once for each byte to use for pixel value}
        read_byte (b_p^, stat);        {read this byte into PVAL}
        if sys_error(stat) then return;
        b_p := univ_ptr(sys_int_adr_t(b_p) + 1); {to next most significant byte}
        end;
      end;
    end;                               {end of machine byte order cases}

  if bytes_pix > n then begin          {there were more bytes than we could read ?}
    skip (bytes_pix - n, stat);        {skip over the remaining bytes}
    if sys_error(stat) then return;
    end;

  pix.alpha := 255;
  if color
    then begin                         {pixel contains RGB components}
      if shift_red > 0
        then pix.red := rshft(pval, shift_red) & mask_red
        else pix.red := lshft(pval, -shift_red) & mask_red;
      if shift_grn > 0
        then pix.grn := rshft(pval, shift_grn) & mask_grn
        else pix.grn := lshft(pval, -shift_grn) & mask_grn;
      if shift_blu > 0
        then pix.blu := rshft(pval, shift_blu) & mask_blu
        else pix.blu := lshft(pval, -shift_blu) & mask_blu;
      end
    else begin                         {pixel contains only GRAY component}
      if shift_gra > 0
        then i := rshft(pval, shift_gra) & mask_gra
        else i := lshft(pval, -shift_gra) & mask_gra;
      pix.red := i;
      pix.grn := i;
      pix.blu := i;
      end;
    ;
  end;
{
*****************************
*
*   Local subroutine GET_PIX_VAL (PIX, STAT)
*   This subroutine is local to IMG_D_TGA_OPEN_READ.
*
*   Read the next raw pixel value from the TGA file and return its ultimate
*   value in PIX.  LUT mapping is performed, if enabled.
}
procedure get_pix_val (
  out     pix: img_pixel1_t;           {returned pixel value}
  out     stat: sys_err_t);            {completion status code}

var
  ind: sys_int_conv32_t;               {LUT index value}
  n: sys_int_machine_t;                {number of bytes to read for LUT index}
  i: sys_int_machine_t;                {loop counter}
  b_p: ^char;                          {pointer to where to put next byte}

begin
  if not lut_mapped then begin         {interpret pixel value directly ?}
    read_pix (pix, stat);
    return;
    end;

  ind := 0;                            {init all bits of LUT index value}
  n := min(bytes_pix, sizeof(ind));    {number of LUT index bytes to actually use}
  case sys_byte_order_k of             {what is our machine byte order ?}
sys_byte_order_fwd_k: begin            {forwards byte order (high byte first)}
      b_p := univ_ptr(                 {init pointer to low IND byte}
        sys_int_adr_t(addr(ind)) + sizeof(ind) - 1);
      for i := 1 to n do begin         {once for each byte to use for LUT index}
        read_byte (b_p^, stat);        {read this byte into IND}
        if sys_error(stat) then return;
        b_p := univ_ptr(sys_int_adr_t(b_p) - 1); {to next most significant byte}
        end;
      end;
sys_byte_order_bkw_k: begin            {backwards byte order (low byte first)}
      b_p := univ_ptr(addr(ind));      {init pointer to low IND byte}
      for i := 1 to n do begin         {once for each byte to use for LUT index}
        read_byte (b_p^, stat);        {read this byte into IND}
        if sys_error(stat) then return;
        b_p := univ_ptr(sys_int_adr_t(b_p) + 1); {to next most significant byte}
        end;
      end;
    end;                               {end of machine byte order cases}

  if bytes_pix > n then begin          {there were more bytes than we could read ?}
    skip (bytes_pix - n, stat);        {skip over the remaining bytes}
    if sys_error(stat) then return;
    end;

  ind := ind & mask_psc;               {mask in only LUT index bits}
  pix := lut_p^[ind];                  {look up final pixel value in LUT}
  end;
{
*****************************
*
*   Local subroutine SETUP_MAPPED (STAT)
*   This subroutine is local to IMG_D_TGA_OPEN_READ.
*
*   Set up the local state for reading pixels that are to be mapped thru
*   a lookup table.
}
procedure setup_mapped (
  out     stat: sys_err_t);

var
  lut_size: sys_int_adr_t;             {size of pixel value lookup table}
  lut_ents: sys_int_machine_t;         {total number of LUT entries}
  i: sys_int_machine_t;                {loop counter}
  v: sys_int_machine_t;                {color component value}

begin
  sys_error_none (stat);               {init to no errors}
  lut_ents := lshft(1, bits_pix_rgb);  {make total number of LUT entries}
  lut_size := lut_ents * sizeof(img_pixel1_t); {size of whole LUT}
  util_mem_grab (lut_size, data_p^.mem_p^, true, lut_p); {allocate memory for LUT}
{
*   Init LUT to gray ramp.
}
  for i := 0 to lut_ents-1 do begin    {once for each LUT entry}
    v :=                               {0-255 RGB value at this LUT entry}
      min(255, trunc(i * 256.0 / (lut_ents - 1)));
    lut_p^[i].alpha := 255;
    lut_p^[i].red := v;
    lut_p^[i].grn := v;
    lut_p^[i].blu := v;
    end;                               {back and init next LUT entry}
{
*   Set up so that READ_PIX routine understands format of LUT entries.
}
  bytes_pix := (bits_lut + 7) div 8;   {number of bytes per TGA file LUT data entry}
  if color
    then begin                         {pixel values are RGB}
      i := min(bits_lut div 3, 8);     {bits for each LUT data RGB component}
      shift_blu := i - 8;              {bits to shift right to align component data}
      shift_grn := shift_blu + i;
      shift_red := shift_grn + i;
      if shift_blu > 0
        then mask_blu := 255 & rshft(-1, shift_blu)
        else mask_blu := 255 & lshft(-1, -shift_blu);
      mask_grn := mask_blu;
      mask_red := mask_blu;
      end
    else begin                         {pixel values are gray}
      shift_gra := bits_lut - 8;       {bits shift right to align gray data}
      if shift_gra > 0
        then mask_gra := 255 & rshft(-1, shift_gra)
        else mask_gra := 255 & lshft(-1, -shift_gra);
      end
    ;

  for i := lut_start to (lut_start+lut_len_file-1) do begin {each LUT entry in file}
    read_pix (lut_p^[i], stat);        {fill in RGB value for color index I}
    if sys_error(stat) then return;
    end;                               {back and fill in next LUT entry}
{
*   Set other state for reading pixel values from file later.
}
  bytes_pix := (bits_pix + 7) div 8;   {bytes per pixel in file}
  mask_psc := lut_ents - 1;            {for masking in only pseudo color LUT index}
  bits_rgb := 8;                       {set precision of RGB values returned to user}
  end;
{
*****************************
*
*   Local subroutine SETUP_UNMAPPED (STAT)
*   This subroutine is local to IMG_D_TGA_OPEN_READ.
*
*   Set up the local state for reading pixels that will not be mapped thru
*   a LUT.
}
procedure setup_unmapped (
  out     stat: sys_err_t);

var
  i: sys_int_machine_t;

begin
  sys_error_none (stat);               {no errors here}
  bytes_pix := (bits_pix + 7) div 8;   {bytes per pixel in file}
  if color
    then begin                         {pixel values are RGB}
      i := min(bits_pix_rgb div 3, 8); {bits for each pixel RGB component}
      shift_blu := i - 8;              {bits to shift right to align component data}
      shift_grn := shift_blu + i;
      shift_red := shift_grn + i;
      if shift_blu > 0
        then mask_blu := 255 & rshft(-1, shift_blu)
        else mask_blu := 255 & lshft(-1, -shift_blu);
      mask_grn := mask_blu;
      mask_red := mask_blu;
      bits_rgb := i;                   {user-visible RGB resolution}
      end
    else begin                         {pixel values are gray}
      shift_gra := bits_pix_rgb - 8;   {bits shift right to align gray data}
      if shift_gra > 0
        then mask_gra := 255 & rshft(-1, shift_gra)
        else mask_gra := 255 & lshft(-1, -shift_gra);
      bits_rgb := min(bits_pix_rgb, 8); {user-visible RGB resolution}
      end
    ;
  end;
{
*****************************
*
*   Local subroutine READ_SCAN (STAT)
*   This subroutine is local to IMG_D_TGA_OPEN_READ.
*
*   Read in all the pixels from the next scan line in the TGA file.  The
*   final interpreted values resulting from these pixels will be stored
*   in the current scan line buffer.  The first pixel will be stored at where
*   PIX_P is pointing.  Each subsequent pixel will be stored DPIX_P offset
*   from the previous pixel.  These values have been previously set to take
*   care of right or left pixel order.
}
procedure read_scan (
  out     stat: sys_err_t);

var
  pix_left: sys_int_machine_t;         {pixels left on this scan line}
  i: sys_int_machine_t;                {loop counter}
  pix_cnt: sys_int_machine_t;          {number of pixels used from current run}

label
  runlen_encoded;

begin
  if runlen then goto runlen_encoded;  {pixel data runlength encoded ?}
{
*   Pixel data is not compressed.
}
  for i := 1 to x_size do begin        {once for every pixel in scan}
    get_pix_val (pix_p^, stat);        {stuff this pixel value}
    if sys_error(stat) then return;
    pix_p := univ_ptr(                 {advance pointer to next pixel in scan}
      sys_int_adr_t(pix_p) + dpix_p);
    end;                               {back and do next pixel in scan}
  return;                              {all done, uncompressed case}
{
*   Pixel data is runlength compressed.
}
runlen_encoded:
  pix_left := x_size;                  {init number of pixels left to process}

  while pix_left > 0 do begin          {back here each runlen packet}

    if run_left <= 0 then begin        {need to read in another run ?}
      read8 (i, stat);                 {get run length byte value}
      run_left := (i & 127) + 1;       {extract pixel count field}
      run_verbatim := (i & 128) = 0;   {TRUE if list of verbatim pixels}
      if not run_verbatim then begin   {this run is one pixel value repeated ?}
        get_pix_val (run_val, stat);
        end;
      if sys_error(stat) then return;
      end;                             {done reading new run}

    pix_cnt := min(run_left, pix_left); {clip run size we use to this scan line}
    if run_verbatim
      then begin                       {PIX_CNT verbatim pixels follow}
        for i := 1 to pix_cnt do begin {once for each pixel in verbatim packet}
          get_pix_val (pix_p^, stat);  {stuff this pixel value}
          pix_p := univ_ptr(           {advance pointer to next pixel in scan}
            sys_int_adr_t(pix_p) + dpix_p);
          end;                         {back and do next pixel in packet}
        end
      else begin                       {PIX_CNT is repetition count for next value}
        for i := 1 to pix_cnt do begin {once for each repeated pixel in run}
          pix_p^ := run_val;           {copy run pixel value into this pixel}
          pix_p := univ_ptr(           {advance pointer to next pixel in scan}
            sys_int_adr_t(pix_p) + dpix_p);
          end;                         {back and do next pixel in packet}
        end
      ;
    run_left := run_left - pix_cnt;    {update number of pixels left in this run}
    pix_left := pix_left - pix_cnt;    {update number of pixels left this scan}
    end;                               {back and process rest of pixels this scan}
  end;
{
*****************************
*
*   Start of main routine, IMG_D_TGA_OPEN_READ.
}
begin
  data_p := img.data_p;                {make pointer to IMG library internal data}
  img_mem_alloc (                      {allocate our own private data block}
    img, sizeof(data_read_p^), data_read_p);
  with
      data_p^: d,                      {D is IMG library conn data block}
      data_read_p^: dr                 {DR is TGA file private conn data block}
      do begin

  d.data_p := data_read_p;             {save pointer to our private data block}

  string_fnam_extend (fnam, '.tga', img.tnam); {require .tga file name suffix}
  file_open_read_bin (                 {open TGA file for READ operations}
    img.tnam,                          {file name with .tga suffix}
    '',                                {file name extension string}
    conn,                              {file connection handle}
    stat);
  if sys_error(stat) then return;      {error trying to open IMG file ?}
  buf_left := 0;                       {init to no TGA file bytes in our buffer}
  lut_p := nil;                        {init to indicate no LUT allocated}
{
*   The TGA file has been successfully opened.  Process the fixed header.
}
  read8 (id_len, stat);                {get ID LENGTH field}
  if sys_error(stat) then return;

  read8 (i, stat);                     {get COLOR MAP TYPE field value}
  if sys_error(stat) then return;
  case i of                            {all the cases defined in manual}
0: begin
      lut_present := false;
      end;
1: begin
      lut_present := true;
      end;
otherwise
    sys_msg_parm_int (msg_parms[1], i);
    sys_message_bomb ('img', 'tga_bad_lut_type', msg_parms, 1);
    end;

  read8 (i, stat);                     {get IMAGE TYPE field value}
  if sys_error(stat) then return;
  case i of                            {all the cases defined in manual}
0: begin
      sys_message_bomb ('img', 'tga_no_image', nil, 0);
      end;
1: begin
      runlen := false;
      lut_mapped := true;
      color := true;
      end;
2: begin
      runlen := false;
      lut_mapped := false;
      color := true;
      end;
3: begin
      runlen := false;
      lut_mapped := false;
      color := false;
      end;
9: begin
      runlen := true;
      lut_mapped := true;
      color := true;
      end;
10: begin
      runlen := true;
      lut_mapped := false;
      color := true;
      end;
11: begin
      runlen := true;
      lut_mapped := false;
      color := false;
      end;
otherwise
    sys_msg_parm_int (msg_parms[1], i);
    sys_message_bomb ('img', 'tga_bad_image_type', msg_parms, 1);
    end;                               {end of IMAGE TYPE cases}
  if lut_mapped and (not lut_present) then begin
    sys_message_bomb ('img', 'tga_no_lut', nil, 0);
    end;

  if lut_present
    then begin                         {LUT exists}
      read16 (lut_start, stat);        {first valid color table index}
      if sys_error(stat) then return;
      read16 (lut_len_file, stat);     {number of color table indicies}
      if sys_error(stat) then return;
      read8 (bits_lut, stat);          {bits per color table entry in file}
      if sys_error(stat) then return;
      end
    else begin                         {LUT does not exist}
      skip (5, stat);                  {skip over unused LUT data fields}
      if sys_error(stat) then return;
      lut_start := 0;                  {set fields to "unused" values}
      lut_len_file := 0;
      bits_lut := 0;
      end
    ;

  skip (4, stat);                      {skip over image origin fields}
  if sys_error(stat) then return;

  read16 (x_size, stat);               {width of image in pixels}
  if sys_error(stat) then return;

  read16 (y_size, stat);               {height of image in pixels}
  if sys_error(stat) then return;

  read8 (bits_pix, stat);              {number of bits / pixel in image data section}
  if sys_error(stat) then return;

  read8 (i, stat);                     {get IMAGE DESCRIPTOR field value}
  if sys_error(stat) then return;
  bits_attr := i & 15;                 {number of attribute bits per pixel}
  right := (i & 16) = 0;               {set flag for horizontal pixel order}
  down := (i & 32) <> 0;               {set flag for vertical scan line order}
  bits_pix_rgb := bits_pix - bits_attr; {number of RGB bits / TGA file pixel}

  skip (id_len, stat);                 {skip over IMAGE ID field}
  if sys_error(stat) then return;

  if lut_mapped
    then begin
      setup_mapped (stat);             {set up for processing mapped pixels}
      end
    else begin
      setup_unmapped (stat);           {set up for processing direct pixels}
      end
    ;
  if sys_error(stat) then return;
{
*   All the data except for the pixel values has been read from the TGA file
*   and processed.  Now fill in the various data structures that will survive
*   this routine.
*
*   Fill in user-visible connection handle.
}
  img.x_size := x_size;
  img.y_size := y_size;
  img.aspect := img.x_size / img.y_size; {assume square pixels}
  img.bits_alpha := 0;
  img.bits_red := bits_rgb;
  img.bits_grn := bits_rgb;
  img.bits_blu := bits_rgb;
  img.bits_max := bits_rgb;
  string_vstring (img.file_type, 'tga', 3);
  string_fnam_unextend (conn.gnam, '.tga', img.gnam); {image file generic leaf name}
  string_copy (conn.tnam, img.tnam);   {image file full treename}
{
*   Fill in IMG library private connection data.
}
  d.read_scan1 := addr(img_d_tga_read_scan1);
  d.rewind := addr(img_d_tga_rewind);
  d.close := addr(img_d_tga_close_read);
{
*   Fill in our own private data block.
}
  size :=                              {size of scan line pointers array}
    img.y_size * sizeof(img_scan1_arg_p_t);
  img_mem_alloc (img, size, dr.scan_p_ar_p); {allocate scan line pointers array}

  size :=                              {size of all the scan line buffers}
    img.x_size * img.y_size * sizeof(img_pixel1_t);
  img_mem_alloc (img, size, p);        {allocate mem for all the scan lines}

  size := img.x_size * sizeof(img_pixel1_t); {amount of mem for one scan line}
  for y := 0 to img.y_size-1 do begin  {once for each scan line}
    dr.scan_p_ar_p^[y] := p;           {set start adr for this scan line}
    p := univ_ptr(                     {update P to point to where next scan goes}
      sys_int_adr_t(p) + size);
    end;
{
*   All data structures have been filled in.  Now read all the pixels from the
*   TGA file, convert them to format 1 pixel values, and store them in the
*   dynamically allocated scan line buffers.
}
  if right
    then begin                         {TGA file pixels stored left-to-right}
      dpix_p := sizeof(img_pixel1_t);  {increment to get to next pixel}
      pix_p_start_ofs := 0;            {starting PIX_P offset from scan start}
      end
    else begin                         {TGA file pixels stored right-to-left}
      dpix_p := -sizeof(img_pixel1_t); {increment to get to next pixel}
      pix_p_start_ofs :=               {starting PIX_P offset from scan start}
        (img.x_size - 1) * sizeof(img_pixel1_t);
      end
    ;
  if down
    then begin                         {TGA scan lines stored top-to-botton}
      y := 0;                          {init coordinate of first scan line}
      dy := 1;                         {increment to make next scan Y}
      end
    else begin                         {TGA scan lines stored bottom-to-top}
      y := img.y_size - 1;             {init coordinate of first scan line}
      dy := -1;                        {incmement to make next scan Y}
      end
    ;

  run_left := 0;                       {init to no pixels left in current run}
  for i := 1 to img.y_size do begin    {once for each scan line}
    pix_p := univ_ptr(                 {make adr of first pixel to fill in}
      sys_int_adr_t(dr.scan_p_ar_p^[y]) + pix_p_start_ofs);
    read_scan (stat);                  {read, convert, and store this scan line data}
    if sys_error(stat) then return;
    y := y + dy;                       {make coordinate of next scan line}
    end;                               {back and do next scan line in TGA file}
{
*   All the pixel values have been read from the TGA file, converted to our
*   format, and stored.
}
  file_close (conn);                   {close TGA file}
  if lut_p <> nil then begin           {LUT was allocated ?}
    util_mem_ungrab (lut_p, d.mem_p^); {deallocate LUT memory}
    end;
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_READ_SCAN1 (IMG, SCAN, STAT)
*
*   Return the next scan line of TGA file data.
}
procedure img_d_tga_read_scan1 (       {read a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: data_read_p_t;          {pointer to IMG driver private data}
  x: sys_int_machine_t;                {X pixel coordinate loop counter}

begin
  sys_error_none (stat);               {init to no error}

  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_read_p := data_p^.data_p;       {make pointer to our private connection data}
  with
      data_read_p^: dr                 {DR is IMG file private conn data block}
      do begin

  for x := 0 to img.x_size-1 do begin  {once for each pixel in scan}
    scan[x] := dr.scan_p_ar_p^[img.next_y]^[x];
    end;

  img.next_y := img.next_y + 1;        {update Y coordinate of next scan line}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_REWIND (IMG, STAT)
*
*   Reset so that next scan line read will be top scan line (Y = 0).
}
procedure img_d_tga_rewind (           {rewind so next read is first image pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}

begin
  sys_error_none (stat);
  img.next_y := 0;
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_CLOSE_READ (IMG, STAT)
*
*   Close this use of TGA file for read.
}
procedure img_d_tga_close_read (       {close TGA file open for read}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}

begin
  sys_error_none (stat);
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_WRITE_BUFF (D, STAT)
*
*   Write whatever is in the output buffer to the TGA output file.  Nothing
*   is done if the output buffer is empty.  D is the private TGA file connection
*   data block
}
procedure img_d_tga_write_buff (
  in out  d: data_write_t;             {private TGA data block for this connection}
  out     stat: sys_err_t);

var
  size: sys_int_adr_t;                 {amount of data to write}

begin
  size :=                              {find amount of data in output buffer}
    sys_int_adr_t(d.wr_p) - sys_int_adr_t(d.buf_p);
  if size <= 0 then return;            {nothing to write ?}
  d.wr_p := d.buf_p;                   {reset write pointer to start of buffer}
  file_write_bin (d.wr_p^, d.conn, size, stat);
  d.buf_left := d.buf_size;            {reset to whole buffer is available}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_WRITE_BYTE (C, D, STAT)
*
*   Write a byte to the TGA output file.  The byte is actually written into
*   the output buffer, which is only written to the file when it fills.
*   C is the data byte as a character.  D is the TGA file private connection handle
*   for this file.  STAT is returned as the completion status code.
}
procedure img_d_tga_write_byte (
  in      c: char;                     {data byte to write}
  in out  d: data_write_t;             {private TGA file connection data}
  out     stat: sys_err_t);

begin
  sys_error_none (stat);               {init to no error}
  d.wr_p^ := c;                        {stuff data byte into buffer}
  d.wr_p := univ_ptr(                  {update to point to next available buf byte}
    sys_int_adr_t(d.wr_p) + 1);
  d.buf_left := d.buf_left - 1;        {one less byte left available in out buffer}
  if d.buf_left <= 0 then begin        {we just filled whole output buffer ?}
    img_d_tga_write_buff (d, stat);
    end;
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_WRITE_I8 (I, D, STAT)
*
*   Write the low 8 bits of I to the TGA file.  D is the private TGA connection
*   block for this connection.
}
procedure img_d_tga_write_i8 (
  in      i: sys_int_machine_t;        {data value to write}
  in out  d: data_write_t;             {private TGA file connection block}
  out     stat: sys_err_t);            {completion status code}

begin
  img_d_tga_write_byte (chr(i), d, stat);
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_WRITE_I16 (I, D, STAT)
*
*   Write the low 16 bits of I to the TGA file.  D is the private TGA connection
*   block for this connection.
}
procedure img_d_tga_write_i16 (
  in      i: sys_int_machine_t;        {data value to write}
  in out  d: data_write_t;             {private TGA file connection block}
  out     stat: sys_err_t);            {completion status code}

begin
  img_d_tga_write_byte (chr(i & 255), d, stat);
  img_d_tga_write_byte (chr(rshft(i, 8) & 255), d, stat);
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_WRITE_I32 (I, D, STAT)
*
*   Write the low 32 bits of I to the TGA file.  D is the private TGA connection
*   block for this connection.
}
procedure img_d_tga_write_i32 (
  in      i: sys_int_machine_t;        {data value to write}
  in out  d: data_write_t;             {private TGA file connection block}
  out     stat: sys_err_t);            {completion status code}

type
  ovl_t = record case integer of       {integer / char overlay}
    1: (i: integer32);                 {everything as one integer field}
    2: (c: array[1..4] of char);       {each byte is separate array entry}
    end;

var
  ovl: ovl_t;                          {integer32 / chars overlay}

begin
  ovl.i := i;

  if sys_byte_order_k = sys_byte_order_fwd_k
    then begin                         {our CPU byte order is FORWARDS}
      img_d_tga_write_byte (ovl.c[4], d, stat);
      img_d_tga_write_byte (ovl.c[3], d, stat);
      img_d_tga_write_byte (ovl.c[2], d, stat);
      img_d_tga_write_byte (ovl.c[1], d, stat);
      end
    else begin                         {our CPU byte order is BACKWARDS}
      img_d_tga_write_byte (ovl.c[1], d, stat);
      img_d_tga_write_byte (ovl.c[2], d, stat);
      img_d_tga_write_byte (ovl.c[3], d, stat);
      img_d_tga_write_byte (ovl.c[4], d, stat);
      end
    ;
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_WRITE_STR (STR, LEN, D, STAT)
*
*   Write a string of characters to the TGA file.
}
procedure img_d_tga_write_str (
  in      str: string;                 {the characters to write}
  in      len: string_index_t;         {number of characters in STR}
  in out  d: data_write_t;             {private TGA file connection block}
  out     stat: sys_err_t);            {completion status code}

var
  i: sys_int_machine_t;                {loop counter}

begin
  for i := 1 to len do begin           {once for each character in string}
    img_d_tga_write_byte (str[i], d, stat); {write this character}
    if sys_error(stat) then return;
    end;
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_WRITE_RUN (D, STAT)
*
*   Write the current run, if there is any, to the TGA output file.  D is
*   the private TGA connection block for this connection.
}
procedure img_d_tga_write_run (
  in out  d: data_write_t;             {private TGA file connection block}
  out     stat: sys_err_t);            {completion status code}

var
  run_size: sys_int_machine_t;         {number of pixels in this run descriptor}

begin
  while d.run_n > 0 do begin           {keep looping until whole run written}
    run_size := min(d.run_n, 128);     {number of pixels in this run descriptor}
    img_d_tga_write_i8 (run_size+127, d, stat); {write run descriptor}
    img_d_tga_write_byte (chr(d.run_val.blu), d, stat);
    img_d_tga_write_byte (chr(d.run_val.grn), d, stat);
    img_d_tga_write_byte (chr(d.run_val.red), d, stat);
    if sys_error(stat) then return;
    d.run_n := d.run_n - run_size;
    end;                               {back and write next part of this run}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_OPEN_WRITE (IMG, PARMS, STAT)
*
*   Open a .TGA (Truevision Targa) image file for write.  The file format will
*   be 8 bit RGB, runlength encoded with no lookup table mapping.
}
procedure img_d_tga_open_write (       {open TGA file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library data about this conn}
  data_write_p: data_write_p_t;        {pointer to our private data when writing}

begin
  data_p := img.data_p;                {make pointer to IMG library internal data}
  img_mem_alloc (                      {allocate our own private data block}
    img, sizeof(data_write_p^), data_write_p);
  with
      data_p^: d,                      {D is IMG library conn data block}
      data_write_p^: dr                {DR is TGA file private conn data block}
      do begin
{
*   Fill in IMG library internal data block for this connection.
}
  d.data_p := data_write_p;            {save pointer to our private data block}
  d.write_scan1 := addr(img_d_tga_write_scan1);
  d.close := addr(img_d_tga_close_write);
{
*   Init our own private data block for this connection.
}
  dr.buf_size := outbuf_size;          {total size of output buffer}
  img_mem_alloc (img, dr.buf_size, dr.buf_p); {allocate output buffer}
  dr.wr_p := dr.buf_p;                 {init where next byte goes in output buffer}
  dr.buf_left := dr.buf_size;          {init to whole output buffer is available}
  dr.run_val.alpha := 255;             {always set to this for comparisons}
  dr.run_n := 0;                       {init number of pixels in this run}

  file_open_write_bin (img.tnam, '', dr.conn, stat); {open TGA file for write}
  if sys_error(stat) then return;
{
*   Write TGA file header.
}
  img_d_tga_write_i8 (0, dr, stat);    {length of ID field later}
  if sys_error(stat) then return;

  img_d_tga_write_i8 (0, dr, stat);    {no LUT is present}
  if sys_error(stat) then return;

  img_d_tga_write_i8 (10, dr, stat);   {format will be runlength encoded true color}
  if sys_error(stat) then return;

  img_d_tga_write_i16 (0, dr, stat);   {first LUT entry number}
  if sys_error(stat) then return;

  img_d_tga_write_i16 (0, dr, stat);   {number of LUT entries}
  if sys_error(stat) then return;

  img_d_tga_write_i8 (0, dr, stat);    {bits per LUT entry}
  if sys_error(stat) then return;

  img_d_tga_write_i16 (0, dr, stat);   {X origin}
  if sys_error(stat) then return;

  img_d_tga_write_i16 (0, dr, stat);   {Y origin}
  if sys_error(stat) then return;

  img_d_tga_write_i16 (img.x_size, dr, stat); {horizontal image size in pixels}
  if sys_error(stat) then return;

  img_d_tga_write_i16 (img.y_size, dr, stat); {vertical image size in pixels}
  if sys_error(stat) then return;

  img_d_tga_write_i8 (24, dr, stat);   {data bits per pixel}
  if sys_error(stat) then return;

  img_d_tga_write_i8 (32, dr, stat);   {start at upper left, no attribute bits}
  if sys_error(stat) then return;
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   Subroutine IMG_D_TGA_WRITE_SCAN1 (IMG, SCAN, STAT)
*
*   Write one scan line of pixels to TGA output file.  IMG is the connection
*   handle for this image file connection.  SCAN is the array of format 1 pixels
*   to write to the file.  STAT is returned as the completion status code.
}
procedure img_d_tga_write_scan1 (      {write a scan line of format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  pix: img_pixel1_t;                   {sanitized pixel value for run compare}
  x: sys_int_machine_t;                {X address of current pixel}
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_write_p: data_write_p_t;        {pointer to IMG driver private data}

begin
  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_write_p := data_p^.data_p;      {make pointer to our private data block}
  with
      data_write_p^: dr                {DR is IMG file private conn data block}
      do begin

  for x := 0 to img.x_size-1 do begin  {once for each pixel in scan line}
    pix := scan[x];                    {fetch this pixel}
    pix.alpha := 255;                  {sanitize for comparision with RUN_VAL}

    if pix.all <> dr.run_val.all then begin {this pixel starts a new run ?}
      img_d_tga_write_run (dr, stat);  {write out old run}
      if sys_error(stat) then return;
      dr.run_val := pix;               {set pixel value for new run}
      end;                             {done getting rid of old run}

    dr.run_n := dr.run_n + 1;          {one more pixel in current run}
    end;                               {back and do next pixel in scan line}

  img_d_tga_write_run (dr, stat);      {make sure run breaks at scan line break}
  end;                                 {done with D and DR abbreviations}
  end;
{
******************************************************************************
*
*   IMG_D_TGA_CLOSE_WRITE (IMG, STAT)
*
*   Close connection to TGA output file.  IMG is the user connection handle.
*   STAT is returned as the completion status code.
}
procedure img_d_tga_close_write (      {close TGA file open for write}
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

  img_d_tga_write_run (dr, stat);      {flush any run in progress}
  if sys_error(stat) then return;
{
*   Write TGA file footer to indicate this is a new format TGA file.
}
  img_d_tga_write_i32 (0, dr, stat);   {extension area offset, 0 = not used}
  if sys_error(stat) then return;

  img_d_tga_write_i32 (0, dr, stat);   {developer directory offset, 0 = not used}
  if sys_error(stat) then return;

  img_d_tga_write_str ('TRUEVISION-XFILE', 16, dr, stat); {signature string}
  if sys_error(stat) then return;

  img_d_tga_write_byte ('.', dr, stat); {extra period}
  if sys_error(stat) then return;

  img_d_tga_write_i8 (0, dr, stat);    {terminating zero}
  if sys_error(stat) then return;

  img_d_tga_write_buff (dr, stat);     {flush all remaining data in output buffer}

  file_close (dr.conn);                {close output file}
  end;                                 {done with D and DR abbreviations}
  end;
