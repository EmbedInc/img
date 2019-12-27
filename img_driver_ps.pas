{   IMG library driver for writing PS (PostScript) image files.
*   Reading of these files is not supported.
}
module img_driver_ps;
define img_d_ps_open_write;
define img_d_ps_write_scan1;
define img_d_ps_close_write;
%include 'img2.ins.pas';
{
*   Declare globally visible entry points that are defined here but not declared
*   in any include file.
}
procedure img_d_ps_write_scan1 (       {write next scan line from format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  extern;

procedure img_d_ps_close_write (       {close PS file that was open for writing}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  extern;

type
  data_write_t = record                {private data used for writing PS file}
    conn: file_conn_t;                 {handle to text output file}
    obuf: string_var80_t;              {one line output buffer}
    npix: sys_int_machine_t;           {number of pixels represented in OBUF}
    red, grn, blu: 0..255;             {pixel value of current run}
    nrun: sys_int_machine_t;           {number of pixels in current run}
    end;

  data_write_p_t =                     {pointer to our private data for writing PS}
    ^data_write_t;
{
***********************************************************
*
*   Local subroutine WRITE_BUF (DW, STAT)
*
*   Write the current output buffer to the PS file.  The output buffer
*   will be reset to empty.
}
procedure write_buf (
  in out  dw: data_write_t;            {our private PS file data}
  out     stat: sys_err_t);            {completion status code}
  internal;

begin
  if dw.obuf.len <= 0 then return;     {nothing to write ?}
  file_write_text (dw.obuf, dw.conn, stat); {write old buffer to file}
  dw.obuf.len := 0;                    {reset output buffer to empty}
  end;
{
***********************************************************
*
*   Local subroutine WRITE_RUN (DW, STAT)
*
*   Write the current run into the output buffer.
}
procedure write_run (
  in out  dw: data_write_t;            {our private PS file data}
  out     stat: sys_err_t);            {completion status code}
  internal;

var
  i: sys_int_conv24_t;                 {for holding combined RGB value}
  rlen: sys_int_machine_t;             {number of pixels in output run}
  s: array[1..5] of char;              {characters to hold one run}

begin
  sys_error_none(stat);                {init to no error}

  while dw.nrun > 0 do begin           {keep going until run exhausted}
    i := lshft(dw.red, 16) ! lshft(dw.grn, 8) ! dw.blu; {make composite value}
    s[4] := chr((i & 63) + 33);        {create the 4 pixel value characters}
    i := rshft(i, 6);
    s[3] := chr((i & 63) + 33);
    i := rshft(i, 6);
    s[2] := chr((i & 63) + 33);
    i := rshft(i, 6);
    s[1] := chr((i & 63) + 33);
    rlen := min(94, dw.nrun);          {length of this output run}
    dw.nrun := dw.nrun - rlen;         {remove this length from input run}
    s[5] := chr(rlen + 32);            {make run length character}
{
*   The characters for this output run are in S.
}
    if (dw.obuf.len + 5) > dw.obuf.max then begin {new chars don't fit in buffer ?}
      write_buf (dw, stat);
      if sys_error(stat) then return;
      end;
    string_appendn (dw.obuf, s, 5);    {append new run to current output buffer}
    end;                               {back until input run is exhausted}
  end;
{
***********************************************************
*
*   Open PS image file for write.
}
procedure img_d_ps_open_write (        {open encapsulated PostScript file for write}
  in out  img: img_conn_t;             {handle to new image file connection}
  in      parms: univ string_var_arg_t; {parameter string passed to driver}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {points to IMG library private data}
  dw_p: data_write_p_t;                {points to our private PS file data}
  token: string_var32_t;               {scratch string for parsing and number conv}
  buf: string_var80_t;                 {one line output buffer}
  stat2: sys_err_t;                    {scratch status to not corrupt STAT}

label
  abort1, abort2;
{
*******************************************
*
*   Local subroutine WRITE_LINE (S)
*   This routine is local to IMG_D_PS_OPEN_WRITE.
*
*   Write the string S as the next line of text to the output file.
*   STAT is set to indicate any errors.  The line is not written, and
*   STAT is not altered if STAT already indicates an error.
}
procedure write_line (
  in      s: string);                  {string to write, trailing blanks ignored}

begin
  if sys_error(stat) then return;      {already an error condition ?}
  string_vstring (buf, s, sizeof(s));  {make var string from input string}
  file_write_text (buf, dw_p^.conn, stat); {write the line, set STAT}
  end;
{
*******************************************
*
*   Start of IMG_D_PS_OPEN_WRITE
}
begin
  buf.max := sizeof(buf.str);          {init local var strings}
  token.max := sizeof(token.str);

  di_p := img.data_p;                  {get pointer to IMG library private data}
  util_mem_grab (                      {allocate our own PS file private data}
    sizeof(dw_p^), di_p^.mem_p^, true, dw_p);

  file_open_write_text (               {try to open PS output file}
    img.tnam, '.ps',                   {file name, suffix}
    dw_p^.conn,                        {handle to new file connection}
    stat);
  if sys_error(stat) then goto abort1;
{
*   The PS file has been successfully opened.
*   Now fill in the IMG library private data for this image file connection.
}
  di_p^.data_p := dw_p;                {save pointer to our mem in IMG private block}
  di_p^.write_scan1 := addr(img_d_ps_write_scan1); {pnt to write scan line routine}
  di_p^.close := addr(img_d_ps_close_write); {pointer to close connection routine}
{
*   Write DSC comments that preceed our symbol definitions.
}
  write_line ('%!PS-Adobe-3.0'(0));
  write_line ('%%Pages: 1'(0));
  write_line ('%%EndComments'(0));
  write_line ('%%BeginProlog'(0));
  write_line ('%%BeginResource: procset'(0));
  if sys_error(stat) then goto abort2;
{
*   Write symbol definitions that depend on parameters of the image.
}
  buf.len := 0;
  string_appends (buf, '/w '(0));
  string_f_fp_free (token, img.y_size * img.aspect, 5);
  string_append (buf, token);
  string_appends (buf, ' def /h '(0));
  string_f_int (token, img.y_size);
  string_append (buf, token);
  string_appends (buf, ' def'(0));
  file_write_text (buf, dw_p^.conn, stat);
  if sys_error(stat) then goto abort2;
{
*   Write the symbol definitions that don't need customization.
}
  write_line ('/instr 256 string def /obuf 1504 3 mul string def /pval 3'(0));
  write_line ('string def /xf { clippath pathbbox /y2 exch def /x2 exch'(0));
  write_line ('def /y1 exch def /x1 exch def /dx x2 x1 sub def /dy y2 y1'(0));
  write_line ('sub def dx w div dy h div 2 copy lt { pop /s exch def x1 dy'(0));
  write_line ('h s mul sub 2 div y1 add translate } { /s exch def pop dx w'(0));
  write_line ('s mul sub 2 div x1 add y1 translate } ifelse s s scale }'(0));
  write_line ('def /doline { /olen 0 def currentfile instr readline pop'(0));
  write_line ('length 0 exch 5 exch 5 sub { dup instr exch get 33 sub /i'(0));
  write_line ('exch def 1 add dup instr exch get 33 sub /i exch i 64 mul'(0));
  write_line ('add def 1 add dup instr exch get 33 sub /i exch i 64 mul'(0));
  write_line ('add def 1 add dup instr exch get 33 sub /i exch i 64 mul'(0));
  write_line ('add def pval 0 i -16 bitshift 255 and put pval 1 i -8'(0));
  write_line ('bitshift 255 and put pval 2 i 255 and put 1 add instr exch'(0));
  write_line ('get 32 sub olen exch 3 exch 3 mul olen add dup /olen exch'(0));
  write_line ('def 3 sub { obuf exch pval putinterval } for } for obuf 0'(0));
  write_line ('olen getinterval } def'(0));
  if sys_error(stat) then goto abort2;
{
*   More DSC comments after the static definitions and between the
*   real executable stuff.
}
  write_line ('%%EndResource'(0));
  write_line ('%%EndProlog'(0));
  write_line ('%%Page: () 1'(0));
  write_line ('%%BeginPageSetup'(0));
  write_line ('%%EndPageSetup'(0));
  if sys_error(stat) then goto abort2;
{
*   Write the actual executable code.
}
  buf.len := 0;
  string_appends (buf, 'xf '(0));
  string_f_int (token, img.x_size);
  string_append (buf, token);
  string_append1 (buf, ' ');
  string_f_int (token, img.y_size);
  string_append (buf, token);
  string_appends (buf, ' 8 [ '(0));
  string_f_fp_free (token, (img.x_size / img.y_size) / img.aspect, 5);
  string_append (buf, token);
  string_appends (buf, ' 0 0 -1 0 '(0));
  string_f_int (token, img.y_size);
  string_append (buf, token);
  string_appends (buf, ' ] { doline } false 3 colorimage'(0));
  file_write_text (buf, dw_p^.conn, stat);
  if sys_error(stat) then goto abort2;
{
*   Initialize our private PS file data structure.
}
  dw_p^.obuf.max := sizeof(dw_p^.obuf.str); {init output buffer to empty}
  dw_p^.obuf.len := 0;
  dw_p^.npix := 0;                     {init number of pixels in output buffer}
  dw_p^.red := 0;                      {init to arbitrary current run value}
  dw_p^.grn := 0;
  dw_p^.blu := 0;
  dw_p^.nrun := 0;                     {init number of pixels in current run}
  return;                              {normal return, no error}
{
*   Error exits.  STAT has already been set to indicate the particular error.
}
abort2:                                {jump here is output file is open}
  file_close (dw_p^.conn);             {close the PS file}
  file_delete_name (dw_p^.conn.tnam, stat2); {delete PS file, if possible}

abort1:                                {jump here if output file not open}
  util_mem_ungrab (dw_p, di_p^.mem_p^); {release our private PS file data area}
  end;
{
***********************************************************
*
*   Write one scan line to PS file.
}
procedure img_d_ps_write_scan1 (       {write next scan line from format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {points to IMG library private data}
  dw_p: data_write_p_t;                {points to our private PS file data}
  x: sys_int_machine_t;                {current pixel X coordinate}

begin
  di_p := img.data_p;                  {get pointer to IMG library private data}
  dw_p := di_p^.data_p;                {get pointer to our PS file private data}

  for x := 0 to img.x_size-1 do begin  {once for each pixel in this scan line}
    if                                 {new pixel doesn't match old run ?}
        (scan[x].red <> dw_p^.red) or
        (scan[x].grn <> dw_p^.grn) or
        (scan[x].blu <> dw_p^.blu)
        then begin
      write_run (dw_p^, stat);         {write contents of old run}
      if sys_error(stat) then return;
      dw_p^.red := scan[x].red;        {set pixel value for new run}
      dw_p^.grn := scan[x].grn;
      dw_p^.blu := scan[x].blu;
      dw_p^.nrun := 0;                 {init number of pixels in new run}
      end;                             {done making new run}
    dw_p^.nrun := dw_p^.nrun + 1;      {count one more pixel in this run}
    end;                               {back to do next pixel in this scan line}
  end;
{
***********************************************************
*
*   Close PS file.
}
procedure img_d_ps_close_write (       {close PS file that was open for writing}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {points to IMG library private data}
  dw_p: data_write_p_t;                {points to our private PS file data}

begin
  di_p := img.data_p;                  {get pointer to IMG library private data}
  dw_p := di_p^.data_p;                {get pointer to our PS file private data}

  write_run (dw_p^, stat);             {put last run into output buffer}
  if sys_error(stat) then return;
  write_buf (dw_p^, stat);             {write last output buffer to PS file}
  if sys_error(stat) then return;

  file_write_text (string_v('showpage'(0)), dw_p^.conn, stat);
  if sys_error(stat) then return;
  file_write_text (string_v('%%EOF'(0)), dw_p^.conn, stat);

  file_close (dw_p^.conn);             {close the PS output file}
  end;
