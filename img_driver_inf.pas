{   IMG library driver for handling Cirrus INF files.  The INF file is
*   an "information" file that describes where the files containing
*   the pixel data are, and provides some LUT information.
}
module img_driver_inf;
define img_d_inf_open_read;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';
{
*******************************************************************
*******************************************************************
*
*   This section contains declarations that are common to both
*   reading and writing INF image files.
}
type
  dform_k_t = (                        {pixel component data formats}
    dform_u8_k,                        {unsigned 8 bit integer}
    dform_s8_k,                        {signed 8 bit integer}
    dform_u16_k,                       {unsigned 16 bit integer}
    dform_s16_k);                      {signed 16 bit integer}

  comp_k_t = (                         {IDs for each pixel component}
    comp_red_k,                        {red}
    comp_grn_k,                        {green}
    comp_blu_k,                        {blue}
    comp_alpha_k);                     {alpha}
  comps_t = set of comp_k_t;           {all the component flags in one set}
{
*******************************************************************
*******************************************************************
*
*   This section contains information that is unique to reading
*   INF files.
}
type
  read_lutcomp_t =                     {LUT for reading one color component}
    array[0..65535] of int16u_t;

  read_comp_t = record                 {info for one color component}
    map_p: file_map_handle_p_t;        {pnt to mapped file handle, NIL on comp off}
    adrfile: sys_int_adr_t;            {mem mapped address of start of file}
    adr0: sys_int_adr_t;               {mem mapped address of first pixel data}
    dpix: sys_int_adr_t;               {offset for one pixel right in same scan}
    dscan: sys_int_adr_t;              {offset for one scan line down}
    conn: file_conn_t;                 {input file connection, may not be used}
    map: file_map_handle_t;            {mapped file handle, may not be used}
    fixed16: int16u_t;                 {fixed data when this comp off}
    fixed8: int8u_t;
    lut: read_lutcomp_t;               {map from unsigned data to 16u output format}
    end;
  read_comp_p_t = ^read_comp_t;

  read_t = record                      {private state for reading INF image}
    byord: sys_byte_order_k_t;         {byte order of image data files}
    dform: dform_k_t;                  {input file component data type}
    x_size, y_size: sys_int_machine_t; {our image size in pixels}
    red: read_comp_t;                  {data for each pixel component}
    grn: read_comp_t;
    blu: read_comp_t;
    alpha: read_comp_t;
    end;
  read_p_t = ^read_t;
{
*******************************************************************
*
*   Subroutine READ_CLOSE (IMG, STAT)
*
*   Close this connection to the image file.
}
procedure read_close (                 {close this connection}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {pointer to internal IMG data block}
  d_p: read_p_t;                       {pointer to our private GIF read data block}

begin
  sys_error_none (stat);               {init to no error occurred}
  di_p := img.data_p;                  {get pointer to internal IMG lib data block}
  d_p := di_p^.data_p;                 {get pointer to our private INF read data}
  with d_p^: d do begin                {D is our private INF read data block}

  if d.red.map_p = addr(d.red.map) then begin
    file_close (d.red.conn);
    end;
  if d.grn.map_p = addr(d.grn.map) then begin
    file_close (d.grn.conn);
    end;
  if d.blu.map_p = addr(d.blu.map) then begin
    file_close (d.blu.conn);
    end;
  if d.alpha.map_p = addr(d.alpha.map) then begin
    file_close (d.alpha.conn);
    end;

  end;                                 {done with D abbreviation}
  end;
{
*******************************************************************
*
*   Subroutine READ_REWIND (IMG, STAT)
*
*   "Rewind" the image file so that the next scan line read is the first
*   scan line in the image.
}
procedure read_rewind (                {rewind so that first pixel is next transfer}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}

begin
  sys_error_none (stat);               {init to no error occurred}
  img.next_y := 0;                     {reset to read top scan line next time}
  end;
{
*******************************************************************
*
*   Subroutine READ_SCAN1 (IMG, SCAN, STAT)
*
*   Read the next scan line from the image.  The result is returned in
*   SCAN, which is an array of format 1 pixels.
}
procedure read_scan1 (                 {read next scan line as format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {pointer to internal IMG data block}
  d_p: read_p_t;                       {pointer to our private GIF read data block}
  red8_p, grn8_p, blu8_p, alpha8_p: ^int8u_t; {pointer to 8 bit component values}
  red16_p, grn16_p, blu16_p, alpha16_p: ^int16u_t; {pnt to 16 bit component values}
  red16, grn16, blu16, alpha16: int16u_t; {16 bit component source values}
  x: sys_int_machine_t;                {X pixel coordinate}

begin
  sys_error_none (stat);               {init to no error occurred}
  di_p := img.data_p;                  {get pointer to internal IMG lib data block}
  d_p := di_p^.data_p;                 {get pointer to our private INF read data}
  with d_p^: d do begin                {D is our private INF read data block}

  if img.next_y >= d.y_size then begin {past end of image ?}
    sys_stat_set (file_subsys_k, file_stat_eof_k, stat);
    return;
    end;

  img.next_y := max(0, img.next_y);    {make sure scan line number in legal range}

  case d.dform of                      {what is the component pixel format}
{
*   Component format is 8 bit integer.  The LUTs have already been adjusted
*   so that we always read as unsigned.
}
dform_u8_k,
dform_s8_k: begin
  red8_p := univ_ptr(d.red.adr0 + d.red.dscan * img.next_y); {init read pointers}
  grn8_p := univ_ptr(d.grn.adr0 + d.grn.dscan * img.next_y);
  blu8_p := univ_ptr(d.blu.adr0 + d.blu.dscan * img.next_y);
  alpha8_p := univ_ptr(d.alpha.adr0 + d.alpha.dscan * img.next_y);
  for x := 0 to d.x_size-1 do begin    {once for each pixel in scan line}
    scan[x].red := rshft(d.red.lut[red8_p^], 8); {get and translate RGB value}
    scan[x].grn := rshft(d.grn.lut[grn8_p^], 8);
    scan[x].blu := rshft(d.blu.lut[blu8_p^], 8);
    scan[x].alpha := rshft(d.alpha.lut[alpha8_p^], 8);
    red8_p := univ_ptr(sys_int_adr_t(red8_p) + d.red.dpix); {advance read pointers}
    grn8_p := univ_ptr(sys_int_adr_t(grn8_p) + d.grn.dpix);
    blu8_p := univ_ptr(sys_int_adr_t(blu8_p) + d.blu.dpix);
    alpha8_p := univ_ptr(sys_int_adr_t(alpha8_p) + d.alpha.dpix);
    end;                               {back to do next pixel in scan line}
  end;
{
*   Component format is 16 bit integer.  The LUTs have already been adjusted
*   so that we always read as unsigned.
}
dform_u16_k,
dform_s16_k: begin
  red16_p := univ_ptr(d.red.adr0 + d.red.dscan * img.next_y); {init read pointers}
  grn16_p := univ_ptr(d.grn.adr0 + d.grn.dscan * img.next_y);
  blu16_p := univ_ptr(d.blu.adr0 + d.blu.dscan * img.next_y);
  alpha16_p := univ_ptr(d.alpha.adr0 + d.alpha.dscan * img.next_y);
  if d.byord = sys_byte_order_k
    then begin                         {data byte order matches our byte order}
      for x := 0 to d.x_size-1 do begin {once for each pixel in scan line}
        scan[x].red := rshft(d.red.lut[red16_p^], 8); {get and translate values}
        scan[x].grn := rshft(d.grn.lut[grn16_p^], 8);
        scan[x].blu := rshft(d.blu.lut[blu16_p^], 8);
        scan[x].alpha := rshft(d.alpha.lut[alpha16_p^], 8);
        red16_p := univ_ptr(sys_int_adr_t(red16_p) + d.red.dpix); {advance read pnts}
        grn16_p := univ_ptr(sys_int_adr_t(grn16_p) + d.grn.dpix);
        blu16_p := univ_ptr(sys_int_adr_t(blu16_p) + d.blu.dpix);
        alpha16_p := univ_ptr(sys_int_adr_t(alpha16_p) + d.alpha.dpix);
        end;                           {back to do next pixel in scan line}
      end
    else begin                         {incoming data must be flipped before use}
      for x := 0 to d.x_size-1 do begin {once for each pixel in scan line}
        red16 := red16_p^;             {fetch raw data values}
        grn16 := grn16_p^;
        blu16 := blu16_p^;
        alpha16 := alpha16_p^;
        sys_order_flip (red16, sizeof(red16)); {flip input data to internal order}
        sys_order_flip (grn16, sizeof(grn16));
        sys_order_flip (blu16, sizeof(blu16));
        sys_order_flip (alpha16, sizeof(alpha16));
        scan[x].red := rshft(d.red.lut[red16], 8); {translate to final values}
        scan[x].grn := rshft(d.grn.lut[grn16], 8);
        scan[x].blu := rshft(d.blu.lut[blu16], 8);
        scan[x].alpha := rshft(d.alpha.lut[alpha16], 8);
        red16_p := univ_ptr(sys_int_adr_t(red16_p) + d.red.dpix); {advance read pnts}
        grn16_p := univ_ptr(sys_int_adr_t(grn16_p) + d.grn.dpix);
        blu16_p := univ_ptr(sys_int_adr_t(blu16_p) + d.blu.dpix);
        alpha16_p := univ_ptr(sys_int_adr_t(alpha16_p) + d.alpha.dpix);
        end;                           {back to do next pixel in scan line}
      end
    ;
  end;

otherwise
    sys_stat_set (img_subsys_k, img_err_internal_k, stat);
    sys_stat_parm_int (1, stat);
    sys_stat_parm_str ('IMG_DRIVER_INF:READ_SCAN1', stat);
    end;

  img.next_y := img.next_y + 1;        {make scan line number for next time}
  end;                                 {done with D abbreviation}
  end;
{
*******************************************************************
*
*   Subroutine READ_SCAN2 (IMG, SCAN, STAT)
*
*   Read the next scan line from the image.  The result is returned in
*   SCAN, which is an array of format 2 pixels.
}
procedure read_scan2 (                 {read next scan line as format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  di_p: img_conn2_p_t;                 {pointer to internal IMG data block}
  d_p: read_p_t;                       {pointer to our private GIF read data block}
  red8_p, grn8_p, blu8_p, alpha8_p: ^int8u_t; {pointer to 8 bit component values}
  red16_p, grn16_p, blu16_p, alpha16_p: ^int16u_t; {pnt to 16 bit component values}
  red16, grn16, blu16, alpha16: int16u_t; {16 bit component source values}
  x: sys_int_machine_t;                {X pixel coordinate}

begin
  sys_error_none (stat);               {init to no error occurred}
  di_p := img.data_p;                  {get pointer to internal IMG lib data block}
  d_p := di_p^.data_p;                 {get pointer to our private INF read data}
  with d_p^: d do begin                {D is our private INF read data block}

  if img.next_y >= d.y_size then begin {past end of image ?}
    sys_stat_set (file_subsys_k, file_stat_eof_k, stat);
    return;
    end;

  img.next_y := max(0, img.next_y);    {make sure scan line number in legal range}

  case d.dform of                      {what is the component pixel format}
{
*   Component format is 8 bit integer.  The LUTs have already been adjusted
*   so that we always read as unsigned.
}
dform_u8_k,
dform_s8_k: begin
  red8_p := univ_ptr(d.red.adr0 + d.red.dscan * img.next_y); {init read pointers}
  grn8_p := univ_ptr(d.grn.adr0 + d.grn.dscan * img.next_y);
  blu8_p := univ_ptr(d.blu.adr0 + d.blu.dscan * img.next_y);
  alpha8_p := univ_ptr(d.alpha.adr0 + d.alpha.dscan * img.next_y);
  for x := 0 to d.x_size-1 do begin    {once for each pixel in scan line}
    scan[x].red := d.red.lut[red8_p^]; {get and translate RGB value}
    scan[x].grn := d.grn.lut[grn8_p^];
    scan[x].blu := d.blu.lut[blu8_p^];
    scan[x].alpha := d.alpha.lut[alpha8_p^];
    red8_p := univ_ptr(sys_int_adr_t(red8_p) + d.red.dpix); {advance read pointers}
    grn8_p := univ_ptr(sys_int_adr_t(grn8_p) + d.grn.dpix);
    blu8_p := univ_ptr(sys_int_adr_t(blu8_p) + d.blu.dpix);
    alpha8_p := univ_ptr(sys_int_adr_t(alpha8_p) + d.alpha.dpix);
    end;                               {back to do next pixel in scan line}
  end;
{
*   Component format is 16 bit integer.  The LUTs have already been adjusted
*   so that we always read as unsigned.
}
dform_u16_k,
dform_s16_k: begin
  red16_p := univ_ptr(d.red.adr0 + d.red.dscan * img.next_y); {init read pointers}
  grn16_p := univ_ptr(d.grn.adr0 + d.grn.dscan * img.next_y);
  blu16_p := univ_ptr(d.blu.adr0 + d.blu.dscan * img.next_y);
  alpha16_p := univ_ptr(d.alpha.adr0 + d.alpha.dscan * img.next_y);
  if d.byord = sys_byte_order_k
    then begin                         {data byte order matches our byte order}
      for x := 0 to d.x_size-1 do begin {once for each pixel in scan line}
        scan[x].red := d.red.lut[red16_p^]; {get and translate values}
        scan[x].grn := d.grn.lut[grn16_p^];
        scan[x].blu := d.blu.lut[blu16_p^];
        scan[x].alpha := d.alpha.lut[alpha16_p^];
        red16_p := univ_ptr(sys_int_adr_t(red16_p) + d.red.dpix); {advance read pnts}
        grn16_p := univ_ptr(sys_int_adr_t(grn16_p) + d.grn.dpix);
        blu16_p := univ_ptr(sys_int_adr_t(blu16_p) + d.blu.dpix);
        alpha16_p := univ_ptr(sys_int_adr_t(alpha16_p) + d.alpha.dpix);
        end;                           {back to do next pixel in scan line}
      end
    else begin                         {incoming data must be flipped before use}
      for x := 0 to d.x_size-1 do begin {once for each pixel in scan line}
        red16 := red16_p^;             {fetch raw data values}
        grn16 := grn16_p^;
        blu16 := blu16_p^;
        alpha16 := alpha16_p^;
        sys_order_flip (red16, sizeof(red16)); {flip input data to internal order}
        sys_order_flip (grn16, sizeof(grn16));
        sys_order_flip (blu16, sizeof(blu16));
        sys_order_flip (alpha16, sizeof(alpha16));
        scan[x].red := d.red.lut[red16]; {translate to final values}
        scan[x].grn := d.grn.lut[grn16];
        scan[x].blu := d.blu.lut[blu16];
        scan[x].alpha := d.alpha.lut[alpha16];
        red16_p := univ_ptr(sys_int_adr_t(red16_p) + d.red.dpix); {advance read pnts}
        grn16_p := univ_ptr(sys_int_adr_t(grn16_p) + d.grn.dpix);
        blu16_p := univ_ptr(sys_int_adr_t(blu16_p) + d.blu.dpix);
        alpha16_p := univ_ptr(sys_int_adr_t(alpha16_p) + d.alpha.dpix);
        end;                           {back to do next pixel in scan line}
      end
    ;
  end;

otherwise
    sys_stat_set (img_subsys_k, img_err_internal_k, stat);
    sys_stat_parm_int (1, stat);
    sys_stat_parm_str ('IMG_DRIVER_INF:READ_SCAN2', stat);
    end;

  img.next_y := img.next_y + 1;        {make scan line number for next time}
  end;                                 {done with D abbreviation}
  end;
{
*******************************************************************
*
*   Subroutine MAKE_LUT (LO, HI, DFORM, LUT)
*
*   Fill in the component lookup table LUT.  The raw component LO value
*   must map to the minimum intensity, and the raw component HI value
*   must map to the maximum intensity.  DFORM is the incoming component
*   data format.
*
*   The LUT is loaded so that incoming data values can always be read as
*   unsigned.  The LUT value is always unsigned 16 bit.
}
procedure make_lut (                   {fill in LUT for one component}
  in      lo: sys_int_machine_t;       {data value to map to min intensity}
  in      hi: sys_int_machine_t;       {data value to map to max intensity}
  in      dform: dform_k_t;            {component input data type}
  out     lut: read_lutcomp_t);        {component LUT to fill in}
  val_param; internal;

var
  minv, maxv: sys_int_machine_t;       {min/max component values for this DFORM}
  mask: sys_int_machine_t;             {mask for making 16u LUT index}
  i: sys_int_machine_t;                {input value loop counter}
  ind: sys_int_machine_t;              {LUT index}
  neg: boolean;                        {TRUE if LO/HI range flipped to negative}

begin
  case dform of                        {what is incoming data type ?}
dform_u8_k: begin                      {unsigned 8 bit}
      minv := 0;
      maxv := 255;
      mask := 255;
      end;
dform_s8_k: begin                      {signed 8 bit}
      minv := -128;
      maxv := 127;
      mask := 255;
      end;
dform_u16_k: begin                     {unsigned 16 bit}
      minv := 0;
      maxv := 65535;
      mask := 65535;
      end;
dform_s16_k: begin                     {signed 16 bit}
      minv := -32768;
      maxv := 32767;
      mask := 65535;
      end;
    end;

  neg := lo > hi;                      {TRUE if high/low ranged flipped to negative}

  for i := minv to maxv do begin       {once for each possible input value}
    ind := i & mask;                   {make LUT index for this value}
    if neg
      then begin                       {range is high to low}
        if i <= hi then begin
          lut[ind] := 65535;
          next;
          end;
        if i >= lo then begin
          lut[ind] := 0;
          next;
          end;
        end
      else begin                       {range is low to high}
        if i <= lo then begin
          lut[ind] := 0;
          next;
          end;
        if i >= hi then begin
          lut[ind] := 65535;
          next;
          end;
        end
      ;
    {
    *   The input value I is between HI and LO.
    }
    lut[ind] := trunc(65536.0 * (i - lo) / (hi - lo));
    end;                               {back to do next LUT entry}
  end;
{
*******************************************************************
*
*   Subroutine IMG_D_INF_OPEN_READ (FNAM, IMG, STAT)
*
*   Open the indicated INF file for read.
}
procedure img_d_inf_open_read (        {open INF file for read}
  in      fnam: univ string_var_arg_t; {image file name, may have extension}
  in out  img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}

const
  cmd_maxlen_k = 10;                   {max length of any INF file command keyword}
  n_cmd_k = 12;                        {number of command keywords in list}

  cmd_strlen_k = cmd_maxlen_k + 1;     {CMD string length to allow room for space}
  cmds_len_k = cmd_strlen_k * n_cmd_k; {total string length for commands list}

type
  cmd_t =                              {string to hold one command keyword}
    array[1..cmd_strlen_k] of char;

  cmds_t = record                      {var string with all the command keywords}
    max: string_index_t;
    len: string_index_t;
    str: array[1..n_cmd_k] of cmd_t;   {raw keywords separated by spaces}
    end;

var
  cmds: static cmds_t := [             {list of the INF file command keywords}
    len := cmds_len_k, max := cmds_len_k,
    str := [
      'WIDTH     ',                    {1}
      'HEIGHT    ',                    {2}
      'RED       ',                    {3}
      'GRN       ',                    {4}
      'BLU       ',                    {5}
      'ALPHA     ',                    {6}
      'FILE      ',                    {7}
      'FILEOFS   ',                    {8}
      'COMPOFS   ',                    {9}
      'PIXSTRIDE ',                    {10}
      'SCANSTRIDE',                    {11}
      'RANGE     ']                    {12}
      ];

type
  comp_t = record                      {data for each pixel component}
    fnam: string_treename_t;           {source file name}
    ofs0: sys_int_adr_t;               {offset to start of first pixel in file}
    ofscomp: sys_int_adr_t;            {offset within pixel for this component}
    stride_pix: sys_int_adr_t;         {pixel stride}
    stride_scan: sys_int_adr_t;        {scan line stride}
    lo, hi: sys_int_machine_t;         {input range to map to black-white range}
    p: read_comp_p_t;                  {pnt to this comp data in INF reading data}
    stride_pix_set: boolean;           {TRUE if pixel stride explicitly set}
    stride_scan_set: boolean;          {TRUE if scan line stride explicitly set}
    range_set: boolean;                {TRUE if LO/HI explicitly set}
    end;

  comp_ar_t =                          {array of info for each component}
    array[firstof(comp_k_t)..lastof(comp_k_t)] of comp_t;

var
  di_p: img_conn2_p_t;                 {pointer to internal IMG library data block}
  d_p: read_p_t;                       {pointer to our private INF read data block}
  inf_open: boolean;                   {TRUE when INF file is open}
  hi, lo: sys_int_machine_t;           {scratch high/low range values}
  i: sys_int_machine_t;                {scratch integer}
  ofs: sys_int_adr_t;                  {scratch address offset}
  pick: sys_int_machine_t;             {num of keyword picked from list}
  tlen: sys_int_adr_t;                 {data size actually transferred or mapped}
  p: string_index_t;                   {LINE parse index}
  comp: comp_ar_t;                     {array of info for each component}
  con: comps_t;                        {set of components enabled for curr command}
  c, c2: comp_k_t;                     {component loop indicies}
  dform_bits: sys_int_machine_t;       {number of bits in input data components}
  line: string_var8192_t;              {line read from INF file}
  cmd: string_var32_t;                 {INF file command name keyword}
  parm: string_var8192_t;              {parameter string to INF file command}
  dir: string_treename_t;              {starting directory for component files}
  conn: file_conn_t;                   {connection handle to INF file}

label
  loop_line, loop_cmd, done_cmds, got_file,
  err_syntax, abort1, abort0;

begin
  cmd.max := size_char(cmd.str);       {init local var strings}
  parm.max := size_char(parm.str);
  line.max := size_char(line.str);
  dir.max := size_char(dir.str);
  sys_error_none (stat);               {init to no errors encountered}
  inf_open := false;                   {init to INF file is not open}

  di_p := img.data_p;                  {get pointer to internal IMG lib data block}
  util_mem_grab (                      {allocate our private INF reading block}
    sizeof(d_p^),                      {amount of memory to allocate}
    di_p^.mem_p^,                      {parent memory context}
    true,                              {we may need to individually deallocate this}
    d_p);                              {returned pointer to the new memory}
  di_p^.data_p := d_p;                 {save pointer to our private state block}
  with d_p^: d do begin                {D is our private INF reading state block}

  string_fnam_extend (fnam, '.inf', img.tnam); {require the .INF suffix}
  file_open_read_text (                {try to open the INF file for text read}
    img.tnam,                          {file name}
    '',                                {file name suffix}
    conn,                              {returned file connection handle}
    stat);
  if sys_error(stat) then goto abort0;
  inf_open := true;                    {indicate INF file is open}

  string_pathname_split (conn.tnam, dir, parm); {save name of dir INF file is in}
{
*   INF file is open for text read on CONN.
*   Now init some state before reading the INF file.
}
  d.byord := sys_byte_order_k;         {init to same byte order as this system}
  d.dform := dform_u8_k;               {pixel component format is 8 bit unsigned int}
  d.x_size := 0;                       {init to invalid image size}
  d.y_size := 0;

  comp[comp_red_k].p := addr(d.red);
  comp[comp_grn_k].p := addr(d.grn);
  comp[comp_blu_k].p := addr(d.blu);
  comp[comp_alpha_k].p := addr(d.alpha);

  for c := firstof(c) to lastof(c) do begin {once for each pixel component}
    comp[c].fnam.max := size_char(comp[c].fnam.str);
    comp[c].fnam.len := 0;
    comp[c].ofs0 := 0;
    comp[c].ofscomp := 0;
    comp[c].stride_pix := 0;
    comp[c].stride_scan := 0;
    comp[c].lo := 0;
    comp[c].hi := 0;
    comp[c].stride_pix_set := false;
    comp[c].stride_scan_set := false;
    comp[c].range_set := false;
    comp[c].p^.map_p := nil;           {init to no file open for this component}
    end;
{
*   Back here to read each new line from the INF file.
}
loop_line:
  file_read_text (conn, line, stat);   {read next line from INF file}
  if file_eof(stat) then goto done_cmds; {hit end of INF file ?}
  if sys_error(stat) then goto abort0;
  string_unpad (line);                 {delete trailing spaces}
  if line.len = 0 then goto loop_line; {ignore blank lines}
  if line.str[1] = '*' then goto loop_line; {ignore comment lines}
  p := 1;                              {init input line parse index}
  string_token (line, p, cmd, stat);
  if sys_error(stat) then goto err_syntax;
  con := ~setof(con);                  {init to command applies to all components}

loop_cmd:                              {back here with new keyword in CMD}
  string_upcase (cmd);                 {make upper case for keyword matching}
  string_tkpick (cmd, cmds, pick);     {pick command keyword from list}
  case pick of                         {which command keyword is it ?}
{
*   WIDTH dx
}
1: begin                               {WIDTH}
  string_token_int (line, p, d.x_size, stat);
  end;
{
*   HEIGHT dy
}
2: begin                               {HEIGHT}
  string_token_int (line, p, d.y_size, stat);
  end;
{
*   RED command ...
}
3: begin                               {RED}
  con := [comp_red_k];
  string_token (line, p, cmd, stat);
  if sys_error(stat) then goto err_syntax;
  goto loop_cmd;
  end;
{
*   GRN command ...
}
4: begin                               {GRN}
  con := [comp_grn_k];
  string_token (line, p, cmd, stat);
  if sys_error(stat) then goto err_syntax;
  goto loop_cmd;
  end;
{
*   BLU command ...
}
5: begin                               {BLU}
  con := [comp_blu_k];
  string_token (line, p, cmd, stat);
  if sys_error(stat) then goto err_syntax;
  goto loop_cmd;
  end;
{
*   ALPHA command ...
}
6: begin                               {ALPHA}
  con := [comp_alpha_k];
  string_token (line, p, cmd, stat);
  if sys_error(stat) then goto err_syntax;
  goto loop_cmd;
  end;
{
*   FILE filename
}
7: begin                               {FILE}
  string_token (line, p, parm, stat);
  if sys_error(stat) then goto err_syntax;
  for c := firstof(c) to lastof(c) do begin {once for each component}
    if not (c in con) then next;       {command not apply to this component ?}
    string_copy (parm, comp[c].fnam);
    end;
  end;
{
*   FILEOFS offset
}
8: begin                               {FILEOFS}
  string_token_int (line, p, i, stat);
  if sys_error(stat) then goto err_syntax;
  for c := firstof(c) to lastof(c) do begin {once for each component}
    if not (c in con) then next;       {command not apply to this component ?}
    comp[c].ofs0 := i;
    end;
  end;
{
*   COMPOFS offset
}
9: begin                               {COMPOFS}
  string_token_int (line, p, i, stat);
  if sys_error(stat) then goto err_syntax;
  for c := firstof(c) to lastof(c) do begin {once for each component}
    if not (c in con) then next;       {command not apply to this component ?}
    comp[c].ofscomp := i;
    end;
  end;
{
*   PIXSTRIDE stride
}
10: begin                              {PIXSTRIDE}
  string_token_int (line, p, i, stat);
  if sys_error(stat) then goto err_syntax;
  for c := firstof(c) to lastof(c) do begin {once for each component}
    if not (c in con) then next;       {command not apply to this component ?}
    comp[c].stride_pix := i;
    comp[c].stride_pix_set := true;
    end;
  end;
{
*   SCANSTRIDE stride
}
11: begin                              {SCANSTRIDE}
  string_token_int (line, p, i, stat);
  if sys_error(stat) then goto err_syntax;
  for c := firstof(c) to lastof(c) do begin {once for each component}
    if not (c in con) then next;       {command not apply to this component ?}
    comp[c].stride_scan := i;
    comp[c].stride_scan_set := true;
    end;
  end;
{
*   RANGE low high
}
12: begin                              {RANGE}
  string_token_int (line, p, lo, stat); {get LOW value}
  if sys_error(stat) then goto err_syntax;
  string_token_int (line, p, hi, stat); {get HIGH value}
  if sys_error(stat) then goto err_syntax;
  for c := firstof(c) to lastof(c) do begin {once for each component}
    if not (c in con) then next;       {command not apply to this component ?}
    comp[c].lo := lo;
    comp[c].hi := hi;
    comp[c].range_set := true;
    end;
  end;
{
*   Unrecognized command name.
}
otherwise
    sys_stat_set (img_subsys_k, img_err_cmd_bad_k, stat);
    sys_stat_parm_vstr (cmd, stat);
    sys_stat_parm_int (conn.lnum, stat);
    sys_stat_parm_vstr (conn.tnam, stat);
    goto abort0;
    end;                               {end of command keyword cases}
{
*   All done processing this command.  STAT may be set to indicate an
*   error processing the command.
}
  if sys_error(stat) then goto err_syntax; {report err as syntax error on this line}
  string_token (line, p, parm, stat);  {try to extract another token on this line}
  if string_eos(stat) then goto loop_line; {no extra junk on line, back for next cmd}
  goto err_syntax;                     {report extra token as syntax error}
{
*   All done reading the INF file.
}
done_cmds:
  file_close (conn);                   {close the INF file}
  inf_open := false;                   {flag that INF is not currently open}

  if (d.x_size < 1) or (d.y_size < 1) then begin {invalid image size ?}
    sys_stat_set (img_subsys_k, img_err_file_data_k, stat);
    sys_stat_parm_int (1, stat);
    sys_stat_parm_str ('IMG_D_INF_OPEN_READ', stat);
    goto abort0;
    end;

  case d.dform of                      {what is pixel component format ?}
dform_u8_k: begin                      {unsigned 8 bit integer}
      lo := 0;
      hi := 255;
      dform_bits := 8;
      for c := firstof(c) to lastof(c) do begin
        if c = comp_alpha_k
          then begin
            comp[c].p^.fixed8 := hi;
            end
          else begin
            comp[c].p^.fixed8 := lo;
            end
          ;
        end;
      end;
dform_s8_k: begin                      {signed 8 bit integer}
      lo := -128;
      hi := 127;
      dform_bits := 8;
      for c := firstof(c) to lastof(c) do begin
        if c = comp_alpha_k
          then begin
            comp[c].p^.fixed8 := hi;
            end
          else begin
            comp[c].p^.fixed8 := lo;
            end
          ;
        end;
      end;
dform_u16_k: begin                     {unsigned 16 bit integer}
      lo := 0;
      hi := 65535;
      dform_bits := 16;
      for c := firstof(c) to lastof(c) do begin
        if c = comp_alpha_k
          then begin
            comp[c].p^.fixed16 := hi;
            end
          else begin
            comp[c].p^.fixed16 := lo;
            end
          ;
        if sys_byte_order_k <> d.byord then begin
          sys_order_flip (comp[c].p^.fixed16, sizeof(comp[c].p^.fixed16));
          end;
        end;
      end;
dform_s16_k: begin                     {signed 16 bit integer}
      lo := -32768;
      hi := 32767;
      dform_bits := 16;
      for c := firstof(c) to lastof(c) do begin
        if c = comp_alpha_k
          then begin
            comp[c].p^.fixed16 := hi;
            end
          else begin
            comp[c].p^.fixed16 := lo;
            end
          ;
        if sys_byte_order_k <> d.byord then begin
          sys_order_flip (comp[c].p^.fixed16, sizeof(comp[c].p^.fixed16));
          end;
        end;
      end;
otherwise
    sys_stat_set (img_subsys_k, img_err_internal_k, stat);
    sys_stat_parm_int (1, stat);
    sys_stat_parm_str ('IMG_D_INF_OPEN_READ', stat);
    goto abort0;
    end;

  ofs :=                               {make mem size of pixel component value}
    (dform_bits + sys_bits_adr_k - 1) div sys_bits_adr_k;
{
*   Fill in defaults for any unset values.  LO and HI are the min and max
*   values for this data type, and OFS is the memory size for this data type
*   on this machine.
}
  file_currdir_get (line, stat);       {save existing current directory}
  if sys_error(stat) then begin
    sys_stat_set (img_subsys_k, img_err_internal_k, stat);
    sys_stat_parm_int (2, stat);
    sys_stat_parm_str ('IMG_D_INF_OPEN_READ', stat);
    goto abort0;
    end;
  file_currdir_set (dir, stat);        {temp go to directory containing INF file}
  if sys_error(stat) then begin
    sys_stat_set (img_subsys_k, img_err_internal_k, stat);
    sys_stat_parm_int (3, stat);
    sys_stat_parm_str ('IMG_D_INF_OPEN_READ', stat);
    goto abort0;
    end;

  for c := firstof(c) to lastof(c) do begin {once for each pixel component}
    if not comp[c].stride_pix_set then begin
      comp[c].stride_pix := ofs;
      end;
    if not comp[c].stride_scan_set then begin
      comp[c].stride_scan := ofs * d.x_size;
      end;
    if not comp[c].range_set then begin
      comp[c].lo := lo;
      comp[c].hi := hi;
      end;
    make_lut (comp[c].lo, comp[c].hi, d.dform, comp[c].p^.lut); {fill in LUT}
    if comp[c].fnam.len > 0 then begin {there is a real file name for this comp ?}
      string_treename (comp[c].fnam, parm); {translate to full absolute pathname}
      string_copy (parm, comp[c].fnam);
      end;
    end;

  file_currdir_set (line, stat);       {restore original current directory}
  if sys_error(stat) then begin
    sys_stat_set (img_subsys_k, img_err_internal_k, stat);
    sys_stat_parm_int (4, stat);
    sys_stat_parm_str ('IMG_D_INF_OPEN_READ', stat);
    goto abort0;
    end;
{
*   Open the component data files and map them for reading.  The individual
*   component file names have already been translated to full absolute
*   path names.
}
  for c := firstof(c) to lastof(c) do begin {once for each pixel component}
    with comp[c].p^: cd do begin       {CD is component data in INF READ structure}
    if comp[c].fnam.len = 0 then begin {this component always reads default data}
      cd.map_p := nil;                 {indicate not connected to a file}
      cd.adrfile := 0;
      case d.dform of
dform_u8_k,
dform_s8_k: begin                      {data type is 8 bit}
          cd.adr0 := sys_int_adr_t(addr(cd.fixed8));
          end;
dform_u16_k,
dform_s16_k: begin                     {data type is 16 bit}
          cd.adr0 := sys_int_adr_t(addr(cd.fixed16));
          end;
        end;
      cd.dpix := 0;
      cd.dscan := 0;
      next;                            {on to open file for next component}
      end;
    for c2 := firstof(c2) to c do begin {check for this file already opened}
      if c2 = c then exit;             {only check previous components}
      if string_equal(comp[c2].fnam, comp[c].fnam) then begin {same file ?}
        {
        *   The input file for component C is the same as the previously opened
        *   file for component C2.  Set the data for C to point to the existing
        *   file mapping.
        }
        cd.map_p := comp[c2].p^.map_p; {copy pointer to mapped region handle}
        cd.adrfile := comp[c2].p^.adrfile; {copy mem mapped address of file start}
        goto got_file;                 {all done "opening" file for this component}
        end;
      end;
    file_open_map (                    {open file for memory mapping}
      comp[c].fnam, '',                {file name and suffix}
      [file_rw_read_k],                {we only need to read the file}
      cd.conn,                         {returned connection descriptor}
      stat);
    if sys_error(stat) then goto abort1;
    cd.map_p := addr(cd.map);          {indicate file open for this component}

    file_map (                         {map min required region to our address space}
      cd.conn,                         {file connection descriptor}
      0,                               {mapped region start offset from file start}
      file_map_length(cd.conn),        {size of region to map (whole file)}
      [file_rw_read_k],                {we only need to read data in this region}
      univ_ptr(cd.adrfile),            {returned address of mapped region start}
      tlen,                            {length actually mapped}
      cd.map,                          {handle to this mapped region}
      stat);
    if sys_error(stat) then goto abort1;

got_file:                              {done opening file for this component}
    cd.adr0 :=                         {make component address of first image pixel}
      cd.adrfile + comp[c].ofs0 + comp[c].ofscomp;
    cd.dpix := comp[c].stride_pix;
    cd.dscan := comp[c].stride_scan;
    end;                               {done with CD abbreviation}
    end;                               {back to open file for next pixel component}
{
*   Fill in the private IMG library state block.
}
  di_p^.read_scan1 := addr(read_scan1); {fill in private IMG library state block}
  di_p^.read_scan2 := addr(read_scan2);
  di_p^.rewind := addr(read_rewind);
  di_p^.close := addr(read_close);
{
*   Fill in the remaining app-visible IMG library state block.
}
  img.x_size := d.x_size;
  img.y_size := d.y_size;
  img.aspect := img.x_size / img.y_size;
  img.next_y := 0;
  if d.alpha.map_p = nil
    then img.bits_alpha := 0
    else img.bits_alpha := dform_bits;
  if d.red.map_p = nil
    then img.bits_red := 0
    else img.bits_red := dform_bits;
  if d.grn.map_p = nil
    then img.bits_grn := 0
    else img.bits_grn := dform_bits;
  if d.blu.map_p = nil
    then img.bits_blu := 0
    else img.bits_blu := dform_bits;
  img.bits_max :=
    max(img.bits_alpha, img.bits_red, img.bits_grn, img.bits_blu);
  string_vstring (img.file_type, 'inf', 3);
  img.read_write := file_rw_read_k;
  string_copy (conn.tnam, img.tnam);
  string_generic_fnam (img.tnam, '.inf', img.gnam);
  return;                              {normal non-error return point}
{
*   Error exits.
}
err_syntax:                            {INF file syntax error, INF file open}
  sys_stat_set (img_subsys_k, img_err_file_syntax_k, stat);
  sys_stat_parm_int (conn.lnum, stat);
  sys_stat_parm_vstr (conn.tnam, stat);
  goto abort0;
{
*   Error abort exits.  The code above jumps to the appropriate abort point
*   when an error is encountered.  STAT must already be set to indicate the
*   error condition.  The choice of abort point depends on how far the code
*   got, and therefore which resources must be released on an error.  Each
*   abort point is layered on the next lower numbered abort point.
}
abort1:                                {component data files might be open}
  for c := firstof(c) to lastof(c) do begin {once for each component}
    with comp[c].p^: cd do begin       {CD is INF READ data for this component}
      if cd.map_p = addr(cd.map) then begin {this component has an open file ?}
        file_close (cd.conn);          {close the file}
        end;
      end;                             {done with CD abbreviation}
    end;                               {back to check next component for open file}

abort0:                                {private read INF state block allocated}
  if inf_open then begin               {INF file is currently open ?}
    file_close (conn);                 {close the INF file}
    end;
  util_mem_ungrab (di_p^.data_p, di_p^.mem_p^); {deallocate private INF state block}
  end;                                 {done with D abbreviation}
  end;
