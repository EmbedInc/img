{   Filter WARP
*
*   This filter can adjust color balance, the gray scale mapping, the black
*   and white levels, and saturation.  Conceptually, there are 3 stages to this
*   filter.  In order, they are a linear transform, non-linear brightness and
*   saturation adjustments, and another linear transform.
*
*   Parameters for stage 1 (input linear transform) are:
*
*     MULT_IN m
*     MULT_IN_RED m
*     MULT_IN_GRN m
*     MULT_IN_BLU m
*
*     ADD_IN a
*     ADD_IN_RED a
*     ADD_IN_GRN a
*     ADD_IN_BLU a
*
*       These parameters together specify an arbitrary linear transform to be applied
*       to the red, green, and blue pixel component values at stage 1 of the filter.
*       The pixel component values are in the 0.0 to 1.0 space.  MULT_IN and
*       ADD_IN set the mapping parameters for all three colors.  These can also be
*       set separately to obtain a crude color balance adjustment.  The overall
*       transformation is:
*
*         <out value> = <in value> * M + A
*
*       The default is to not alter the pixel component values (M = 1, A = 0).
*
*   Parameters for stage 2 (non-linear brightnes add saturation adjust) are:
*
*     BRIGHTEN f
*
*       Brighten or darken the image while not altering the ends of the range.
*       Positive values will brighten the image, while negative values will
*       darken it.  Useful values are typically in the range of -1 to 1.
*       The default is to not alter the image values (F = 0.0).
*
*     SATURATE f
*
*       Adjust the relative saturation of color values.  This adjustment will have
*       no effect on fully desaturated (gray) or fully saturated colors.
*       Intermediate colors will be more saturated when F is positive and less
*       saturated when F is negative.  Useful values are typically in the range
*       of -1 to 1.  The default is to not alter the saturation (F = 0.0).
*
*   Parameters for stage 3 (output linear transform) are:
*
*     MULT_OUT m
*     ADD_OUT a
*
*       These parameters specify an arbitrary linear transform on the red, green,
*       and blue pixel component values.  They work just like the ones for the
*       input linear transformation.  See above.
*
*   All resulting values are clipped to the 0.0 to 1.0 intensity range.
}
module img_filter_warp;
define img_f_warp_open_read;
%include 'img2.ins.pas';

const
  n_cmds = 12;                         {number of possible commands}
  cmd_len_max = 11;                    {size of largest command name}

  cmd_len_alloc = cmd_len_max + 1;     {chars to allocate for each command}
  cmds_len = cmd_len_alloc * n_cmds;   {total number of chars in commands list}

type
  comp_t = record                      {unique data for a single pixel component}
    m1, b1: real;                      {stage 1 linear mapping}
    end;

  warp_read_p_t = ^warp_read_t;
  warp_read_t = record                 {our private data for this connection}
    img_p: img_conn_p_t;               {connection to source image data stream}
    scan_p: img_scan2_arg_p_t;         {pointer to one input scan line}
    red: comp_t;                       {unique parameters for each pixel component}
    grn: comp_t;
    blu: comp_t;
    eb: real;                          {brighten exponent}
    es: real;                          {saturate exponent}
    m3: real;                          {stage 3 multiply factor}
    b3: real;                          {stage 3 add value}
    end;

  cmd_t =                              {one command name}
    array[1..cmd_len_alloc] of char;

  cmds_t = record                      {array of all the command names}
    max: string_index_t;               {simulate a var string}
    len: string_index_t;
    str: array[1..n_cmds] of cmd_t;
    end;

var
  filter_name: string_var32_t :=       {upper case name of this filter}
    [str := 'WARP', len := 4, max := sizeof(filter_name.str)];
  cmds: cmds_t := [                    {all the command names}
    max := cmds_len, len := cmds_len, str := [
      'MULT_IN    ',                   {1}
      'MULT_IN_RED',                   {2}
      'MULT_IN_GRN',                   {3}
      'MULT_IN_BLU',                   {4}
      'ADD_IN     ',                   {5}
      'ADD_IN_RED ',                   {6}
      'ADD_IN_GRN ',                   {7}
      'ADD_IN_BLU ',                   {8}
      'SATURATE   ',                   {9}
      'BRIGHTEN   ',                   {10}
      'MULT_OUT   ',                   {11}
      'ADD_OUT    ',                   {12}
      ]
    ];
{
*   Internal filter entry points.  The external routine for opening the filter
*   is declared in IMG2.INS.PAS, since this filter comes standard with the
*   IMG library.
}
procedure img_f_warp_read_scan2 (      {read a scan line of format 2 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}
  forward;

procedure img_f_warp_rewind (          {rewind so next read is first image pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}
  forward;

procedure img_f_warp_close (           {close this use of filter}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}
  forward;
{
********************************************************************************
*
*   Subroutine IMG_F_WARP_OPEN_READ (IMG_OLD, PARMS, IMG, STAT)
*
*   Open this filter for read operations.  IMG_OLD is the image data stream
*   handle that the incoming data will be read from.  PARMS is a string of
*   parameters for this filter.  See the comment header for this module for an
*   explanation of what may be in PARMS.  IMG is filled in as the connection
*   handle that filtered data my be read from.  STAT is returned as the
*   completion status code.
}
procedure img_f_warp_open_read (       {open WARP filter for read}
  in      img_old: img_conn_t;         {image stream filter will use as input}
  in      parms: univ string_var_arg_t; {parameter string passed to filter}
  out     img: img_conn_t;             {connection to output of filter}
  out     stat: sys_err_t);            {completion status code}

type
  range_t = record                     {descriptor for one set of -RANGE values}
    low: real;                         {input value that should map to 0.0}
    high: real;                        {input value that should map to 1.0}
    end;

var
  mult_in_red, mult_in_grn, mult_in_blu: real; {stage 1 linear transform parameters}
  add_in_red, add_in_grn, add_in_blu: real;
  saturate: real;                      {SATURATE parameter value}
  brighten: real;                      {BRIGHTEN parameter value}
  mult_out, add_out: real;             {stage 3 linear transform parameters}
  range_red: range_t;                  {equivalent stage 1 -RANGE values}
  range_grn: range_t;
  range_blu: range_t;

  p: string_index_t;                   {PARMS parse index}
  p_old: string_index_t;               {saved P value before option parm parsed}
  opt: string_var32_t;                 {option name parsed from PARMS}
  pick: sys_int_machine_t;             {number of token picked from list}
  parm: string_var32_t;                {parameter token for current OPT}
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: warp_read_p_t;          {pointer to IMG driver private data}
  size: sys_int_adr_t;                 {amount of memory to allocate}

  r1: real;                            {scrach number for reading parameters}

label
  next_opt, bad_parm, done_opts;

begin
  opt.max := sizeof(opt.str);          {init var strings}
  parm.max := sizeof(parm.str);
  sys_error_none (stat);               {init to no errors}

  data_p := img.data_p;                {make pointer to IMG library internal data}
  img_mem_alloc (                      {allocate our own private data block}
    img, sizeof(data_read_p^), data_read_p);
  with
      data_p^: d,                      {D is IMG library conn data block}
      data_read_p^: dr                 {DR is WARP filter private conn data block}
      do begin

  d.data_p := data_read_p;             {save pointer to our private data block}
{
*   Init values to defaults before processing PARMS string.
}
  mult_in_red := 1.0;                  {init stage 1 to default}
  mult_in_grn := 1.0;
  mult_in_blu := 1.0;
  add_in_red := 0.0;
  add_in_grn := 0.0;
  add_in_blu := 0.0;
  saturate := 0.0;                     {init stage 2 to default}
  brighten := 0.0;
  mult_out := 1.0;                     {init stage 3 to default}
  add_out := 0.0;
{
*   Process PARMS string.
}
  p := 1;                              {init PARMS parse index}

next_opt:                              {back here each new option name in PARMS}
  string_token (parms, p, opt, stat);  {extract next token from PARMS}
  if string_eos(stat) then goto done_opts; {reached end of PARMS string ?}
  string_upcase (opt);                 {make upper case for token matching}
  string_tkpick (opt, cmds, pick);     {pick token from list of legal keywords}
  p_old := p;                          {save parse index before parameter parsed}
  case pick of
{
*   MULT_IN m
}
1: begin
  string_token_fpm (parms, p, mult_in_red, stat);
  mult_in_grn := mult_in_red;
  mult_in_blu := mult_in_red;
  end;
{
*   MULT_IN_RED m
}
2: begin
  string_token_fpm (parms, p, mult_in_red, stat);
  end;
{
*   MULT_IN_GRN m
}
3: begin
  string_token_fpm (parms, p, mult_in_grn, stat);
  end;
{
*   MULT_IN_BLU m
}
4: begin
  string_token_fpm (parms, p, mult_in_blu, stat);
  end;
{
*   ADD_IN m
}
5: begin
  string_token_fpm (parms, p, add_in_red, stat);
  add_in_grn := add_in_red;
  add_in_blu := add_in_red;
  end;
{
*   ADD_IN_RED m
}
6: begin
  string_token_fpm (parms, p, add_in_red, stat);
  end;
{
*   ADD_IN_GRN m
}
7: begin
  string_token_fpm (parms, p, add_in_grn, stat);
  end;
{
*   ADD_IN_BLU m
}
8: begin
  string_token_fpm (parms, p, add_in_blu, stat);
  end;
{
*   SATURATE f
}
9: begin
  string_token_fpm (parms, p, saturate, stat);
  end;
{
*   BRIGHTEN f
}
10: begin
  string_token_fpm (parms, p, brighten, stat);
  end;
{
*   MULT_OUT m
}
11: begin
  string_token_fpm (parms, p, mult_out, stat);
  end;
{
*   ADD_OUT b
}
12: begin
  string_token_fpm (parms, p, add_out, stat);
  end;
{
*   Unrecognized option name.
}
otherwise
    sys_stat_set (img_subsys_k, img_err_bad_opt_filt_k, stat);
    sys_stat_parm_vstr (opt, stat);
    sys_stat_parm_vstr (filter_name, stat);
    return;                            {return with BAD OPTION NAME error}
    end;                               {done with option name cases}

  if not sys_error(stat) then goto next_opt; {no error processing this option}

bad_parm:                              {jump here on encounter bad command parameter}
  string_token (parms, p_old, parm, stat); {get parameter that caused trouble}
  sys_stat_set (img_subsys_k, img_err_bad_parm_filt_k, stat);
  sys_stat_parm_vstr (parm, stat);
  sys_stat_parm_vstr (opt, stat);
  sys_stat_parm_vstr (filter_name, stat);
  return;                              {return with BAD OPTION PARAMETER error}

done_opts:                             {jump here when done processing PARMS string}
{
*   Done processing PARMS string.
*   Now fill in user-visible and IMG private connection data structures.
}
  img.bits_alpha := min(img_old.bits_alpha, 16);
  img.bits_red := 16;
  img.bits_grn := 16;
  img.bits_blu := 16;
  img.bits_max := 16;

  d.read_scan2 := addr(img_f_warp_read_scan2);
  d.rewind := addr(img_f_warp_rewind);
  d.close := addr(img_f_warp_close);

  string_list_line_add (img.comm);     {create comment line with date}
  sys_date_time1 (img.comm.str_p^);    {init our comment line with date/time string}
  string_appends (img.comm.str_p^, '  Filtered using WARP filter:');

  string_list_line_add (img.comm);     {create comment line for filter parameters}
  string_append1 (img.comm.str_p^, ' ');

  if                                   {stage 1 used ?}
      (mult_in_red <> 1.0) or (add_in_red <> 0.0) or
      (mult_in_grn <> 1.0) or (add_in_grn <> 0.0) or
      (mult_in_blu <> 1.0) or (add_in_blu <> 0.0)
      then begin
    r1 := 1.0 / mult_in_red;
    range_red.low := -add_in_red * r1;
    range_red.high := range_red.low + r1;

    r1 := 1.0 / mult_in_grn;
    range_grn.low := -add_in_grn * r1;
    range_grn.high := range_grn.low + r1;

    r1 := 1.0 / mult_in_blu;
    range_blu.low := -add_in_blu * r1;
    range_blu.high := range_blu.low + r1;

    if
        (range_grn.low = range_red.low) and (range_blu.low = range_red.low) and
        (range_grn.high = range_red.high) and (range_blu.high = range_red.high)
      then begin                       {all three colors are mapped the same}
        string_appends (img.comm.str_p^, ' RANGE');
        string_append1 (img.comm.str_p^, ' ');
        string_f_fp_fixed (parm, range_red.low, 3);
        string_append (img.comm.str_p^, parm);
        string_append1 (img.comm.str_p^, ' ');
        string_f_fp_fixed (parm, range_red.high, 3);
        string_append (img.comm.str_p^, parm);
        end
      else begin                       {the three colors have different mappings}
        string_appends (img.comm.str_p^, ' RANGE_RED');
        string_append1 (img.comm.str_p^, ' ');
        string_f_fp_fixed (parm, range_red.low, 3);
        string_append (img.comm.str_p^, parm);
        string_append1 (img.comm.str_p^, ' ');
        string_f_fp_fixed (parm, range_red.high, 3);
        string_append (img.comm.str_p^, parm);

        string_appends (img.comm.str_p^, ' RANGE_GRN');
        string_append1 (img.comm.str_p^, ' ');
        string_f_fp_fixed (parm, range_grn.low, 3);
        string_append (img.comm.str_p^, parm);
        string_append1 (img.comm.str_p^, ' ');
        string_f_fp_fixed (parm, range_grn.high, 3);
        string_append (img.comm.str_p^, parm);

        string_appends (img.comm.str_p^, ' RANGE_BLU');
        string_append1 (img.comm.str_p^, ' ');
        string_f_fp_fixed (parm, range_blu.low, 3);
        string_append (img.comm.str_p^, parm);
        string_append1 (img.comm.str_p^, ' ');
        string_f_fp_fixed (parm, range_blu.high, 3);
        string_append (img.comm.str_p^, parm);
        end
      ;
    end;                               {done writing stage 1 parameters}

  if saturate <> 0.0 then begin
    string_appends (img.comm.str_p^, ' SATURATE');
    string_append1 (img.comm.str_p^, ' ');
    string_f_fp_fixed (parm, saturate, 2);
    string_append (img.comm.str_p^, parm);
    end;

  if brighten <> 0.0 then begin
    string_appends (img.comm.str_p^, ' BRIGHTEN');
    string_append1 (img.comm.str_p^, ' ');
    string_f_fp_fixed (parm, brighten, 2);
    string_append (img.comm.str_p^, parm);
    end;

  if (mult_out <> 1.0) or (add_out <> 0.0) then begin
    string_appends (img.comm.str_p^, ' OFS_LOW');
    string_append1 (img.comm.str_p^, ' ');
    string_f_fp_fixed (parm, add_out, 3);
    string_append (img.comm.str_p^, parm);
    string_appends (img.comm.str_p^, ' OFS_HIGH');
    string_append1 (img.comm.str_p^, ' ');
    r1 := add_out + mult_out - 1.0;
    string_f_fp_fixed (parm, r1, 3);
    string_append (img.comm.str_p^, parm);
    end;
{
*   Fill in our private persistent state.
}
  dr.img_p := addr(img_old);           {save pointer to source image stream handle}

  size :=                              {amount of mem needed for one input scan line}
    img_old.x_size * sizeof(dr.scan_p^[0]);
  img_mem_alloc (img, size, dr.scan_p); {allocate memory for one input scan line}

  dr.red.m1 := mult_in_red;            {save stage 1 mapping parameters}
  dr.red.b1 := add_in_red;
  dr.grn.m1 := mult_in_grn;
  dr.grn.b1 := add_in_grn;
  dr.blu.m1 := mult_in_blu;
  dr.blu.b1 := add_in_blu;

  dr.eb := 2.0 ** (-brighten);         {exponent resulting from BRIGHTEN}
  dr.es := 2.0 ** saturate;            {exponent resulting from SATURATE}

  dr.m3 := mult_out;                   {save stage 3 mapping parameters}
  dr.b3 := add_out;
  end;                                 {done with D and DR abbreviations}
  end;
{
********************************************************************************
*
*   Subroutine IMG_F_WARP_READ_SCAN2 (IMG, SCAN, STAT)
*
*   Read the next scan line and return the values as format 2 pixels.
}
procedure img_f_warp_read_scan2 (      {read a scan line of format 2 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: warp_read_p_t;          {pointer to IMG driver private data}
  x: sys_int_machine_t;                {current pixel X coordinate}
  red, grn, blu: real;                 {0.0 to 1.0 intermediate component value}
  iten: real;                          {0.0 to 1.0 pixel intensity}
  itene: real;                         {ITEN with brightness exponent applied}

begin
  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_read_p := data_p^.data_p;       {make pointer to our private connection data}
  with
      data_read_p^: dr                 {DR is WARP filter private conn data block}
      do begin

  img_read_scan2 (dr.img_p^, dr.scan_p^, stat); {read scan line from source file}
  if sys_error(stat) then return;

  for x := 0 to img.x_size-1 do begin  {once for each pixel in scan line}
{
*   Perform stage 1 filter.
}
    red := ((dr.scan_p^[x].red / 65535.0) * dr.red.m1) + dr.red.b1;
    grn := ((dr.scan_p^[x].grn / 65535.0) * dr.grn.m1) + dr.grn.b1;
    blu := ((dr.scan_p^[x].blu / 65535.0) * dr.blu.m1) + dr.blu.b1;
{
*   Perform stage 2 filter.
*
*   GIVEN:
*     BRIGHTEN and SATURATE parameters supplied in PARMS.
*     ITEN = 0.0 to 1.0 pixel intensity.  This is the max of the color components.
*     VAL  = 0.0 to 1.0 value of pixel color component to convert.
*
*   FIND:
*     ADJ_VAL = Adjusted 0.0 to 1.0 pixel component value.
*
*
*   EB  =  Warping exponent resulting from BRIGHTEN.
*          EB = 2**(-BRIGHTEN)
*   ES  =  Warping exponent resulting from SATURATE.
*          ES = 2**SATURATE.
*
*                      eb            es
*                  ITEN    (VAL/ITEN)
*   ADJ_VAL = VAL ------- -------------
*                  ITEN    (VAL/ITEN)
*
*   This reduces to:
*
*                 eb             es
*   ADJ_VAL = ITEN   * (VAL/ITEN)
*
*   EB and ES have been pre-computed and saved in DR.
}
    iten := max(red, grn, blu);        {make pixel intensity}
    if iten > 10.0e-6
      then begin                       {intensity is above 0, perform computation}
        itene := iten ** dr.eb;        {make ITEN with exponent applied}
        red := itene * ((red / iten) ** dr.es);
        grn := itene * ((grn / iten) ** dr.es);
        blu := itene * ((blu / iten) ** dr.es);
        end
      else begin                       {intensity is zero}
        red := 0.0;
        grn := 0.0;
        blu := 0.0;
        end
      ;
{
*   Perform stage 3 filter.
}
    red := red * dr.m3 + dr.b3;
    grn := grn * dr.m3 + dr.b3;
    blu := blu * dr.m3 + dr.b3;
{
*   Convert the results back to integer and write them into the output scan
*   line.
}
    scan[x].alpha := dr.scan_p^[x].alpha;
    scan[x].red := trunc(max(0.0, min(0.99999, red)) * 65536.0);
    scan[x].grn := trunc(max(0.0, min(0.99999, grn)) * 65536.0);
    scan[x].blu := trunc(max(0.0, min(0.99999, blu)) * 65536.0);
    end;                               {back and translate next pixel}

  img.next_y := dr.img_p^.next_y;      {update Y coordinate of next scan line}
  end;                                 {done with DR abbreviation}
  end;
{
********************************************************************************
*
*   Subroutine IMG_F_WARP_REWIND (IMG, STAT)
}
procedure img_f_warp_rewind (          {rewind so next read is first image pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: warp_read_p_t;          {pointer to IMG driver private data}

begin
  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_read_p := data_p^.data_p;       {make pointer to our private connection data}
  with
      data_read_p^: dr                 {DR is WARP filter private conn data block}
      do begin

  img_rewind (dr.img_p^, stat);
  img.next_y := 0;

  end;                                 {done with D and DR abbreviations}
  end;
{
********************************************************************************
*
*   Subroutine IMG_F_WARP_CLOSE (IMG,STAT)
}
procedure img_f_warp_close (           {close this use of filter}
  in out  img: img_conn_t;             {handle to connection to close}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG library internal conn data}
  data_read_p: warp_read_p_t;          {pointer to IMG driver private data}

begin
  data_p := img.data_p;                {make pointer to IMG library internal data}
  data_read_p := data_p^.data_p;       {make pointer to our private connection data}
  with
      data_read_p^: dr                 {DR is WARP filter private conn data block}
      do begin

  img_close (dr.img_p^, stat);

  end;                                 {done with D and DR abbreviations}
  end;
