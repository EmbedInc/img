module img_parms;
define img_parms_read;
define img_parms_skip;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';
{
*****************************************************************
*
*   Subroutine IMG_PARMS_READ (PSTR, PARMS, STAT)
*
*   Read and process the parameters in the string PSTR.  This is the
*   parameter string passed to a driver when opening an image file
*   for write.  The standard parameters are interpreted and their
*   values set in PARMS.  Each explicitly set parameter (as apposed
*   to defaulted) has its element set in SET.  All unrecognized option
*   keywords and their parameters are put into the REM string for
*   processing by the driver code.
*
*   The PSTR string contains option keywords, each of which may be followed
*   by zero or more parameter tokens.  Option keywords start with
*   a dash (-) followed by a letter (A-Z).  All keywords are case-insensitive.
*   Parameters to options never start with a dash followed by a letter.
*   For backwards compatibility, the option keywords -ALPHA, -RED, -GREEN,
*   and -BLUE may have their leading dashes omitted if they are not preceeded
*   by any option that does start with a dash.
*
*   All values in PARMS that are not explicitly set by an option, are
*   set to default values.  Note that each driver may chose different
*   defaults, or ignore the values altogether.
*
*   The "standard" option keywords are:
*
*     -ALPHA bits
*
*       Requests the transparency resolution in bits per pixel.  The default
*       is 0, meaning the transparency component is not requested (all pixels
*       are fully opaque).
*
*     -RED bits
*     -GREEN bits
*     -BLUE bits
*
*       Requests the resolution in bits per pixel for each of the color
*       components.  The default is 8.
*
*     -ANIM
*
*       Indicates that the file is intended to contain a sequence of
*       images forming an animation.  The default is to not assume this.
*
*     -LOOP
*
*       The animation is an infinite loop.  Implies -ANIM.
*
*     -RATE fps
*
*       Set the animation rate in frames per second.  The default is 10.
*
*     -GRAY
*
*       Indicates that the image data will be gray scale, meaning that the
*       red, green, and blue values are always the same within each pixel.
*       Results are undefined if non-gray data is sent.  The driver is not
*       obligated to convert to gray scale, and may assume the data will
*       be gray scale if this options is is used.  Some image file formats
*       allow gray scale images to be stored more compactly or with higher
*       fidelity than color images.  The -GRAY option allows the driver to
*       use these alternate formats.
*
*     -QUAL qual
*
*       Set the quality/compression tradeoff when applicable.  QUAL is the
*       desired quality on a 0.0 to 100.0 scale.  100 means to write the
*       image as lossless as possible, while 0 means to compress as much
*       as possible.  The scale is not necessarily meant to be linear, just
*       monotonic.  The default is 100, meaning to minimize data loss due
*       to compression.
}
procedure img_parms_read (             {read and process standard write open parms}
  in      pstr: univ string_var_arg_t; {write parameters string}
  out     parms: img_parms_t;          {returned interpreted parameter info}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  i: sys_int_machine_t;                {scratch integer and loop counter}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  opt: string_var32_t;                 {option name keyword}
  p: string_index_t;                   {string parse index}
  parm: string_var8192_t;              {parameter to option in OPT}
  prevdash: boolean;                   {TRUE if previous option had dash}
  dash: boolean;                       {TRUE if OPT had leading dash}

label
  loop_opt, got_opt, loop_parm, done_opt, badopt, done_opts;
{
********************************************
*
*   Start of main routine.
}
begin
  opt.max := sizeof(opt.str);          {init local var strings}
  parm.max := sizeof(parm.str);
  sys_error_none (stat);               {init to no error encountered}
{
*   Init PARMS to defaults before processing input string.
}
  parms.expl := [];                    {no values explicitly set}
  parms.bits_alpha := 0;               {no transparency requested}
  parms.bits_red := 8;
  parms.bits_grn := 8;
  parms.bits_blu := 8;
  parms.qual := 1.0;                   {init to maximum quality, lowest compression}
  parms.rate := 10.0;                  {frames per second}
  parms.anim := false;                 {this is not an animation}
  parms.loop := false;                 {animation is once-thru, not infinite loop}
  parms.gray := false;                 {color, not gray scale}
  parms.rem.max := sizeof(parms.rem.str); {no unrecognized options}
  parms.rem.len := 0;
{
*   Read and process each of the options in PSTR.
}
  p := 1;                              {init PSTR parse index}
  prevdash := false;                   {no previous option started with dash}

loop_opt:                              {back here for each new option keyword}
  string_token (pstr, p, opt, stat);   {get next option name keyword}
  if string_eos(stat) then goto done_opts; {hit end of input string ?}
  if sys_error(stat) then return;
  dash := (opt.len >= 1) and (opt.str[1] = '-'); {TRUE on OPT has leading dash}
  if dash then begin                   {remove leading dash if present}
    for i := 1 to opt.len-1 do begin   {move each character back one column}
      opt.str[i] := opt.str[i+1];
      end;
    opt.len := opt.len - 1;            {one less character long after dash removed}
    end;
got_opt:                               {option keyword in OPT, dash removed}
  string_upcase (opt);                 {make upper case for keyword matching}
  if prevdash and (not dash) then goto badopt; {must have dash if previous did}
  prevdash := prevdash or dash;        {update previous option with dash flag}
  string_tkpick80 (opt,                {pick option name from list}
    'ALPHA RED GREEN BLUE ANIM GRAY LOOP RATE QUAL',
    pick);
  if (not dash) and (i > 4) then begin {no dash and it not a compatibility option ?}
    goto badopt;                       {only old options may have dash omitted}
    end;
  sys_error_none (stat);               {init to no parameter error}
  case pick of                         {which option keyword is it}
{
*   ALPHA bits
}
1: begin
  string_token_int (pstr, p, parms.bits_alpha, stat);
  parms.expl := parms.expl + [img_parm_alpha_k];
  end;
{
*   RED bits
}
2: begin
  string_token_int (pstr, p, parms.bits_red, stat);
  parms.expl := parms.expl + [img_parm_red_k];
  end;
{
*   GREEN bits
}
3: begin
  string_token_int (pstr, p, parms.bits_grn, stat);
  parms.expl := parms.expl + [img_parm_grn_k];
  end;
{
*   BLUE bits
}
4: begin
  string_token_int (pstr, p, parms.bits_blu, stat);
  parms.expl := parms.expl + [img_parm_blu_k];
  end;
{
*   ANIM
}
5: begin
  if not dash then goto badopt;
  parms.anim := true;
  parms.expl := parms.expl + [img_parm_anim_k];
  end;
{
*   GRAY
}
6: begin
  if not dash then goto badopt;
  parms.gray := true;
  parms.expl := parms.expl + [img_parm_gray_k];
  end;
{
*   LOOP
}
7: begin
  if not dash then goto badopt;
  parms.loop := true;
  parms.anim := true;
  parms.expl := parms.expl + [img_parm_loop_k, img_parm_anim_k];
  end;
{
*   RATE fps
}
8: begin
  if not dash then goto badopt;
  string_token_fpm (pstr, p, parms.rate, stat);
  parms.expl := parms.expl + [img_parm_rate_k];
  end;
{
*   QUAL quality
}
9: begin
  if not dash then goto badopt;
  string_token_fpm (pstr, p, parms.qual, stat);
  if sys_error(stat) then goto done_opt;
  parms.qual := parms.qual / 100.0;    {convert to 0.0 to 1.0 scale}
  parms.expl := parms.expl + [img_parm_qual_k];
  end;
{
*   This option keyword is not recognized.  Save it and any parameters
*   following it in REM.
}
otherwise
    string_vstring (parm, '-', 1);     {reconstruct whole option name with dash}
    string_append (parm, opt);
    string_append_token (parms.rem, parm); {append option name keyword to REM}

loop_parm:                             {back here each new parameter to this option}
    string_token (pstr, p, parm, stat); {get next token from input string}
    if string_eos(stat) then goto done_opts; {hit end of input string ?}
    if sys_error(stat) then return;
    if                                 {this token is really next option keyword ?}
        (parm.len >= 2) and            {at least two characters long ?}
        (parm.str[1] = '-') and        {first character is dash ?}
        ( ((parm.str[2] >= 'a') and (parm.str[2] <= 'z')) or {lower case letter ?}
          ((parm.str[2] >= 'A') and (parm.str[2] <= 'Z'))) {upper case letter ?}
        then begin
      string_substr (                  {copy part after dash into OPT}
        parm,                          {string to extract substring from}
        2, parm.len,                   {start/end substring indicies}
        opt);                          {extracted string is placed here}
      dash := true;                    {this option definately started with a dash}
      goto got_opt;                    {back and process as regular option keyword}
      end;
    string_append_token (parms.rem, parm); {append this parameter token to REM}
    goto loop_parm;                    {back for next parameter token}
    end;                               {end of option keyword cases}
{
*   All done processing a recognized option keyword.  STAT is set on
*   any error reading or interpreting a parameter.
}
done_opt:                              {jump here on done processing curr option}
  if string_eos(stat) then begin       {hit end of string when expecting parameter ?}
    sys_stat_set (img_subsys_k, img_err_fmt_parm_miss_k, stat); {parm missing}
    sys_stat_parm_vstr (opt, stat);
    return;
    end;

  if sys_error(stat) then begin        {error on dealing with a parameter ?}
    sys_stat_set (img_subsys_k, img_err_fmt_parm_k, stat); {parameter error}
    sys_stat_parm_vstr (opt, stat);
    return;
    end;

  goto loop_opt;                       {back for next input string option keyword}
{
*   Syntax error in getting option OPT.
}
badopt:
  sys_stat_set (img_subsys_k, img_err_fmt_parm_k, stat);
  sys_stat_parm_vstr (opt, stat);
  return;
{
*   Hit end of input string when looking for next option.
}
done_opts:
  end;
{
*****************************************************************
*
*   Subroutine IMG_PARMS_SKIP (PSTR, P)
*
*   Skip over the next parameters in the driver format string PSTR.  P is the
*   current parse index into PSTR.  P will be advanced to skip over parameter
*   tokens, as apposed to command tokens.  The next token parsed after this
*   call will either be the next command keyword, or will result in an end
*   of string status.
*
*   This routine is used by drivers when an unrecognized format string is
*   command is encountered.  Drivers should ignore any unrecognized commands.
*   This routine provides an easy means for skipping over the parameters for
*   the unrecognized command.
}
procedure img_parms_skip (             {skip over PARMS parameters at curr position}
  in      pstr: univ string_var_arg_t; {driver PARMS string}
  in out  p: string_index_t);          {skip all parameters starting here, updated}
  val_param;

var
  pst: string_index_t;                 {saved P when last token was parsed}
  tk: string_var32_t;                  {token parsed from PSTR}
  stat: sys_err_t;                     {completion status code}

label
  loop;

begin
  tk.max := sizeof(tk.str);            {init local var string}

  pst := p;                            {init local parse index}

loop:                                  {back here to parse each new token from PSTR}
  string_token (pstr, pst, tk, stat);  {try to parse another token from PSTR}
  if sys_error(stat) then return;      {assume hit end of string ?}
  string_upcase (tk);
  if                                   {this is a parameter, not command, token ?}
      (tk.len < 2) or                  {commands always start with "-" and letter}
      (tk.str[1] <> '-') or            {first char is not dash ?}
      (tk.str[2] < 'A') or (tk.str[2] > 'Z') {second char is not a letter ?}
      then begin
    p := pst;                          {skip over this token}
    goto loop;                         {back to try next token}
    end;
  end;
