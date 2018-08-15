{   Routines that deal with the special image file comment header lines that are
*   machine readable.  These allow arbitrary additional information to be
*   described about the image.  The machine readable image file header comment
*   lines follow these rules:
*
*     1 - Each starts with a keyword followed by a colon.  The keyword must
*         start in column 1, and is case-sensitive.  It may contain blanks.  The
*         keword starts with a upper case letter, and generally contains no more
*         upper case letters.  Parameters follow the colon separated by spaces.
*         If the parameter is a string, then it starts two columns after the
*         colon and extends to the end of the line.
*
*     2 - Keywords that take a fixed number of individual parameters may have
*         a blank and additional characters following the last parameter.  This
*         additional text is only for displaying to a user when listing all the
*         comment lines without special interpretation of the header lines.  The
*         additional text is ignored when interpreting the header lines by
*         machine.
*
*         For example, the keyword "Altitude" has one fixed parameter, which is
*         altitude in meters.  The following is a valid Altitude header line:
*
*           Altitude: 239.2
*
*         While this is precise and unambiguous to a machine, it is usually
*         desirable to provide more information such that the human users need
*         not be familiar with the detailed specification of the Altitude header
*         line:
*
*           Altitude: 239.2 m, 785 ft
*
*         The additional text "m, 785 ft" is separated from the required
*         parameter by a space, so it is ignored when this line is interpreted
*         by machine.
*
*     3 - The first blank comment line or with a blank in column 1 ends the
*         header comments.  Further comments are not interpreted as header lines
*         even if they match a keyword and otherwise follow the syntax rules.
*
*     4 - Header lines with unrecognized keywords or otherwise invalid syntax
*         are silently considered regular comment lines.
}
module img_head;
define img_head_get;
define img_head_close;
%include 'img2.ins.pas';
{
********************************************************************************
*
*   Local subroutine INIT_STRING (STR, MAX)
*
*   Initialize the var string STR.  Its maximum string storage size is MAX.  The
*   string will be set to empty with all characters in the array set to NULLs.
}
procedure init_string (                {init var string}
  out     str: univ string_var_arg_t;  {the var string to initialize}
  in      max: sys_int_machine_t);     {number of characters storage in array}
  val_param; internal;

var
  ii: sys_int_machine_t;

begin
  str.max := max;                      {set max possible string length}
  str.len := 0;                        {set current length}
  for ii := 1 to max do begin          {init each character to NULL}
    str.str[ii] := chr(0);
    end;
  end;
{
********************************************************************************
*
*   Local subroutine INIT_HEAD (HEAD)
*
*   Init all the fields of HEAD to default or benign values except do not
*   allocate any sytem resources.
}
procedure init_head (                  {init comments header descriptor}
  out     head: img_head_t);           {descriptor to initialize}
  val_param; internal;

begin
  head.fieldset := [];
  head.time := sys_clock_from_fp_abs(0.0);
  head.hours_east := 0.0;
  head.iso := 0.0;
  head.exptime := 0.0;
  head.fstop := 0.0;
  head.focal := 0.0;
  head.focal35 := 0.0;
  head.altitude := 0.0;
  head.lat := 0.0;
  head.lon := 0.0;
  head.locrad := 10.0;
  init_string (head.src_manuf, size_char(head.src_manuf.str));
  init_string (head.src_model, size_char(head.src_model.str));
  init_string (head.src_softw, size_char(head.src_softw.str));
  init_string (head.src_host, size_char(head.src_host.str));
  init_string (head.src_user, size_char(head.src_user.str));
  init_string (head.creator, size_char(head.creator.str));
  init_string (head.copyright, size_char(head.copyright.str));
  head.list := false;                  {init to no comment lines list allocated}
  end;
{
********************************************************************************
*
*   Subroutine IMG_HEAD_GET (COMM, HEAD)
*
*   Reads the image comment lines in COMM into HEAD.  The special header lines
*   are interpreted and their values written to fixed fields in HEAD.  All the
*   input comment lines that are not interpreted as special header lines are
*   written to the string list HEAD.COMM.  This will be a editable list.
}
procedure img_head_get (               {read img comments and interpret header fields}
  in out  comm: string_list_t;         {raw list of image comment lines}
  out     head: img_head_t);           {interpreted header fields and remaining comments}
  val_param;

var
  p: string_index_t;                   {input line parse index}
  tk: string_var132_t;                 {scratch token}
  keywords: string_var8192_t;          {list of keywords}
  pick: sys_int_machine_t;             {number of keyword picked from list}
  r1, r2: double;                      {FP parameters}
  time: sys_clock_t;                   {time parameter}
  inhead: boolean;                     {in header}
  stat: sys_err_t;                     {completion status}

label
  commline;
{
********************
*
*   Local subroutine KEYWORD (STR)
*
*   Add the regular string STR as the next keyword in the list of keywords in
*   KEYWORDS.
}
procedure keyword (                    {add keyword to list}
  in      str: string);                {the keyword to add}

var
  k: string_var80_t;                   {var string keyword}

begin
  k.max := size_char(k.str);           {init local var string}

  string_vstring (k, str, size_char(str)); {make var string keyword}
  string_append_token (keywords, k);   {add it to list of keywords}
  end;
{
********************
*
*   Start of IMG_HEAD_GET.
}
begin
  tk.max := size_char(tk.str);         {init local var strings}
  keywords.max := size_char(keywords.str);
{
*   Initialize the header data descriptor.
}
  init_head (head);                    {init but do not allocate resources}
  string_list_init (head.comm, util_top_mem_context);
  head.list := true;                   {indicate COMM list is allocated}
  head.comm.deallocable := true;
{
*   Create the list of header keywords.
}
  keywords.len := 0;

  keyword ('Source equipment manufacturer'); {1}
  keyword ('Source equipment model');  {2}
  keyword ('Source software');         {3}
  keyword ('Exposure seconds');        {4}
  keyword ('F-stop');                  {5}
  keyword ('ISO film speed');          {6}
  keyword ('Altitude');                {7}
  keyword ('Focal length');            {8}
  keyword ('Focal length, actual');    {9}
  keyword ('Focal length, 35mm equivalent'); {10}
  keyword ('Time');                    {11}
  keyword ('Lat/lon');                 {12}
  keyword ('Created by');              {13}
  keyword ('Host computer');           {14}
  keyword ('User');                    {15}
  keyword ('Copyright');               {16}
{
*   Process each of the strings in COMM sequentially.
}
  string_list_pos_start (comm);        {position to before first input line}
  inhead := true;                      {init to in header lines}

  while true do begin                  {back here each new input line}
    string_list_pos_rel (comm, 1);     {advance to the next input line}
    if comm.str_p = nil then exit;     {done with all input lines ?}
    if not inhead then goto commline;  {not in header, regular comment line ?}
    if comm.str_p^.len = 0 then begin  {blank line that ends header ?}
      inhead := false;                 {indicate no longer in header}
      if head.comm.n = 0 then next;
      goto commline;
      end;
    if comm.str_p^.str[1] = ' ' then begin {leading blank char that ends header?}
      inhead := false;
      goto commline;
      end;
    {
    *   Look for first ":" and extract keyword.
    }
    p := 1;                            {init parse index}
    while comm.str_p^.str[p] <> ':' do begin {not found first ":" yet ?}
      p := p + 1;                      {advance to next char}
      if p > comm.str_p^.len then goto commline; {no ":" on this line ?}
      end;
    string_substr (comm.str_p^, 1, p-1, tk); {extract keyword into TK}
    p := p + 1;                        {advance parse index to first unread char}
    if p <= comm.str_p^.len then begin {still within string ?}
      if comm.str_p^.str[p] <> ' ' then goto commline; {not blank following ":" ?}
      end;
    p := p + 1;                        {advance to first parameter character}

    string_tkpick (tk, keywords, pick); {pick the keyword from the list}
    case pick of                       {which keyword is it ?}

1:    begin                            {Source equipment manufacturer: <string>}
        string_substr (comm.str_p^, p, comm.str_p^.len, head.src_manuf);
        end;

2:    begin                            {Source equipment model: <string>}
        string_substr (comm.str_p^, p, comm.str_p^.len, head.src_model);
        end;

3:    begin                            {Source software: <string>}
        string_substr (comm.str_p^, p, comm.str_p^.len, head.src_softw);
        end;

4:    begin                            {Exposure seconds: <seconds>}
        string_token (comm.str_p^, p, tk, stat);
        if sys_error(stat) then goto commline;
        string_t_fp2 (tk, r1, stat);
        if sys_error(stat) then goto commline;
        head.exptime := r1;
        end;

5:    begin                            {F-stop: <fstop>}
        string_token (comm.str_p^, p, tk, stat);
        if sys_error(stat) then goto commline;
        string_t_fp2 (tk, r1, stat);
        if sys_error(stat) then goto commline;
        head.fstop := r1;
        end;

6:    begin                            {ISO film speed: <ISO>}
        string_token (comm.str_p^, p, tk, stat);
        if sys_error(stat) then goto commline;
        string_t_fp2 (tk, r1, stat);
        if sys_error(stat) then goto commline;
        head.iso := r1;
        end;

7:    begin                            {Altitude: <meters>}
        string_token (comm.str_p^, p, tk, stat);
        if sys_error(stat) then goto commline;
        string_t_fp2 (tk, r1, stat);
        if sys_error(stat) then goto commline;
        head.altitude := r1;
        head.fieldset := head.fieldset + [imghead_alt_k];
        end;

8:    begin                            {Focal length: <mm>}
        string_token (comm.str_p^, p, tk, stat);
        if sys_error(stat) then goto commline;
        string_t_fp2 (tk, r1, stat);
        if sys_error(stat) then goto commline;
        head.focal := r1;
        head.focal35 := r1;
        end;

9:    begin                            {Focal length, actual: <mm>}
        string_token (comm.str_p^, p, tk, stat);
        if sys_error(stat) then goto commline;
        string_t_fp2 (tk, r1, stat);
        if sys_error(stat) then goto commline;
        head.focal := r1;
        end;

10:   begin                            {Focal length, 35mm equivalent: <mm>}
        string_token (comm.str_p^, p, tk, stat);
        if sys_error(stat) then goto commline;
        string_t_fp2 (tk, r1, stat);
        if sys_error(stat) then goto commline;
        head.focal35 := r1;
        end;

11:   begin                            {Time: YYYY/MM/DD.hh:mm:ss.ss [<hours east>]}
        string_token (comm.str_p^, p, tk, stat); {get time string}
        if sys_error(stat) then goto commline;
        string_t_time1 (tk, false, time, stat); {convert to time}
        if sys_error(stat) then goto commline;

        string_token (comm.str_p^, p, tk, stat); {try to get hours east offset}
        if not sys_error(stat) then begin {got hours east ?}
          string_t_fp2 (tk, r1, stat); {make FP hours east in R1}
          if sys_error(stat) then goto commline;

          time := sys_clock_add(       {add hours west to make real CUT}
            time, sys_clock_from_fp_rel(-r1 * 3600.0));
          head.hours_east := r1;       {save local time zone offset}
          head.fieldset := head.fieldset + [imghead_heast_k];
          end;

        head.time := time;             {save image time}
        head.fieldset := head.fieldset + [imghead_time_k];
        end;

12:   begin                            {Lat/lon: <deg lat> <deg lon> [<meters>]}
        string_token (comm.str_p^, p, tk, stat); {get latitude degrees}
        if sys_error(stat) then goto commline;
        string_t_fp2 (tk, r1, stat);
        if sys_error(stat) then goto commline;

        string_token (comm.str_p^, p, tk, stat); {get longitude degrees}
        if sys_error(stat) then goto commline;
        string_t_fp2 (tk, r2, stat);
        if sys_error(stat) then goto commline;

        head.lat := r1;
        head.lon := r2;
        head.fieldset := head.fieldset + [imghead_latlon_k];

        string_token (comm.str_p^, p, tk, stat); {get confidence radius in meters}
        if sys_error(stat) then next;
        string_t_fp2 (tk, r1, stat);
        if sys_error(stat) then next;

        head.locrad := r1;
        head.fieldset := head.fieldset + [imghead_locrad_k];
        end;

13:   begin                            {Created by: <string>}
        string_substr (comm.str_p^, p, comm.str_p^.len, head.creator);
        end;

14:   begin                            {Host computer: <string>}
        string_substr (comm.str_p^, p, comm.str_p^.len, head.src_host);
        end;

15:   begin                            {User: <string>}
        string_substr (comm.str_p^, p, comm.str_p^.len, head.src_user);
        end;

16:   begin                            {Copyright: <string>}
        string_substr (comm.str_p^, p, comm.str_p^.len, head.copyright);
        end;

otherwise                              {invalid or unrecognized keyword}
      goto commline;                   {copy as ordinary comment line}
      end;                             {end of keyword cases}
    next;                              {done header line, on to next input line}
{
*   This input line is not a special header line.  Just copy it to the end of
*   ordinary comment lines list.
}
commline:
    head.comm.size := comm.str_p^.len; {set size of comment line to create}
    string_list_line_add (head.comm);  {create the new output line}
    string_copy (comm.str_p^, head.comm.str_p^); {copy the comment line to out list}
    end;                               {back to process next input line}
  end;
{
********************************************************************************
*
*   Subroutine IMG_HEAD_CLOSE (HEAD)
*
*   Release any system resources allocated to the comments header information
*   descriptor HEAD.  Fields are set to default of benign value to the extent
*   possible without attaching any system resources.
}
procedure img_head_close (             {end use of header desc, dealloc resources}
  in out  head: img_head_t);           {descriptor to dealloc resources of}
  val_param;

begin
  if head.list then begin
    string_list_kill (head.comm);      {delete the list of regular comment lines}
    end;
  init_head (head);                    {reset fields that don't require resources}
  end;
