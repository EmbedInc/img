{   Subroutine IMG_OPEN_READ_IMG (FNAM, IMG, STAT)
*
*   Open an image file for read.  FNAM is the image file name.  If it ends in
*   .<image file type>, then only that file type is tried.  If it does not end
*   that way then each file type is tried in turn, with the suffix for that file
*   type appended.  The image file type suffixes are the image file type name
*   in lower case, preceded by a period.  IMG is returned as the user handle
*   to this new connection.  STAT is the returned completion status code.
}
module img_open_read_img;
define img_open_read_img;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';

procedure img_open_read_img (          {open an image file for read}
  in      fnam: univ string_var_arg_t; {name of image file to open}
  out     img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to private IMG data about connection}
  dr: sys_int_machine_t;               {number of current driver}

label
  not_found, abort, opened;

begin
  img_init_conn (img);                 {init IMG and allocate private data}
  data_p := img.data_p;                {get pointer to private connection data}
  with data_p^: d do begin             {D is private data about this connection}

  img_find_driver (fnam, dr);          {check for specific driver indicated}
{
*   Handle case where suffix of FNAM indicates a specific driver.
}
  if dr <> 0 then begin                {FNAM indicates a specific driver ?}
    if driver[dr].open_read_proc = nil then goto not_found; {driver can't read ?}
    driver[dr].open_read_proc^ (fnam, img, stat); {call driver OPEN READ routine}
    if sys_error(stat) then goto abort;
    goto opened;                       {driver open has been done}
    end;
{
*   The suffix of FNAM did not indicate any specific driver.  Try all the
*   drivers in turn until one of them successfully opens the file.
}
  for dr := 1 to n_drivers do begin    {once for each driver}
    if driver[dr].open_read_proc = nil then next; {this driver not really here ?}
    driver[dr].open_read_proc^ (fnam, img, stat); {call driver OPEN READ routine}
    if not sys_error(stat) then goto opened; {OPEN READ was successful ?}
    if not file_not_found(stat) then goto abort; {other than just file not found ?}
    end;                               {back and try next driver in list}

not_found:
  sys_stat_set (file_subsys_k, file_stat_not_found_k, stat);
  sys_stat_parm_vstr (fnam, stat);
  sys_stat_parm_vstr (string_v(''(0)), stat);
{
*   Abort with error.  STAT is already set to the appropriate error code.
*   We must deallocate our private data block before leaving.
}
abort:                                 {jump here to abort with error}
  util_mem_context_del (d.mem_p);      {delete memory context for this connection}
  sys_mem_dealloc (data_p);            {deallocate our private data block}
  return;                              {return with error}
{
*   The driver OPEN READ call completed without error.
}
opened:                                {jump here after driver OPEN READ with no err}
  string_list_pos_start (img.comm);    {re-position to before start of comments}
  end;                                 {done with D abbreviation}
  end;
