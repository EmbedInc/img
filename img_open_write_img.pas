{   Subroutine IMG_OPEN_WRITE_IMG (FNAM, ASPECT, X_SIZE, Y_SIZE, FILE_TYPE,
*     FILE_FMT, COMM, IMG, STAT)
*
*   Open an image file for write.  The call arguments are:
*
*   FNAM  -  Image file name.  This will have the FILE_TYPE name (same as the
*     driver name) appended if it doesn't already.  All image files created with
*     the IMG library end in .<file type>.
*
*   ASPECT  -  Width/height of the image when it is properly displayed.
*
*   X_SIZE, Y_SIZE  -  Image size in pixels.
*
*   FILE_TYPE  -  The image file type name.  This is the same as the driver
*     name.  The image output file will end in .<file type name>.
*
*   FILE_FMT  -  Specific file format information.  This is an arbitrary string,
*     and is interpreted differently for each driver.  This string is just passed
*     to the driver.
*
*   COMM  -  Strings list handle for all the comment lines.  The comment
*     lines will be written to the output file if the file type can support this.
*
*   IMG  -  Returned connection handle to newly opened image stream.
*
*   STAT  -  Returned completion status code.
}
module img_open_write_img;
define img_open_write_img;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';

procedure img_open_write_img (         {open an image file for write}
  in      fnam: univ string_var_arg_t; {name of image file to open}
  in      aspect: real;                {width/height of properly displayed image}
  in      x_size, y_size: sys_int_machine_t; {size of image in pixels}
  in      file_type: string;           {driver name (IMG, TGA, etc.)}
  in      file_fmt: univ string_var_arg_t; {format string, unique to each FILE_TYPE}
  in      comm: string_list_t;         {descriptor for chain of comment lines}
  out     img: img_conn_t;             {handle to new image file connection}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  s: string_treename_t;                {scratch file name}
  ext: string_var80_t;                 {file name extension name}
  data_p: img_conn2_p_t;               {points to private connection data}
  dr: sys_int_machine_t;               {number of driver from list}

label
  abort;

begin
  s.max := sizeof(s.str);              {init var strings}
  ext.max := sizeof(ext.str);

  img_init_conn (img);                 {init conn handle, allocate private data}
  data_p := img.data_p;                {get pointer to private connection data}
  with data_p^: d do begin             {D is private data about this connection}
{
*   Set the FILE_TYPE field in IMG, the image connection handle.  This is
*   set explicitly from the FILE_TYPE argument if it is not the null string.
*   In that case, look at the file name suffix for a match with any of the
*   current driver names.  If that fails, default to IMG.
}
  string_vstring (img.file_type, file_type, sizeof(file_type)); {make driver name}
  if img.file_type.len <= 0 then begin {FILE_TYPE not explicitly selected ?}
    img_find_driver (fnam, dr);        {get driver ID if implied by file name}
    if dr = 0
      then begin                       {FNAM doesn't imply an image file type}
        string_vstring (img.file_type, 'img', 3); {default to IMG file type}
        end
      else begin                       {FNAM already ended in image file type}
        string_copy (driver[dr].name, img.file_type); {select driver from file name}
        end
      ;
    end;                               {done handling default image file type}
  string_downcase (img.file_type);     {driver names are stored in lower case}
{
*   IMG.FILE_TYPE is set to the image file type to use.
*   Now look thru the driver table to find the driver that matches that file
*   type.
}
  for dr := 1 to n_drivers do begin    {once for each known driver}
    if driver[dr].open_write_proc = nil then next; {this driver can't write ?}
    if not string_equal(driver[dr].name, img.file_type) then next; {not this driver ?}

    ext.len := 1;                      {file name extension always starts with "."}
    ext.str[1] := '.';
    string_append (ext, img.file_type); {make file name extension string}
    string_fill (ext);

    img.aspect := aspect;              {fill in user connection handle}
    img.x_size := x_size;
    img.y_size := y_size;
    img.next_y := 0;
    string_generic_fnam (fnam, ext.str, img.gnam);
    string_fnam_extend (fnam, ext.str, s);
    string_treename (s, img.tnam);
    string_list_kill (img.comm);       {deallocate previous strings list}
    string_list_copy (comm, img.comm, d.mem_p^); {copy comment lines}

    driver[dr].open_write_proc^ (img, file_fmt, stat); {do driver OPEN WRITE operation}
    if sys_error(stat) then goto abort; {error opening image output file ?}
    return;                            {normal return, all went well}
    end;                               {back and try next driver in list}
{
*   None of the drivers in the list matched with the FILE_TYPE argument.
}
  sys_stat_set (img_subsys_k, img_err_no_driver_k, stat); {indicate driver not found}
  sys_stat_parm_vstr (img.file_type, stat); {pass driver name as parameter}

abort:                                 {jump here on error, STAT all set}
  util_mem_context_del (d.mem_p);      {delete memory context for this connection}
  sys_mem_dealloc (data_p);            {deallocate our private data block}
  end;                                 {done with D abbreviation}
  end;
