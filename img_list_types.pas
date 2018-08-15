{   Subroutine IMG_LIST_TYPES (RW, NAMES)
*
*   Return a list of all the image file formats supported for the read/write
*   access indicated by RW.
*
*   The string will contain the list of image file types separated by
*   blanks.  The image file type names will be directly compatable with
*   the FILE_TYPE argument to IMG_OPEN_WRITE_IMG.  This is the file
*   name suffix without the preceding ".".  For example, file type name
*   for Cognivision image files is "img", and for Truevision targa
*   files is "tga".
}
module img_list_types;
define img_list_types;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';

procedure img_list_types (             {get list of supported image file types}
  in      rw: file_rw_t;               {read/write mode asking about}
  in out  names: univ string_var_arg_t); {list of file suffix tokens}
  val_param;

var
  i: sys_int_machine_t;                {loop counter}

begin
  names.len := 0;                      {init returned string to empty}

  for i := 1 to n_drivers do begin     {once for each active driver}
    if (file_rw_read_k in rw) and (driver[i].open_read_proc = nil)
      then next;
    if (file_rw_write_k in rw) and (driver[i].open_write_proc = nil)
      then next;
    if names.len > 0 then begin        {this is not first token added to NAMES ?}
      string_append1 (names, ' ');     {add separating space}
      end;
    string_append (names, driver[i].name); {add token for this driver name}
    end;
  end;
