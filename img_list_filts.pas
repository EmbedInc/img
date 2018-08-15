{   Subroutine IMG_LIST_FILTS (RW, NAMES)
*
*   Return a list of all the currently supported image filters matching
*   the read/write requirements in RW.  The string will contain the list
*   of image filter names separated by blanks.  The image filter names
*   will be directly compatable with the FILT_NAME argument to
*   IMG_OPEN_READ_FILT.
}
module img_list_filts;
define img_list_filts;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';

procedure img_list_filts (             {get list of supported filters}
  in      rw: file_rw_t;               {read/write mode asking about}
  in out  names: univ string_var_arg_t); {list of filter name tokens}
  val_param;

var
  i: sys_int_machine_t;                {loop counter}

begin
  names.len := 0;                      {init returned string to empty}

  for i := 1 to n_filters do begin     {once for each active filter}
    if (file_rw_read_k in rw) and (filter[i].open_read_proc = nil)
      then next;
    if (file_rw_write_k in rw) then next;
    if names.len > 0 then begin        {this is not first token added to NAMES ?}
      string_append1 (names, ' ');     {add separating space}
      end;
    string_append (names, filter[i].name); {add token for this filter name}
    end;
  end;
