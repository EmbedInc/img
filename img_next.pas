module img_next;
define img_next_write_img;
define img_next_read_img;
define img_end;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';
{
*****************************************************************
*
*   Subroutine IMG_NEXT_READ_IMG (IMG, STAT)
*
*   Close the current image in the image file and open the next one
*   for reading.  The image file must have been originally opened for
*   reading.  If the image file type does not support multiple images
*   per file, or the current image is the last image in the file,
*   then STAT is returned with IMG_ERR_END_IMAGES_K status.  This
*   status can be tested for easily with the function IMG_END.
}
procedure img_next_read_img (          {open next image in already-open file}
  in out  img: img_conn_t;             {connection handle to image open for read}
  out     stat: sys_err_t);            {completion status code}

var
  dat_p: img_conn2_p_t;                {pointer to IMG library private conn state}

begin
  dat_p := img.data_p;                 {get pointer to internal IMG library state}

  if dat_p^.next_read = nil
    then begin                         {driver doesn't support NEXT_READ function}
      sys_stat_set (img_subsys_k, img_err_end_images_k, stat);
      end
    else begin                         {driver does have NEXT_READ entry point}
      dat_p^.next_read^ (img, stat);   {call driver routine}
      end
    ;
  end;
{
*****************************************************************
*
*   Subroutine IMG_NEXT_WRITE_IMG (IMG, STAT)
*
*   Close the current image in the image file and open the next one
*   for writing.  The image file must have been originally opened for
*   write.
}
procedure img_next_write_img (         {open next image in already-open file}
  in out  img: img_conn_t;             {connection handle to image open for write}
  in      x_size, y_size: sys_int_machine_t; {size of image in pixels}
  in      parms: univ string_var_arg_t; {additional format commands}
  out     stat: sys_err_t);            {completion status code}
  val_param;

var
  dat_p: img_conn2_p_t;                {pointer to IMG library private conn state}

begin
  dat_p := img.data_p;                 {get pointer to internal IMG library state}

  if dat_p^.next_write = nil then begin {driver doesn't support NEXT_WRITE ?}
    sys_stat_set (img_subsys_k, img_err_no_nextw_k, stat);
    return;
    end;

  if img.next_y <> img.y_size then begin {not just finished writing curr image ?}
    sys_stat_set (img_subsys_k, img_err_old_ndone_k, stat);
    return;
    end;

  dat_p^.next_write^ (img, x_size, y_size, parms, stat); {call driver routine}
  end;
{
*****************************************************************
*
*   Function IMG_END (STAT)
*
*   Returns TRUE when STAT is indicating the status IMG_ERR_END_IMAGES_K.
*   This indicates that no subsequent image is available in the image
*   file if open for read, or that no subsequent image can be added if
*   open for write.
*
*   When IMG_END returns TRUE, STAT is reset to indicate no error.
}
function img_end (                     {TRUE if no more images available in file}
  in out  stat: sys_err_t)             {status code, reset to no err image file end}
  :boolean;                            {TRUE on end of file status}
  val_param;

begin
  img_end :=
    sys_stat_match (img_subsys_k, img_err_end_images_k, stat) or
    sys_stat_match (img_subsys_k, img_err_no_nextw_k, stat);
  end;
