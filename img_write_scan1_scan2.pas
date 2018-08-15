{   Subroutine IMG_WRITE_SCAN1_SCAN2 (IMG, SCAN, STAT)
*
*   Write a format 1 scan line by converting to format 2 scan line and writing
*   it directly.  This routine is automatically installed when an image write
*   driver does not support format 2.
}
module img_write_scan1_scan2;
define img_write_scan1_scan2;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';

procedure img_write_scan1_scan2 (      {write fmt 1 scan by converting to fmt 2}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan1_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to private IMG data about connection}
  x: sys_int_machine_t;                {pixel X coordinate}

begin
  data_p := img.data_p;                {get pointer to private connection data}

  if data_p^.scan2_p = nil then begin  {scan line buffer not previously allocated ?}
    img_alloc_scan2 (data_p^);         {allocate the scan line buffer}
    end;

  for x := 0 to img.x_size-1 do begin  {once for each pixel in scan line}
    data_p^.scan2_p^[x].alpha := lshft(scan[x].alpha, 8) ! scan[x].alpha;
    data_p^.scan2_p^[x].red := lshft(scan[x].red, 8) ! scan[x].red;
    data_p^.scan2_p^[x].grn := lshft(scan[x].grn, 8) ! scan[x].grn;
    data_p^.scan2_p^[x].blu := lshft(scan[x].blu, 8) ! scan[x].blu;
    end;

  data_p^.write_scan2^ (img, data_p^.scan2_p^, stat); {write converted scan line}
  end;
