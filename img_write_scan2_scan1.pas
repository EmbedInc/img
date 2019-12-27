{   Subroutine IMG_WRITE_SCAN2_SCAN1 (IMG, SCAN, STAT)
*
*   Write a format 2 scan line by converting to format 1 scan line and writing
*   it directly.  This routine is automatically installed when an image write
*   driver does not support format 2.
}
module img_write_scan2_scan1;
define img_write_scan2_scan1;
%include 'img2.ins.pas';

procedure img_write_scan2_scan1 (      {write fmt 2 scan by converting to fmt 1}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan2_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to private IMG data about connection}
  x: sys_int_machine_t;                {pixel X coordinate}

begin
  data_p := img.data_p;                {get pointer to private connection data}

  if data_p^.scan1_p = nil then begin  {scan line buffer not previously allocated ?}
    img_alloc_scan1 (data_p^);         {allocate the scan line buffer}
    end;

  for x := 0 to img.x_size-1 do begin  {once for each pixel in scan line}
    data_p^.scan1_p^[x].alpha := rshft(scan[x].alpha, 8);
    data_p^.scan1_p^[x].red := rshft(scan[x].red, 8);
    data_p^.scan1_p^[x].grn := rshft(scan[x].grn, 8);
    data_p^.scan1_p^[x].blu := rshft(scan[x].blu, 8);
    end;

  data_p^.write_scan1^ (img, data_p^.scan1_p^, stat); {write converted scan line}
  end;
