module img_read_scan1_scan2;
define img_read_scan1_scan2;
define img_alloc_scan2;
%include 'img2.ins.pas';
{
*****************************************************************************
*
*   Subroutine IMG_READ_SCAN1_SCAN2 (IMG, SCAN, STAT)
*
*   Pass back a format 1 scan line by converting from a format 2 scan line.
*   This routine is automatically installed when an image read driver does
*   not support format 1.
}
procedure img_read_scan1_scan2 (       {read fmt 1 scan by converting from fmt 2}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to private IMG data about connection}
  x: sys_int_machine_t;                {pixel X coordinate}

begin
  data_p := img.data_p;                {get pointer to private connection data}

  if data_p^.scan2_p = nil then begin  {scan line buffer not previously allocated ?}
    img_alloc_scan2 (data_p^);         {allocate the scan line buffer}
    end;

  data_p^.read_scan2^ (img, data_p^.scan2_p^, stat); {read format 2 scan line}

  for x := 0 to img.x_size-1 do begin  {once for each pixel in scan line}
    scan[x].alpha := rshft(data_p^.scan2_p^[x].alpha, 8);
    scan[x].red := rshft(data_p^.scan2_p^[x].red, 8);
    scan[x].grn := rshft(data_p^.scan2_p^[x].grn, 8);
    scan[x].blu := rshft(data_p^.scan2_p^[x].blu, 8);
    end;
  end;
{
*****************************************************************************
*
*   Subroutine IMG_ALLOC_SCAN2 (CONN2)
*
*   Allocate the format 2 scan line buffer and set CONN2.SCAN2_P pointing
*   to its start.  This routine should not be called unless CONN2.SCAN2_P
*   is NIL, although this is not checked.
}
procedure img_alloc_scan2 (            {allocate the format 2 scan line buffer}
  in out  conn2: img_conn2_t);         {internal image I/O connection}
  val_param;

begin
  util_mem_grab (                      {allocate memory}
    conn2.img_p^.x_size * sizeof(conn2.scan2_p^[0]), {amount of memory to allocate}
    conn2.mem_p^,                      {parent memory context}
    false,                             {will not need to individually deallocate}
    conn2.scan2_p);                    {returned pointer to the new memory}
  end;
