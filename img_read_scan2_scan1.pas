module img_read_scan2_scan1;
define img_read_scan2_scan1;
define img_alloc_scan1;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';
{
*****************************************************************************
*
*   Subroutine IMG_READ_SCAN2_SCAN1 (IMG, SCAN, STAT)
*
*   Pass back a format 2 scan line by converting from a format 1 scan line.
*   This routine is automatically installed when an image read driver does
*   not support format 2.
}
procedure img_read_scan2_scan1 (       {read fmt 2 scan by converting from fmt 1}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan2_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to private IMG data about connection}
  x: sys_int_machine_t;                {pixel X coordinate}

begin
  data_p := img.data_p;                {get pointer to private connection data}

  if data_p^.scan1_p = nil then begin  {scan line buffer not previously allocated ?}
    img_alloc_scan1 (data_p^);         {allocate the scan line buffer}
    end;

  data_p^.read_scan1^ (img, data_p^.scan1_p^, stat); {read format 1 scan line}

  for x := 0 to img.x_size-1 do begin  {once for each pixel in scan line}
    scan[x].alpha := lshft(data_p^.scan1_p^[x].alpha, 8) ! data_p^.scan1_p^[x].alpha;
    scan[x].red := lshft(data_p^.scan1_p^[x].red, 8) ! data_p^.scan1_p^[x].red;
    scan[x].grn := lshft(data_p^.scan1_p^[x].grn, 8) ! data_p^.scan1_p^[x].grn;
    scan[x].blu := lshft(data_p^.scan1_p^[x].blu, 8) ! data_p^.scan1_p^[x].blu;
    end;
  end;
{
*****************************************************************************
*
*   Subroutine IMG_ALLOC_SCAN1 (CONN2)
*
*   Allocate the format 1 scan line buffer and set CONN2.SCAN1_P pointing
*   to its start.  This routine should not be called unless CONN2.SCAN1_P
*   is NIL, although this is not checked.
}
procedure img_alloc_scan1 (            {allocate the format 1 scan line buffer}
  in out  conn2: img_conn2_t);         {internal image I/O connection}
  val_param;

begin
  util_mem_grab (                      {allocate memory}
    conn2.img_p^.x_size * sizeof(conn2.scan1_p^[0]), {amount of memory to allocate}
    conn2.mem_p^,                      {parent memory context}
    false,                             {will not need to individually deallocate}
    conn2.scan1_p);                    {returned pointer to the new memory}
  end;
