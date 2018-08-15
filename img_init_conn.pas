{   Subroutine IMG_INIT_CONN (IMG)
*
*   Init a user image connection handle.  All the MAX fields of the var strings
*   are set, etc.  The IMG library private data block for this connection is
*   allocated and the DATA_P pointer filled in.
}
module img_init_conn;
define img_init_conn;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';

procedure img_init_conn (              {init user conn, allocates private block}
  out     img: img_conn_t);            {properly initialized connection handle}

var
  data_p: img_conn2_p_t;               {pointer to private IMG data about connection}

begin
  sys_mem_alloc (sizeof(data_p^), data_p); {allocate IMG library private conn data}
  sys_mem_error (data_p, '', '', nil, 0);
  with data_p^:d do begin              {D is abbreviation for private data block}
  util_mem_context_get (util_top_mem_context, d.mem_p); {create our mem context}
{
*   Init user connection handle.
}
  img.x_size := 0;
  img.y_size := 0;
  img.aspect := 0.0;
  img.next_y := 0;
  img.bits_alpha := 0;
  img.bits_red := 8;
  img.bits_grn := 8;
  img.bits_blu := 8;
  img.bits_max := 8;
  img.file_type.max := sizeof(img.file_type.str);
  img.file_type.len := 0;
  img.gnam.max := sizeof(img.gnam.str);
  img.gnam.len := 0;
  img.tnam.max := sizeof(img.tnam.str);
  img.tnam.len := 0;
  string_list_init (img.comm, d.mem_p^); {init comment lines list to empty}
  img.data_p := data_p;
{
*   Init private connection data.
}
  d.data_p := nil;
  d.read_scan1 := addr(img_read_scan1_scan2);
  d.read_scan2 := addr(img_read_scan2_scan1);
  d.rewind := nil;
  d.write_scan1 := addr(img_write_scan1_scan2);
  d.write_scan2 := addr(img_write_scan2_scan1);
  d.next_read := nil;
  d.next_write := nil;
  d.close := nil;
  d.scan1_p := nil;
  d.scan2_p := nil;
  d.img_p := addr(img);

  end;                                 {done with D abbreviation}
  end;
