{   Subroutine IMG_OPEN_READ_FILT (IMG_OLD, FILT_NAME, PARMS, IMG, STAT)
*
*   Connect an image filter to an image data stream.
}
module img_open_read_filt;
define img_open_read_filt;
%include 'img2.ins.pas';

procedure img_open_read_filt (         {open a filter connection for read}
  in      img_old: img_conn_t;         {image stream filter will use as input}
  in      filt_name: string;           {filter name string}
  in      parms: univ string_var_arg_t; {parameter string passed to filter}
  out     img: img_conn_t;             {connection to output of filter}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to private IMG data about connection}
  fl: sys_int_machine_t;               {number of current filter in table}
  flname: string_leafname_t;           {filter name, lower case}
  flnameu: string_leafname_t;          {filter name, upper case}

label
  found_filt, abort;

begin
  flname.max := sizeof(flname.str);    {init var string}

  img_init_conn (img);                 {init IMG and allocate private data}
  data_p := img.data_p;                {get pointer to private connection data}
  with data_p^: d do begin             {D is private data about this connection}

  string_vstring (flname, filt_name, sizeof(filt_name));
  string_downcase (flname);            {lower case filter name}
  string_copy (flname, flnameu);
  string_upcase (flnameu);             {upper case filter name}

  for fl := 1 to n_filters do begin    {once for each filter in table}
    if string_equal(filter[fl].name, flname) {found our filter ?}
      then goto found_filt;
    end;

  sys_stat_set (img_subsys_k, img_err_no_filter_k, stat); {set NO FILTER error code}
  sys_stat_parm_vstr (flnameu, stat);  {filter name is STAT parameter}
  goto abort;
{
*   FL is the number of the filter from the table that matched the FILT_NAME
*   argument.
}
found_filt:                            {jump here when found filter name in table}
  img.x_size := img_old.x_size;        {init new handle with info from old}
  img.y_size := img_old.y_size;
  img.aspect := img_old.aspect;
  img.next_y := img_old.next_y;
  img.bits_alpha := img_old.bits_alpha;
  img.bits_red := img_old.bits_red;
  img.bits_grn := img_old.bits_grn;
  img.bits_blu := img_old.bits_blu;
  img.bits_max := img_old.bits_max;
  string_copy (img_old.file_type, img.file_type);
  string_copy (img_old.gnam, img.gnam);
  string_copy (img_old.tnam, img.tnam);
  string_list_kill (img.comm);
  string_list_copy (img_old.comm, img.comm, d.mem_p^);
  string_list_pos_last (img.comm);
  img.data_p := data_p;

  filter[fl].open_read_proc^ (img_old, parms, img, stat); {try to open filter}
  if sys_error(stat) then goto abort;  {open filter failed ?}
  string_list_pos_start (img.comm);
  return;                              {normal return, no error}

abort:                                 {jump here on error exit, STAT already set}
  util_mem_context_del (d.mem_p);      {deallocate our private mem context}
  sys_mem_dealloc (data_p);            {deallocate our private data block}
  end;                                 {done with D abbreviation}
  end;
