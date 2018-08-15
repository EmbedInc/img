{   Subroutine IMG_CLOSE (IMG, STAT)
*
*   Close an image data stream.  This may be either a direct connection to a file,
*   or a layered connection thru a filter.  In the case of a layered connection,
*   the CLOSE operation is progpagated all the way down.  Any memory allocated
*   using this connection with IMG_MEM_ALLOC will be deallocated.
*
*   IMG is the handle to this image data stream.
*
*   STAT is returned as the completion status code.
}
module img_close;
define img_close;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';

procedure img_close (                  {close an image file}
  in out  img: img_conn_t;             {handle to open image file or connection}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to IMG private connection data}

begin
  data_p := img.data_p;                {make pointer to private IMG data}

  with data_p^: d do begin             {D is IMG library private connection data}
    d.close^ (img, stat);              {close connection}
    util_mem_context_del (d.mem_p);    {deallocate dynamic memory for this conn}
    end;                               {done with D abbreviation}

  sys_mem_dealloc (img.data_p);        {deallocate img library private data block}
  end;
