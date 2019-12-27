{   Subroutine IMG_MEM_ALLOC (IMG, SIZE, P)
*
*   Allocate dynamic memory that will be associated with a particular image
*   connection.  This memory will be automatically deallcated when the connection
*   is closed.
*
*   IMG is the connection handle.
*
*   SIZE is the amount of memory to allocate in machine address increments.
*
*   P is returned pointing to the first address of the newly allocated memory.
}
module img_mem_alloc;
define img_mem_alloc;
%include 'img2.ins.pas';

procedure img_mem_alloc (              {allocate memory associated with connection}
  in out  img: img_conn_t;             {handle to established image data stream}
  in      size: sys_int_adr_t;         {amount of memory to allocate}
  out     p: univ_ptr);                {pointer to start of new memory}
  val_param;

var
  data_p: img_conn2_p_t;               {points to IMG private connection data}

begin
  data_p := img.data_p;                {make pointer to private data for this conn}
  util_mem_grab (size, data_p^.mem_p^, true, p);
  end;
