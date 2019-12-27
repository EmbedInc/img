{   Subroutine IMG_REWIND (IMG, STAT)
*
*   Rewind the image read stream so that the next pixel to be read is the first
*   pixel in the image.  IMG is the handle to the image stream, which must be
*   open for READ.  STAT is the completion status code.
}
module img_rewind;
define img_rewind;
%include 'img2.ins.pas';

procedure img_rewind (                 {rewind image file to just before first pixel}
  in out  img: img_conn_t;             {user handle to this image file}
  out     stat: sys_err_t);            {error return code}

var
  data_p: img_conn2_p_t;               {pointer to private data about this stream}

begin
  data_p := img.data_p;                {make pointer to our private data}
  data_p^.rewind^ (img, stat);         {call driver routine to do the work}
  end;
