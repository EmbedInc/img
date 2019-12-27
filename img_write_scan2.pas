{   Subroutine IMG_WRITE_SCAN2 (IMG, SCAN, STAT)
*
*   Write the next scan line to the image stream indicated by IMG.  The scan
*   line is assumed to be an array of format 2 pixels.  STAT is the completion
*   status code.
}
module img_write_scan2;
define img_write_scan2;
%include 'img2.ins.pas';

procedure img_write_scan2 (            {write next scan line given format 2 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  in      scan: univ img_scan2_arg_t;  {scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to private data about this stream}

begin
  data_p := img.data_p;                {make pointer to our private data}
  data_p^.write_scan2^ (img, scan, stat); {call driver routine to do the work}
  end;
