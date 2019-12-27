{   Subroutine IMG_READ_SCAN1 (IMG, SCAN, STAT)
*
*   Read the next scan line from the image stream.  The scan line data will be
*   returned in SCAN as an array of format 1 pixels.  IMG is the handle to
*   the image stream to read from.  STAT is the completion status.
}
module img_read_scan1;
define img_read_scan1;
%include 'img2.ins.pas';

procedure img_read_scan1 (             {read next scan line as format 1 pixels}
  in out  img: img_conn_t;             {handle to this image data stream}
  out     scan: univ img_scan1_arg_t;  {returned scan line of pixels}
  out     stat: sys_err_t);            {completion status code}

var
  data_p: img_conn2_p_t;               {pointer to private data about this stream}

begin
  data_p := img.data_p;                {make pointer to our private data}
  data_p^.read_scan1^ (img, scan, stat); {call driver routine to do the work}
  end;
