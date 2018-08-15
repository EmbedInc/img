{   Pascal interface file for the JPEG library.  The JPEG library is mostly
*   from the source code provided by IJG (International JPEG Group).  However,
*   the IJG souce is all in C and only comes with C include files.  To facilitate
*   upgrading to future IJG releases, most interfacing to the IJG library
*   is done thru our C module JPEG_IMG_GLUE.C.
*
*   This include file declares the interface to the IJG JPEG library, mostly
*   as surfaced by the JPEG_IMG_GLUE.C module.
}
type
  jpeg_pixform_k_t = (                 {all the different pixel formats}
    jpeg_pixform_rgb24_k = 0,          {24 bit RGB data per pixel}
    jpeg_pixform_gray8_k = 1);         {8 bit gray scale data per pixel}

  jpeg_appout_t = record               {start of app data required for writing}
    cinfo_p: univ_ptr;                 {pointer to internal JPEG library state}
    buf_p: univ_ptr;                   {pointer to start of output buffer}
    bufsize: sys_int_adr_t;            {output buffer size}
    bufn: sys_int_adr_t;               {number of data bytes currently in buffer}
    writebuf: ^procedure (             {routine to write contents of output buffer}
      appout: ^jpeg_appout_t);         {pointer to app data}
      val_param;
    end;
  jpeg_appout_p_t = ^jpeg_appout_t;

  jpeg_appin_t = record                {start of app data required for reading}
    cinfo_p: univ_ptr;                 {pointer to internal JPEG library state}
    buf_p: univ_ptr;                   {pointer to start of input buffer}
    bufsize: sys_int_adr_t;            {input buffer size}
    bufn: sys_int_adr_t;               {number of unread bytes currently in buffer}
    pixx, pixy: sys_int_machine_t;     {image size in pixels}
    ncomp: sys_int_machine_t;          {number of component values per pixel}
    pixform: jpeg_pixform_k_t;         {pixel format ID}
    readbuf: ^procedure (              {routine to read more data into input buffer}
      appin: ^jpeg_appin_t);
      val_param;
    end;


procedure jpg_open_out (               {open JPEG compression output stream}
  in out  jpegout: jpeg_appout_t;      {application data for output stream}
  in      qual: real;                  {0.0 to 1.0 quality level}
  in      pixx, pixy: sys_int_machine_t; {image dimension in pixels}
  in      pixaspect: real;             {width/height aspect ratio of each pixel}
  in      pixform: jpeg_pixform_k_t);  {pixel format ID}
  val_param; extern;

procedure jpg_write_scan (             {write one scan line to JPEG output stream}
  in out  jpegout: jpeg_appout_t;      {application data for output stream}
  in      scan: univ int8u_t);         {scan line output data}
  val_param; extern;

procedure jpg_close_out (              {close JPEG compression stream, deallocate}
  in out  jpegout: jpeg_appout_t);     {application data for output stream}
  val_param; extern;

procedure jpg_open_in (                {open JPEG compression input stream}
  in out  jpegin: jpeg_appin_t);       {application data for input stream}
  val_param; extern;

procedure jpg_read_scan (              {read next scan line from JPEG input stream}
  in out  jpegin: jpeg_appin_t;        {application data for input stream}
  out     buf: univ int8u_t);          {buffer to read scan line data into}
  val_param; extern;

procedure jpg_close_in (               {close JPEG compression input stream}
  in out  jpegin: jpeg_appin_t);       {application data for input stream}
  val_param; extern;
