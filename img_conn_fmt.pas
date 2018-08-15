{   Subroutine IMG_CONN_FMT (IMG, FILE_FMT)
*
*   Create the image file format FILE_FMT from the image connection handle IMG.
*   The format string will contain resolution values for alpha, red, green,
*   and blue.
}
module img_conn_fmt;
define img_conn_fmt;
%include '/cognivision_links/dsee_libs/img/img2.ins.pas';

procedure img_conn_fmt (               {make format string with resolution values}
  in      img: img_conn_t;             {image stream connection handle}
  out     file_fmt: univ string_var_arg_t); {will have ARGB resolution values}

var
  token: string_var32_t;               {string/integer conversion token}

begin
  token.max := sizeof(token.str);      {init var string}

  file_fmt.len := 0;
  string_appendn (file_fmt, 'ALPHA ', 6);
  string_f_int (token, img.bits_alpha);
  string_append (file_fmt, token);

  string_appendn (file_fmt, ' RED ', 5);
  string_f_int (token, img.bits_red);
  string_append (file_fmt, token);

  string_appendn (file_fmt, ' GREEN ', 7);
  string_f_int (token, img.bits_grn);
  string_append (file_fmt, token);

  string_appendn (file_fmt, ' BLUE ', 6);
  string_f_int (token, img.bits_blu);
  string_append (file_fmt, token);
  end;
