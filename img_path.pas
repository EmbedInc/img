{   Pathname handling.
}
module img_path;
define img_path_f_gnam;
%include 'img2.ins.pas';
{
********************************************************************************
*
*   Function IMG_PATH_F_GNAM (GNAM, PATH)
*
*   Determine the full pathname of an image file to read from its generic name.
*   The generic name GNAM must either be absolute, or relative to the current
*   directory.  The directory that GNAM is in is scanned looking for a file with
*   the right suffix to be an image that is readable by the IMG library.
*
*   The function returns TRUE when a suitable image file is found, and FALSE
*   when not.
}
function img_path_f_gnam (             {get pathname from generic name of img to read}
  in      gnam: univ string_var_arg_t; {input generic image name}
  in out  path: univ string_var_arg_t) {output name with suffix of file that exists}
  :boolean;                            {an image file was found}
  val_param;

var
  suffs: string_var132_t;              {list of file name suffixes, blank separated}
  p: string_index_t;                   {SUFFS parse index}
  suff: string_var80_t;                {current suffix to test}
  fnam: string_treename_t;             {file name with suffix to test}
  stat: sys_err_t;

begin
  suffs.max := size_char(suffs.str);   {init local var strings}
  suff.max := size_char(suff.str);
  img_path_f_gnam := false;            {init to no readable image file found}

  img_list_types (                     {get list of suffixes for suitable image files}
    [file_rw_read_k],                  {image files must be readable by IMG lib}
    suffs);                            {returned list of file name suffixes}

  string_copy (gnam, fnam);            {init file name to without suffix}

  p := 1;                              {init suffixes list parse index}
  while true do begin                  {back here each new suffix to try}
    string_token (suffs, p, suff, stat); {get the next suffix to try}
    if string_eos(stat) then exit;     {hit end of suffixes list ?}
    fnam.len := gnam.len;              {rest file name to no suffix}
    string_append1 (fnam, '.');        {add separator between gnam and suffix}
    string_append (fnam, suff);        {make file name with this suffix}
    if file_exists (fnam) then begin   {file with this suffix exists ?}
      string_copy (fnam, path);        {return pathname of image file with suffix}
      img_path_f_gnam := true;         {indicate success}
      return;
      end;
    end;                               {back to try next suffix}
  end;
