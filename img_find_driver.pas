{   Subroutine IMG_FIND_DRIVER (FNAM, DR)
*
*   Find the driver, if any, that is explicitly specified by the image file name
*   in FNAM.  An image file name explicitly specifies a particular driver if
*   it ends in .<driver name>, where the driver name is lower case.
*   DR is returned as the 1..N_DRIVERS driver number.  N is set to zero if
*   the file name suffix did not match any driver name.
}
module img_find_driver;
define img_find_driver;
%include 'img2.ins.pas';

procedure img_find_driver (            {find driver associated with image file name}
  in      fnam: univ string_var_arg_t; {file name with driver name extension}
  out     dr: sys_int_machine_t);      {driver number, = 0 if no match}

var
  i: sys_int_machine_t;                {loop counter}
  p: sys_int_machine_t;                {FNAM read index}

label
  next_driver;

begin
  for dr := 1 to n_drivers do begin    {once for each known driver}
    p := fnam.len - driver[dr].name.len; {first FNAM char to check}
    if p <= 1 then goto next_driver;   {FNAM too short to end in driver name ?}
    if fnam.str[p] <> '.' then goto next_driver; {no period before suffix ?}
    p := p + 1;                        {advance to first FNAM char of driver name}
    for i := 1 to driver[dr].name.len do begin {once for each char in driver name}
      if fnam.str[p] <> driver[dr].name.str[i] {not match driver name ?}
        then goto next_driver;
      p := p + 1;                      {advance FNAM char read index}
      end;                             {back and compare next char}
{
*   The end of FNAM matches the current driver name.
}
    return;                            {DR is set to driver number}

next_driver:                           {jump here if current driver doesn't match}
    end;                               {back and try next driver in list}
{
*   The end of FNAM matched none of the driver names.
}
  dr := 0;                             {inidicate no match}
  end;
