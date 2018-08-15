{   System-dependent private include file for the IMG library.
*
*   This is the "standard" version, and is the main line of decent.
}
const
  img_drivers_max = 9;                 {max image file drivers "loaded" at one time}
  img_filters_max = 1;                 {max conversion filters "loaded" at one time}
{
*   Private common block for IMG library routines.
}
var (img2)
  n_drivers: sys_int_machine_t         {number of known image file drivers}
    := img_drivers_max;
  driver:                              {data about all known image file drivers}
    array [1..img_drivers_max] of img_driver_t := [
      [
        name := [str := 'img', len := 3, max := sizeof(driver[1].name.str)],
        open_read_proc := addr(img_d_img_open_read),
        open_write_proc := addr(img_d_img_open_write)],
      [
        name := [str := 'tif', len := 3, max := sizeof(driver[1].name.str)],
        open_read_proc := addr(img_d_tif_open_read),
        open_write_proc := addr(img_d_tif_open_write)],
      [
        name := [str := 'nef', len := 3, max := sizeof(driver[1].name.str)],
        open_read_proc := addr(img_d_tif_open_read),
        open_write_proc := addr(img_d_tif_open_write)],
      [
        name := [str := 'gif', len := 3, max := sizeof(driver[1].name.str)],
        open_read_proc := addr(img_d_gif_open_read),
        open_write_proc := addr(img_d_gif_open_write)],
      [
        name := [str := 'jpg', len := 3, max := sizeof(driver[1].name.str)],
        open_read_proc := addr(img_d_jpg_open_read),
        open_write_proc := addr(img_d_jpg_open_write)],
      [
        name := [str := 'bmp', len := 3, max := sizeof(driver[1].name.str)],
        open_read_proc := nil,
        open_write_proc := addr(img_d_bmp_open_write)],
      [
        name := [str := 'ps', len := 2, max := sizeof(driver[1].name.str)],
        open_read_proc := nil,
        open_write_proc := addr(img_d_ps_open_write)],
      [
        name := [str := 'tga', len := 3, max := sizeof(driver[1].name.str)],
        open_read_proc := addr(img_d_tga_open_read),
        open_write_proc := addr(img_d_tga_open_write)],
      [
        name := [str := 'inf', len := 3, max := sizeof(driver[1].name.str)],
        open_read_proc := addr(img_d_inf_open_read),
        open_write_proc := nil]
    ];
  n_filters: sys_int_machine_t         {number of known conversion filters}
    := img_filters_max;
  filter:                              {data about all known conversion filters}
    array [1..img_filters_max] of img_filter_t := [
      [
        name := [str := 'warp', len := 4, max := sizeof(filter[1].name.str)],
        open_read_proc := addr(img_f_warp_open_read)]
    ];
