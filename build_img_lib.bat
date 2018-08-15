@echo off
rem
rem   BUILD_IMG_LIB [-dbg]
rem
rem   Build just the IMG linkable library.
rem
setlocal
set libname=img
set srclib=imglib

call src_go %srclib%
call src_get %srclib% img_sys.ins.pas
call src_get %srclib% jpeg_img.ins.pas
call src_get %srclib% tiff.ins.pas
call src_getfrom sys sys.ins.pas
call src_getfrom util util.ins.pas
call src_getfrom string string.ins.pas
call src_getfrom file file.ins.pas
call src_insall %srclib% %libname%

call src_go %srclib%
call src_pas %srclib% %libname%_close %1
call src_pas %srclib% %libname%_comblock %1
call src_pas %srclib% %libname%_conn_fmt %1
call src_pas %srclib% %libname%_driver_bmp %1
call src_pas %srclib% %libname%_driver_gif %1
call src_pas %srclib% %libname%_driver_img %1
call src_pas %srclib% %libname%_driver_inf %1
call src_pas %srclib% %libname%_driver_jpg %1
call src_pas %srclib% %libname%_driver_ps %1
call src_pas %srclib% %libname%_driver_tga %1
call src_pas %srclib% %libname%_driver_tif %1
call src_pas %srclib% %libname%_filter_warp %1
call src_pas %srclib% %libname%_find_driver %1
call src_pas %srclib% %libname%_head %1
call src_pas %srclib% %libname%_init_conn %1
call src_pas %srclib% %libname%_list_filts %1
call src_pas %srclib% %libname%_list_types %1
call src_pas %srclib% %libname%_mem_alloc %1
call src_pas %srclib% %libname%_next %1
call src_pas %srclib% %libname%_open_read_filt %1
call src_pas %srclib% %libname%_open_read_img %1
call src_pas %srclib% %libname%_open_write_img %1
call src_pas %srclib% %libname%_parms %1
call src_pas %srclib% %libname%_read_scan1 %1
call src_pas %srclib% %libname%_read_scan1_scan2 %1
call src_pas %srclib% %libname%_read_scan2 %1
call src_pas %srclib% %libname%_read_scan2_scan1 %1
call src_pas %srclib% %libname%_rewind %1
call src_pas %srclib% %libname%_write_scan1 %1
call src_pas %srclib% %libname%_write_scan1_scan2 %1
call src_pas %srclib% %libname%_write_scan2 %1
call src_pas %srclib% %libname%_write_scan2_scan1 %1

call src_lib %srclib% %libname%
call src_msg %srclib% %libname%
