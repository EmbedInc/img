@echo off
rem
rem   BUILD_IMG_LIB [-dbg]
rem
rem   Build just the IMG linkable library.
rem
setlocal
call build_pasinit
set libname=img

call src_go %srcdir%
call src_get %srcdir% img_sys.ins.pas
call src_get %srcdir% jpeg_img.ins.pas
call src_get %srcdir% tiff.ins.pas
call src_insall %srcdir% %libname%

call src_go %srcdir%
call src_pas %srcdir% %libname%_close %1
call src_pas %srcdir% %libname%_comblock %1
call src_pas %srcdir% %libname%_conn_fmt %1
call src_pas %srcdir% %libname%_driver_bmp %1
call src_pas %srcdir% %libname%_driver_gif %1
call src_pas %srcdir% %libname%_driver_img %1
call src_pas %srcdir% %libname%_driver_inf %1
call src_pas %srcdir% %libname%_driver_jpg %1
call src_pas %srcdir% %libname%_driver_ps %1
call src_pas %srcdir% %libname%_driver_tga %1
call src_pas %srcdir% %libname%_driver_tif %1
call src_pas %srcdir% %libname%_filter_warp %1
call src_pas %srcdir% %libname%_find_driver %1
call src_pas %srcdir% %libname%_head %1
call src_pas %srcdir% %libname%_init_conn %1
call src_pas %srcdir% %libname%_list_filts %1
call src_pas %srcdir% %libname%_list_types %1
call src_pas %srcdir% %libname%_mem_alloc %1
call src_pas %srcdir% %libname%_next %1
call src_pas %srcdir% %libname%_open_read_filt %1
call src_pas %srcdir% %libname%_open_read_img %1
call src_pas %srcdir% %libname%_open_write_img %1
call src_pas %srcdir% %libname%_parms %1
call src_pas %srcdir% %libname%_path %1
call src_pas %srcdir% %libname%_read_scan1 %1
call src_pas %srcdir% %libname%_read_scan1_scan2 %1
call src_pas %srcdir% %libname%_read_scan2 %1
call src_pas %srcdir% %libname%_read_scan2_scan1 %1
call src_pas %srcdir% %libname%_rewind %1
call src_pas %srcdir% %libname%_write_scan1 %1
call src_pas %srcdir% %libname%_write_scan1_scan2 %1
call src_pas %srcdir% %libname%_write_scan2 %1
call src_pas %srcdir% %libname%_write_scan2_scan1 %1

call src_lib %srcdir% %libname%
call src_msg %srcdir% %libname%
