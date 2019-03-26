@echo off
rem
rem   BUILD_LIB [-dbg]
rem
rem   Build the IMGLIB library.
rem
setlocal
call build_img_lib
call build_jpeg_lib
