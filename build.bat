@echo off
rem
rem   BUILD [-dbg]
rem
rem   Build everything in this source directory, which is the IMG library.
rem
setlocal
set srclib=imglib
call godir (cog)source/%srclib%
call build_jpeg_lib
call build_img_lib
