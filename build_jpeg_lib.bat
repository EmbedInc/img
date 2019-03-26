@echo off
rem
rem   Build the JPEG library.
rem
setlocal
call build_pasinit
set libname=jpeg

call src_get %srcdir% jchuff.h
call src_get %srcdir% jconfig.h
call src_get %srcdir% jdct.h
call src_get %srcdir% jdhuff.h
call src_get %srcdir% jerror.h
call src_get %srcdir% jinclude.h
call src_get %srcdir% jmemsys.h
call src_get %srcdir% jmorecfg.h
call src_get %srcdir% jpegint.h
call src_get %srcdir% jpeglib.h
call src_get %srcdir% jversion.h

call src_c %srcdir% %libname%_capimin %1
call src_c %srcdir% %libname%_capistd %1
call src_c %srcdir% %libname%_ccoefct %1
call src_c %srcdir% %libname%_ccolor %1
call src_c %srcdir% %libname%_cdctmgr %1
call src_c %srcdir% %libname%_chuff %1
call src_c %srcdir% %libname%_cinit %1
call src_c %srcdir% %libname%_cmainct %1
call src_c %srcdir% %libname%_cmarker %1
call src_c %srcdir% %libname%_cmaster %1
call src_c %srcdir% %libname%_comapi %1
call src_c %srcdir% %libname%_cparam %1
call src_c %srcdir% %libname%_cphuff %1
call src_c %srcdir% %libname%_cprepct %1
call src_c %srcdir% %libname%_csample %1
call src_c %srcdir% %libname%_ctrans %1
call src_c %srcdir% %libname%_dapimin %1
call src_c %srcdir% %libname%_dapistd %1
call src_c %srcdir% %libname%_datadst %1
call src_c %srcdir% %libname%_datasrc %1
call src_c %srcdir% %libname%_dcoefct %1
call src_c %srcdir% %libname%_dcolor %1
call src_c %srcdir% %libname%_ddctmgr %1
call src_c %srcdir% %libname%_dhuff %1
call src_c %srcdir% %libname%_dinput %1
call src_c %srcdir% %libname%_dmainct %1
call src_c %srcdir% %libname%_dmarker %1
call src_c %srcdir% %libname%_dmaster %1
call src_c %srcdir% %libname%_dmerge %1
call src_c %srcdir% %libname%_dphuff %1
call src_c %srcdir% %libname%_dpostct %1
call src_c %srcdir% %libname%_dsample %1
call src_c %srcdir% %libname%_dtrans %1
call src_c %srcdir% %libname%_error %1
call src_c %srcdir% %libname%_fdctflt %1
call src_c %srcdir% %libname%_fdctfst %1
call src_c %srcdir% %libname%_fdctint %1
call src_c %srcdir% %libname%_idctflt %1
call src_c %srcdir% %libname%_idctfst %1
call src_c %srcdir% %libname%_idctint %1
call src_c %srcdir% %libname%_idctred %1
call src_c %srcdir% %libname%_img_glue %1
call src_c %srcdir% %libname%_memmgr %1
call src_c %srcdir% %libname%_memnobs %1
call src_c %srcdir% %libname%_quant1 %1
call src_c %srcdir% %libname%_quant2 %1
call src_c %srcdir% %libname%_utils %1

call src_lib %srcdir% %libname%
