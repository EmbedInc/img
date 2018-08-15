@echo off
rem
rem   Build the STUFF library.
rem
setlocal
set libname=jpeg
set srclib=imglib

call src_get %srclib% jchuff.h
call src_get %srclib% jconfig.h
call src_get %srclib% jdct.h
call src_get %srclib% jdhuff.h
call src_get %srclib% jerror.h
call src_get %srclib% jinclude.h
call src_get %srclib% jmemsys.h
call src_get %srclib% jmorecfg.h
call src_get %srclib% jpegint.h
call src_get %srclib% jpeglib.h
call src_get %srclib% jversion.h

call src_c %srclib% %libname%_capimin %1
call src_c %srclib% %libname%_capistd %1
call src_c %srclib% %libname%_ccoefct %1
call src_c %srclib% %libname%_ccolor %1
call src_c %srclib% %libname%_cdctmgr %1
call src_c %srclib% %libname%_chuff %1
call src_c %srclib% %libname%_cinit %1
call src_c %srclib% %libname%_cmainct %1
call src_c %srclib% %libname%_cmarker %1
call src_c %srclib% %libname%_cmaster %1
call src_c %srclib% %libname%_comapi %1
call src_c %srclib% %libname%_cparam %1
call src_c %srclib% %libname%_cphuff %1
call src_c %srclib% %libname%_cprepct %1
call src_c %srclib% %libname%_csample %1
call src_c %srclib% %libname%_ctrans %1
call src_c %srclib% %libname%_dapimin %1
call src_c %srclib% %libname%_dapistd %1
call src_c %srclib% %libname%_datadst %1
call src_c %srclib% %libname%_datasrc %1
call src_c %srclib% %libname%_dcoefct %1
call src_c %srclib% %libname%_dcolor %1
call src_c %srclib% %libname%_ddctmgr %1
call src_c %srclib% %libname%_dhuff %1
call src_c %srclib% %libname%_dinput %1
call src_c %srclib% %libname%_dmainct %1
call src_c %srclib% %libname%_dmarker %1
call src_c %srclib% %libname%_dmaster %1
call src_c %srclib% %libname%_dmerge %1
call src_c %srclib% %libname%_dphuff %1
call src_c %srclib% %libname%_dpostct %1
call src_c %srclib% %libname%_dsample %1
call src_c %srclib% %libname%_dtrans %1
call src_c %srclib% %libname%_error %1
call src_c %srclib% %libname%_fdctflt %1
call src_c %srclib% %libname%_fdctfst %1
call src_c %srclib% %libname%_fdctint %1
call src_c %srclib% %libname%_idctflt %1
call src_c %srclib% %libname%_idctfst %1
call src_c %srclib% %libname%_idctint %1
call src_c %srclib% %libname%_idctred %1
call src_c %srclib% %libname%_img_glue %1
call src_c %srclib% %libname%_memmgr %1
call src_c %srclib% %libname%_memnobs %1
call src_c %srclib% %libname%_quant1 %1
call src_c %srclib% %libname%_quant2 %1
call src_c %srclib% %libname%_utils %1

call src_lib %srclib% %libname%
