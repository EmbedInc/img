@echo off
setlocal
call treename_var (cog)source/img/%1 tnam
if not exist "%tnam%" (
  echo "%tnam%" not found.
  exit /b 3
  )
echo %tnam%
copya "%tnam%" %1
