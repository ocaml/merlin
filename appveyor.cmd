@setlocal
@echo off

set Path=C:\cygwin\bin;%Path%
set OCAML_PREV_PATH=%PATH%
set OCAML_PREV_LIB=%LIB%
set OCAML_PREV_INCLUDE=%INCLUDE%

bash -lc "$APPVEYOR_BUILD_FOLDER/appveyor.sh prepare mingw"
if errorlevel 1 exit /b 1
bash -lc "$APPVEYOR_BUILD_FOLDER/appveyor.sh prepare mingw64"
if errorlevel 1 exit /b 1
call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.cmd" /x86 /release
bash -lc "$APPVEYOR_BUILD_FOLDER/appveyor.sh prepare msvc"
if errorlevel 1 exit /b 1
set PATH=%OCAML_PREV_PATH%
set LIB=%OCAML_PREV_LIB%
set INCLUDE=%OCAML_PREV_INCLUDE%
call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.cmd" /x64 /release
bash -lc "$APPVEYOR_BUILD_FOLDER/appveyor.sh prepare msvc64"
if errorlevel 1 exit /b 1
bash -lc "$APPVEYOR_BUILD_FOLDER/appveyor.sh matrix"
bash -lc "$APPVEYOR_BUILD_FOLDER/appveyor.sh build msvc64"
if errorlevel 1 exit /b 1
set PATH=%OCAML_PREV_PATH%
set LIB=%OCAML_PREV_LIB%
set INCLUDE=%OCAML_PREV_INCLUDE%
call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.cmd" /x86 /release
bash -lc "$APPVEYOR_BUILD_FOLDER/appveyor.sh build msvc"
if errorlevel 1 exit /b 1
set PATH=%OCAML_PREV_PATH%
set LIB=%OCAML_PREV_LIB%
set INCLUDE=%OCAML_PREV_INCLUDE%
bash -lc "$APPVEYOR_BUILD_FOLDER/appveyor.sh build mingw"
if errorlevel 1 exit /b 1
bash -lc "$APPVEYOR_BUILD_FOLDER/appveyor.sh build mingw64"
if errorlevel 1 exit /b 1
