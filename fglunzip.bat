@echo off
SETLOCAL
set FGLUNZIPDIR=%~dp0
set THISDRIVE=%~dd0
FOR %%i IN ("%CD%") DO (
  set MYDRIVE=%%~di
)
pushd %CD%
%THISDRIVE%
cd %FGLUNZIPDIR%
set FGL_LENGTH_SEMANTICS=BYTE
set LANG=.fglutf8
rem compile checkgit first 
CALL myfglcomp checkgit
IF %errorlevel% NEQ 0 GOTO myend
git describe --tags --long --abbrev=8 >git_ver.txt
set /P GIT_LONG_VERSION=<git_ver.txt
del /Q git_ver.txt
IF %errorlevel% NEQ 0 GOTO myend
fglrun checkgit %GIT_LONG_VERSION%
IF %errorlevel% NEQ 0 GOTO myend
REM uses inc file
CALL fglcomp -M -r -Wall fglunzip
IF %errorlevel% NEQ 0 GOTO myend
CALL myfglcomp mygetopt
IF %errorlevel% NEQ 0 GOTO myend
popd
%MYDRIVE%
fglrun %FGLUNZIPDIR%\fglunzip.42m %*
:myend
ENDLOCAL
