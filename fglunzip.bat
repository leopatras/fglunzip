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
rem compile mygetopt first as it is used by fglunzip
set FGL_LENGTH_SEMANTICS=BYTE
set LANG=.fglutf8
CALL myfglcomp checkgit
IF %errorlevel% NEQ 0 GOTO myend
FOR /F "delims==" %%x IN ('git describe --tags --long --abbrev=8') DO set GIT_LONG_VERSION=%%x
IF %errorlevel% NEQ 0 GOTO myend
fglrun checkgit %GIT_LONG_VERSION%
IF %errorlevel% NEQ 0 GOTO myend
CALL myfglcomp mygetopt
IF %errorlevel% NEQ 0 GOTO myend
CALL myfglcomp futils
IF %errorlevel% NEQ 0 GOTO myend
CALL myfglcomp fglunzip
IF %errorlevel% NEQ 0 GOTO myend
popd
%MYDRIVE%
fglrun %FGLUNZIPDIR%\fglunzip.42m %*
:myend
ENDLOCAL
