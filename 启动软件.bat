@echo off
chcp 65001 >nul
setlocal

set "APP_DIR=%~dp0"
cd /d "%APP_DIR%"

set "R_HOME=%APP_DIR%R"
set "R_LIBS_USER=%R_HOME%\library"
set "R_LIBS_SITE=%R_HOME%\library"
set "PATH=%R_HOME%\bin\x64;%R_HOME%\bin;%PATH%"

if not exist "%R_HOME%\bin\Rscript.exe" (
  echo Cannot find bundled R: "%R_HOME%\bin\Rscript.exe"
  echo Please make sure the R folder is included beside this file.
  pause
  exit /b 1
)

"%R_HOME%\bin\Rscript.exe" "%APP_DIR%scripts\launch_app.R"
if errorlevel 1 (
  echo.
  echo App failed to start. Check the message above.
  pause
  exit /b 1
)

endlocal
