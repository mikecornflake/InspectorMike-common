@echo off
echo Initialising external dependencies...
git submodule update --init --recursive

if errorlevel 1 (
  echo.
  echo Failed to initialise submodules.
  pause
  exit /b 1
)

echo.
echo Done.
pause