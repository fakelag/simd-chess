@echo off
@rem Build the SPSA variant (--features spsa) and stage it as bin\<worktree>-spsa.exe
@rem Pass a name to override the worktree-derived one.
setlocal

set "root=%~dp0"
if "%~1"=="" (
    for %%I in ("%root%.") do set "feature=%%~nxI"
) else (
    set "feature=%~1"
)

pushd "%root%" || exit /B 1
cargo build -r --features spsa --target-dir target\spsa
if errorlevel 1 (popd & exit /B 1)
popd

if not exist "%root%bin" mkdir "%root%bin"
copy /Y "%root%target\spsa\release\simd-chess.exe" "%root%bin\%feature%-spsa.exe" >nul
if errorlevel 1 exit /B 1

echo staged bin\%feature%-spsa.exe
exit /B 0
