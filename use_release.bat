@echo off
@rem Build release and stage it as bin\<worktree>.exe
@rem Pass a name to override the worktree-derived one (ad-hoc variant builds).
setlocal

set "root=%~dp0"
if "%~1"=="" (
    for %%I in ("%root%.") do set "feature=%%~nxI"
) else (
    set "feature=%~1"
)

pushd "%root%" || exit /B 1
cargo build -r
if errorlevel 1 (popd & exit /B 1)
popd

if not exist "%root%bin" mkdir "%root%bin"
copy /Y "%root%target\release\simd-chess.exe" "%root%bin\%feature%.exe" >nul
if errorlevel 1 exit /B 1

echo staged bin\%feature%.exe
exit /B 0
