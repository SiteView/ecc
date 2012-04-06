@echo off
echo SiteView Server path:
cd
set ProcessName=erl.exe
for /f "tokens=5" %%a in ('tasklist /fi "imagename eq %ProcessName%"') do (
  set ProcessMem=%%a
)
echo %ProcessName% memory use
echo %ProcessMem% KB