@echo off
"C:\Program Files\HTML Help Workshop\hhc.exe" helpfile.hhp
copy helpfile.chm ..\bdoc.chm
pause
