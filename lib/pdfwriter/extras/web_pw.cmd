echo with PDF_Out;>small_demo.adb
echo.>>small_demo.adb
echo procedure Small_Demo is>>small_demo.adb
echo   pdf : PDF_Out.PDF_Out_File;>>small_demo.adb
echo begin>>small_demo.adb
echo   pdf.Create ("small.pdf");>>small_demo.adb
echo   pdf.Put_Line ("This is a very small demo for PDF_Out...");>>small_demo.adb
echo   pdf.Close;>>small_demo.adb
echo end Small_Demo;>>small_demo.adb

rem Call GNATMake without project file: we want the .ali here.

gnatmake ..\demo\pdf_out_demo.adb    -I.. -I..\gid -I..\demo -I..\test -I..\tools -j0
gnatmake ..\test\page_test.adb       -I.. -I..\gid -I..\demo -I..\test -I..\tools -j0
gnatmake ..\test\validation_test.adb -I.. -I..\gid -I..\demo -I..\test -I..\tools -j0
gnatmake ..\tools\img2pdf.adb        -I.. -I..\gid -I..\demo -I..\test -I..\tools -j0

gnatmake small_demo.adb -I.. -I..\gid -j0

rem Small_Demo without local references
perl pw_html.pl     small_demo -d -I.. -I..\gid -opw_html
rem The rest with local references
perl pw_html.pl     pdf_out_demo pdf_out.ads pdf_out.adb img2pdf.adb page_test.adb validation_test.adb -I.. -I..\gid -I..\demo -I..\test -I..\tools -f -d -opw_html

del *.ali
del *.o
del *.exe
