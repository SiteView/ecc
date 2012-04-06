regsvr32 /s C:\SiteScope\bin\micftp.dll
regsvr32 /s C:\SiteScope\bin\exptestzip.dll
regsvr32 /s C:\SiteScope\bin\configco.dll
cmd.exe /c start iexplore "C:\SiteScope\Open SiteScope.htm"
cmd.exe /c "net start SiteScope"
C:\SiteScope\bin\mreg.exe /REGSERVER
regsvr32 /s C:\SiteScope\bin\lrvb1.dll
regsvr32 /s C:\SiteScope\bin\lrvb.dll
"C:\SiteScope\bin\setlicensepermissions.exe" -n -e "SiteScope"
regsvr32 /s C:\SiteScope\bin\lm70.dll
regsvr32 /s C:\SiteScope\bin\dtf1lite.dll
regsvr32 /s C:\SiteScope\bin\book.dll
regsvr32 /s C:\SiteScope\bin\micimap.dll
regsvr32 /s C:\SiteScope\bin\micmapi.dll
regsvr32 /s C:\SiteScope\bin\micsmtp.dll
regsvr32 /s C:\SiteScope\bin\micpop3.dll
