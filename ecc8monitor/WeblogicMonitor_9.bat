@if NOT "%1"=="" set strUsername=%1
@if     "%1"=="" set strUsername=weblogic
@if NOT "%2"=="" set strPwd=%2
@if     "%2"=="" set strPwd=weblogic
@if NOT "%3"=="" set strServerIp=%3
@if     "%3"=="" set strServerIp=192.168.0.161
@if NOT "%4"=="" set strServerPort=%4
@if     "%4"=="" set strServerPort=7001
@if NOT "%5"=="" set strTaskType=%5
@if     "%5"=="" set strTaskType=WlsStatus
@if NOT "%6"=="" set strTaskParam=%6
@if     "%6"=="" set strTaskParam=

set SITEVIEW_HOME9=E:\jdk1.6.0_0
set path=%SITEVIEW_HOME9%\bin;%path%
cd  ecc8monitor
set classpath=weblogic_9.jar;%SITEVIEW_HOME9%\lib\dt.jar;%SITEVIEW_HOME9%\lib\htmlconverter.jar;%SITEVIEW_HOME9%\lib\tools.jar;%SITEVIEW_HOME9%\lib;%SITEVIEW_HOME9%\jre\lib\charsets.jar;%SITEVIEW_HOME9%\jre\lib\jce.jar;%SITEVIEW_HOME9%\jre\lib\jsse.jar;%SITEVIEW_HOME9%\jre\lib\plugin.jar;%SITEVIEW_HOME9%\jre\lib\jsse.jar;%SITEVIEW_HOME9%\jre\lib\rt.jar;%SITEVIEW_HOME9%\jre\lib\sunrsasign.jar;%SITEVIEW_HOME9%\jre\lib\ext\dnsns.jar;%SITEVIEW_HOME9%\jre\lib\ext\ldapsec.jar;%SITEVIEW_HOME9%\jre\lib\ext\localedata.jar;%SITEVIEW_HOME9%\jre\lib\ext\sunjce_provider.jar; 
java -Dsun.lang.ClassLoader.allowArraySyntax=true GetWeblogicPerf %strUsername% %strPwd% %strServerIp% %strServerPort% %strTaskType% %strTaskParam%
