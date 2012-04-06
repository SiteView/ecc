@if NOT "%1"=="" set strUsername=%1
@if     "%1"=="" set strUsername=administrator
@if NOT "%2"=="" set strPwd=%2
@if     "%2"=="" set strPwd=12345678
@if NOT "%3"=="" set strServerIp=%3
@if     "%3"=="" set strServerIp=t3://192.168.0.161:8001
@if NOT "%4"=="" set strTaskType=%4
@if     "%4"=="" set strTaskType=WlsStatus
@if NOT "%5"=="" set strTaskParam=%5
@if     "%5"=="" set strTaskParam=null

set SITEVIEW_HOME=E:\bea\jdk141_05
set path=%SITEVIEW_HOME%\bin;%path%
cd  ecc8monitor

set classpath=weblogic.jar;%SITEVIEW_HOME%\lib\dt.jar;%SITEVIEW_HOME%\lib\htmlconverter.jar;%SITEVIEW_HOME%\lib\tools.jar;%SITEVIEW_HOME%\lib;%SITEVIEW_HOME%\jre\lib\charsets.jar;%SITEVIEW_HOME%\jre\lib\jce.jar;%SITEVIEW_HOME%\jre\lib\jsse.jar;%SITEVIEW_HOME%\jre\lib\plugin.jar;%SITEVIEW_HOME%\jre\lib\jsse.jar;%SITEVIEW_HOME%\jre\lib\rt.jar;%SITEVIEW_HOME%\jre\lib\sunrsasign.jar;%SITEVIEW_HOME%\jre\lib\ext\dnsns.jar;%SITEVIEW_HOME%\jre\lib\ext\ldapsec.jar;%SITEVIEW_HOME%\jre\lib\ext\localedata.jar;%SITEVIEW_HOME%\jre\lib\ext\sunjce_provider.jar; 
java -Dsun.lang.ClassLoader.allowArraySyntax=true getActiveDomainAndServers -U %strUsername% -P %strPwd% -I %strServerIp% -T %strTaskType%  -C %strTaskParam%
