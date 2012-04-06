
#wmic -U administrator%888888 //192.168.0.185 "select * from Win32_ComputerSystem"
#wmic -U administrator%888888 //192.168.0.185 "select * from Win32_BaseBoard"
#wmic -U administrator%888888 //192.168.0.185 "select * from Win32_Directory where Drive= 'C:' and FileName Like 'Windows%'"
#wmic -U administrator%888888 //192.168.0.185 "select * from Win32_Directory where Drive= 'C:' and Path = '\\\\'"
wmic -U administrator%888888 //192.168.0.185 "select * from CIM_DataFile where Drive= 'C:' and Path = '\\\\' and FileName = 'ntdetect'"

   

