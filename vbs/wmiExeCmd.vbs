On Error Resume Next
' 4个参数
If Wscript.Arguments.Count = 4 Then

Dim host
Dim user
Dim pwd
Dim cmd
Dim msg
host = Wscript.Arguments(0)
user=Wscript.Arguments(1)
pwd=Wscript.Arguments(2)
cmd=Wscript.Arguments(3)


set objlocator=createobject("wbemscripting.swbemlocator")
'set objswbemservices=objlocator.connectserver("192.168.0.15","root\cimv2","administrator","test")
set objswbemservices=objlocator.connectserver(host,"root\cimv2",user,pwd)
set ss=objswbemservices.get("Win32_ProcessStartup")
Set oC=ss.SpawnInstance_
oC.ShowWindow=SW_NORMAL
Set pp=objswbemservices.get("Win32_Process")
'Error = pp.Create("cmd   /k   mkdir f:\\cn\nmkdir f:\\cncn", null, null, intProcessID)
'Wscript.Echo cmd

Error = pp.Create("cmd /C " & cmd, null, oC, intProcessID)
If Error = 0 Then
    'Wscript.Echo "Notepad was started with a process ID of " _
    '     & intProcessID & "."
    
    '* 关闭cmd进程
    'set colProcessList = objswbemservices.execquery("Select * from Win32_Process Where ProcessId = " & intProcessID)
    'for each objProcess in colProcessList
    '    '*Wscript.Echo objProcess.name
    '    objProcess.Terminate()
    'next
Else
    'Wscript.Echo "Notepad could not be started due to error " & _
    '    Error & "."
    msg = "Notepad could not be started due to error"
End If
Wscript.Quit()

Else
    'Wscript.Echo "Must 4 parameters"
    msg = "Must 4 parameters"
' 4个参数
End If

