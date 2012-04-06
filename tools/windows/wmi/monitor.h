#pragma once


// 如果必须将位于下面指定平台之前的平台作为目标，请修改下列定义。
// 有关不同平台对应值的最新信息，请参考 MSDN。
#ifndef WINVER				// 允许使用特定于 Windows XP 或更高版本的功能。
#define WINVER 0x0501		// 将此值更改为相应的值，以适用于 Windows 的其他版本。
#endif

#ifndef _WIN32_WINNT		// 允许使用特定于 Windows XP 或更高版本的功能。
#define _WIN32_WINNT 0x0501	// 将此值更改为相应的值，以适用于 Windows 的其他版本。
#endif						

#ifndef _WIN32_WINDOWS		// 允许使用特定于 Windows 98 或更高版本的功能。
#define _WIN32_WINDOWS 0x0410 // 将此值更改为适当的值，以指定将 Windows Me 或更高版本作为目标。
#endif

#ifndef _WIN32_IE			// 允许使用特定于 IE 6.0 或更高版本的功能。
#define _WIN32_IE 0x0600	// 将此值更改为相应的值，以适用于 IE 的其他版本。
#endif

#define WIN32_LEAN_AND_MEAN		// 从 Windows 头中排除极少使用的资料
// Windows 头文件:
#include <windows.h>

#include <string>
#include <map>
#include <list>
#include <vector>
#include <algorithm>
#include <functional>
#include <cctype> 
using namespace std;

#import "progid:WbemScripting.SWbemLocator" named_guids

#include <comdef.h>
#include <wbemcli.h>

#include <winbase.h>

#pragma comment(lib,"WbemUuid.Lib")

#define BUFFER_SIZE 256

typedef enum _wmi_operation_type_
{
	cpu = 1,
	memory,
	disk,
	service,
	process,
	network,
	directory
} WMI_OPERATION_TYPE;

typedef enum _os_type_
{
	Win32s = 1,
	Win95,
	Win98,
	WinME,
	WinNT351,
	WinNT4,
	Win2000,
	WinXP,
	Win2003,
	WinCE,
	Win2008,
	Vista,
	Win7
} OS_TYPE;


typedef struct _wmi_login_info_
{
	int os;
	char machine[BUFFER_SIZE];
	char user[BUFFER_SIZE];
	char password[BUFFER_SIZE];
} WMI_LOGIN_INFO, *PWMI_LOGIN_INFO;

BOOL IsLocalHost(string host);

BOOL ConnectServer(PWMI_LOGIN_INFO login, char* buffer, WbemScripting::ISWbemServicesPtr &services);

BOOL AnsiToUtf8(char* strAnsi, char* strUtf8, int size);