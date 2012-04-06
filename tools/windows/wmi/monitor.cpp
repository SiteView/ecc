#include "monitor.h"

#include<winsock2.h>
#pragma comment(lib,"ws2_32.lib")

BOOL IsLocalHost(string host)
{
	WSADATA wsaData;
	char name[256];
	char *ip;
	PHOSTENT hostinfo; 

	if(host == "127.0.0.1" || host == "localhost")
	{
		return TRUE;
	}

	if(WSAStartup(MAKEWORD(2,0), &wsaData) == 0) 
	{ 
		if(gethostname(name, sizeof(name)) == 0) 
		{ 
			if(host == name)
			{
				WSACleanup();
				return TRUE;
			}
			
			if((hostinfo = gethostbyname(name)) != NULL) 
			{ 
				ip = inet_ntoa (*(struct in_addr *)*hostinfo->h_addr_list); 
				if(host == ip)
				{
					WSACleanup();
					return TRUE;
				}
			} 
		} 
		WSACleanup();
	}
	return FALSE;
}


BOOL ConnectServer(PWMI_LOGIN_INFO login, char* buffer, WbemScripting::ISWbemServicesPtr &services)
{
	try 
	{ 
		WbemScripting::ISWbemLocatorPtr locator; 
		locator.CreateInstance(WbemScripting::CLSID_SWbemLocator);
		
		if(locator != NULL) 
		{ 
			if(IsLocalHost(login->machine))
			{
				services = locator->ConnectServer(".","root\\cimv2","","","","",0,NULL);
			}
			else
			{
				services = locator->ConnectServer(login->machine,"root\\cimv2",login->user, login->password, "","",0,NULL);
			}

			locator.Release();
		}
	}
	catch (_com_error err) 
	{ 
		IErrorInfo * ei = err.ErrorInfo();
		BSTR strDesEI;
		ei->GetDescription(&strDesEI);
		char* pstr = _com_util::ConvertBSTRToString(strDesEI);
		sprintf(buffer, "error=Error ocured:%x: %s", (unsigned)err.Error(), pstr);

		ei->Release();
		free(pstr);
		::SysFreeString(strDesEI);

		return FALSE;
	} 
	catch(...) 
	{ 
		sprintf(buffer, "error=Error ocured: %d",::GetLastError());
		return FALSE;
	} 
	return TRUE;
}


BOOL AnsiToUtf8(char* strAnsi, char* strUtf8, int size)
{

	int wcsLen = MultiByteToWideChar(CP_ACP, NULL, strAnsi, strlen(strAnsi), NULL, 0);
	wchar_t* wcsString = new wchar_t[wcsLen + 1];
	MultiByteToWideChar(CP_ACP, NULL, strAnsi, strlen(strAnsi), wcsString, wcsLen);
	wcsString[wcsLen] = '\0';


	int utf8Len = WideCharToMultiByte(CP_UTF8, NULL, wcsString, wcslen(wcsString), NULL, 0, NULL, NULL);    
	if(utf8Len < size)
	{
		WideCharToMultiByte(CP_UTF8, NULL, wcsString, wcslen(wcsString), strUtf8, utf8Len, NULL, NULL);
		strUtf8[utf8Len] = '\0';
	}

	delete[] wcsString;
	wcsString =NULL;

	return utf8Len < size;

}
