#pragma once

#import "progid:WbemScripting.SWbemLocator" named_guids

#include <erl_driver.h>
#include <ei.h>
#include <comdef.h>
#include <wbemcli.h>
#include <winbase.h>

struct WmiNode
{
	string server;
	string username;
	string password;
	class CWmi * wmi;
};


class CWmi
{
public:
	CWmi(void);
	~CWmi(void);
	bool Open(ei_x_buff *x,char *server,char *username,char *password);
private:
	WbemScripting::ISWbemServicesPtr  services;
	char szMess[256];
	DWORD MyVariantToBSTR(VARIANT *out_pvarDes,VARIANT *in_pvarSrc);
public:
	bool Execute(ei_x_buff *x,char* wql);
	void Close(void);
};
