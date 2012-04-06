
#include "AIXErrpt.h"
#include "TelMonitor.h"
#include "Utils.h"

#define		__MACHINENAME__					"_MachineName="
#define		__USERACCOUNT__					"_UserAccount="
#define		__PASSWORD__					"_PassWord="
#define		__COMMAND__						"_Cmd="
#define		__PORT__						"_Port="

bool telnetValue(char *server, char *uid, char *pwd, int port, char* cmd,char *szReturn);

bool MakeStringListByChar(CStringList& pList, const char * pInput )
{
	const char * p;
	int nSize;
	p=pInput;
	while(*p!='\0')
	{
		nSize = static_cast<int>(strlen(p));
		if( nSize>0 )
		{	
			pList.AddHead(p);
			//	printf(p);
		}
		p=p+nSize+1;
	}
	return true;
}

bool MakeCharByString(char *pOut,int &nOutSize,CString strInput )
{
	char *p;

	int nSize=strInput.GetLength();
	if(nSize+2 <nOutSize)
	{
		strcpy(pOut,strInput.GetBuffer(strInput.GetLength()));
	}
	else 
		return false;
	p=pOut;

	for(int i=0;i<nSize;i++)
	{
		if(*p=='$') 	
			*p='\0';
		p++;
	}
	nOutSize=nSize+1;
	return true;
}

extern "C" __declspec(dllexport) 
BOOL  get_telnet_info(const char * strParas, char * szReturn, int& nSize)
//bool get_telnet_info(char *server, char *uid, char *pwd, int port, char *szReturn)
{
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	CStringList paramList;
	MakeStringListByChar(paramList,strParas);

	CString strUser = _T("");
	CString strPwd = _T("");
	CString strService = _T("");
	CString strCmd = _T("");
	int     nPort = 23;


	POSITION pos = paramList.GetHeadPosition();
	while(pos)
	{
		CString strTmp = _T("");
		strTmp = paramList.GetNext(pos);
		if (0 == strTmp.Find(__MACHINENAME__))
		{
			strService = strTmp.Right(strTmp.GetLength() - strlen(__MACHINENAME__));
		}
		else if (0 == strTmp.Find(__USERACCOUNT__))
		{
			strUser = strTmp.Right(strTmp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if (0 == strTmp.Find(__PASSWORD__))
		{
			strPwd = strTmp.Right(strTmp.GetLength() - strlen(__PASSWORD__));
		}
		else if (0 == strTmp.Find(__COMMAND__))
		{
			strCmd = strTmp.Right(strTmp.GetLength() - strlen(__COMMAND__));
		}
		else if (0 == strTmp.Find(__PORT__))
		{
			nPort = atoi(strTmp.Right(strTmp.GetLength() - strlen(__PORT__)));
		}
		printf("strTmp = %s\n", strTmp.GetBuffer(strTmp.GetLength()));
	}


	if (strService.IsEmpty())
	{
		sprintf(szReturn, "error=%s", "server_not_found");
		return FALSE;
	}

	telnetValue(strService.GetBuffer(strService.GetLength()), 
					 strUser.GetBuffer(strUser.GetLength()), 
					 strPwd.GetBuffer(strPwd.GetLength()), 
					 nPort,
					 strCmd.GetBuffer(strCmd.GetLength()), 
					 szReturn);


	CString strTmp=szReturn;
	MakeCharByString(szReturn,nSize,strTmp);

	return TRUE;
}

bool telnetValue(char *server, char *uid, char *pwd, int port, char* cmd,char *szReturn)
{
	WSA_Init();
	bool bet = true;
//	char cmd[128] = "/usr/bin/vmstat 3 2";
	int hSocket = telnet_socket(server, uid, pwd, port);
	if (hSocket<0)
	{
		printf("connect error %d\n",hSocket);
		bet = false;
		goto err;
	}

	char tempReturn[100000] = {0};
	if (telnet_command_long(hSocket, cmd, tempReturn) < 0)
	{
		printf("telnet_command_long error %d\n",hSocket);
		bet = false;
		goto err;
	}
	printf("############################################# = %d\n", strlen(tempReturn));

	if (strlen(tempReturn)<4)
	{
		strncpy(szReturn,tempReturn,strlen(tempReturn)); 
	}
	else
	{
		strncpy(szReturn,tempReturn,strlen(tempReturn)-4); //去除"DF\r\n"字符
	}
	printf("====================================================\n");
	printf("szReturn = %s\n",szReturn);
	printf("====================================================\n");
err:
	close_telnet(hSocket);
	printf("telnetValue() close_telnet...\n");
	return bet;
}

extern "C" __declspec(dllexport) 
bool test_telnet(char *server, char *uid, char *pwd, int port, char *szReturn)
{
	WSA_Init();
	bool bet = true;
	char cmd[128] = "/usr/bin/vmstat 3 2";
	int hSocket = telnet_socket(server, uid, pwd, port);
	if (hSocket<0)
	{
		printf("connect error %d\n",hSocket);
		bet = false;
		goto err;
	}

	if (telnet_command_long(hSocket, cmd, szReturn) < 0)
	{
		printf("telnet_command_long error %d\n",hSocket);
		bet = false;
		goto err;
	}

err:
	close_telnet(hSocket);
	WSA_Free();

	return bet;
}


extern "C" __declspec(dllexport) 
bool AIXERRPT_TEL(char *server, char *uid, char *pwd, int port, char *szReturn)
{
	char *pserver = NULL; int pport = 0; char *puid = NULL; char *ppwd = NULL;
	char *lprom = "ogin:"; char *pprom = "assword:"; char *prom = "# $ >"; char *inifile = "AIX";
	int ngid = 0; char* strmid = NULL; char *custpath = NULL;
	

	//	_LoginPrompt=ogin:   lprom
	// _PWPrompt=assword:    pprom
	// _Prompt=# $ >         prom
	//	_OsType=AIX          inifile
	//	_MonitorID=1.26.143.3  strmid

	// _PWPrompt=assword:
	// _Prompt=# $ >
	//	sv_disable=
	//	sv_description=
	//	sv_devicetype=_unix
	//	sv_name=192.168.0.68(Unix)
	//	sv_network=false
	//	sv_dependscondition=3
	//	_PriKeyPath=
	//	_ProtocolType=1
	//	_OsType=AIX
	//	_Port=22
	//	_UserAccount=root
	//	_MachineName=192.168.0.68
	//	sv_dependson=
	//	_PassWord=rootroot
	//	_LoginPrompt=ogin:
	//  _TemplateID=430
	//	_MonitorID=1.26.143.3

	WSA_Init();

	int		ret = 0;
	int		hSocket = 0;
	bool	bResult = TRUE;
	char	cmdBuffer[256] = {0};


	int		c = 0;
	char	*ca = prom, *cb = NULL;
	char	arprompt[PR_NUM][256];
	memset(arprompt, 0, sizeof(arprompt));

	LPGDATA pgd=new GDATA;
	memset(pgd,0,sizeof(GDATA));
	while(cb = strchr(ca, 0x20))
	{
		if(ca == cb) 
		{
			ca ++;
			continue;
		}

		strncpy(arprompt[c++], ca, cb - ca);
		ca = cb + 1;
	}
	strcpy(arprompt[c++], ca);
	//------------------------------------

	hSocket = telnet_init(server, port, pserver, pport, puid, ppwd);

	if(hSocket <= 0)
	{
		if(hSocket == -1)
			sprintf(szReturn, "error=%s", "连接失败：域名解析");
		else if(hSocket == -2)
			sprintf(szReturn, "error=%s", "连接失败：通信初始化");
		else if(hSocket == -3)
			sprintf(szReturn, "error=%s", "连接失败：连接超时");
		else if(hSocket == -4)
			sprintf(szReturn, "error=%s", "代理服务器连接失败");
		else if(hSocket == -5)
			sprintf(szReturn, "error=%s", "代理服务器错误");
		else 
			sprintf(szReturn, "error=%s", "连接失败：未知错误");

		bResult = FALSE;
		goto err;
	}

	Tel_Param param;
	if((ret = telnet_connect(hSocket, uid, pwd, lprom, pprom, arprompt,pgd,&param)) < 0)
	{
		if(ret == -1)
			sprintf(szReturn, "error=%s", "登录失败：读数据超时");
		else if(ret == -2)
			sprintf(szReturn, "error=%s", "登录失败：读数据错误");
		else if(ret == -3)
			sprintf(szReturn, "error=%s", "登录失败：查找提示符");
		else if(ret == -4)
			sprintf(szReturn, "error=%s", "登录失败：查找登录提示符");
		else if(ret == -5)
			sprintf(szReturn, "error=%s", "登录失败：查找密码提示符");
		else 
			sprintf(szReturn, "error=%s", "登录失败：未知错误");

		bResult = FALSE;
		goto err;
	}

	strcpy(cmdBuffer,"/usr/bin/errpt");

	if((ret = telnet_command(hSocket, cmdBuffer,pgd)) < 0)
	{
		bResult = FALSE;
		goto err;
	}

	printf("pgd->databuffer = %s\n", pgd->databuffer);


	strcpy(szReturn, pgd->databuffer);

err:
	WSA_Free();
	shutdown(hSocket, 0x02);
	closesocket(hSocket);
	delete pgd;
	return bResult;
}


