// wmi.cpp : 定义 DLL 应用程序的入口点。
//

#include "monitor.h"

#include "cpu.h"
#include "directory.h"
#include "disk.h"
#include "memory.h"
#include "network.h"
#include "process.h"
#include "service.h"


#if _DEBUG
#define _CRTDBG_MAP_ALLOC
#include "stdlib.h"
#include <crtdbg.h>
#endif

extern "C" {

#include "erl_nif.h"

	static int enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf, int size)
	{
		ERL_NIF_TERM head, tail;
		int val,count = 0;

		while (enif_get_list_cell(env, list, &head, &tail)) 
		{
			if(!enif_get_int(env, head, &val)) 
			{
				return 0;
			}

			*buf = (char)val;
			buf++;
			if(++count >= size)
			{
				return 0;
			}

			list = tail; 
		}
		*buf = '\0';
		return 1;
	}


	static int parse(ErlNifEnv* env, ERL_NIF_TERM parameter, char* buffer, int length, int size)
	{
		ERL_NIF_TERM list, head, tail;

		int count = 0;

		char* p = buffer;

		list = parameter;

		while(enif_get_list_cell(env, list, &head, &tail) && count < length)
		{
			enif_get_string(env, head, p, size);
			p += size;
			count++;
			list = tail;
		}

		return count;
	}

	static ERL_NIF_TERM dispatch(ErlNifEnv* env, PWMI_LOGIN_INFO login, ERL_NIF_TERM parameter)
	{
		ERL_NIF_TERM list, head, tail;

		int result;

		int type;

		int count = 0;

		char p[][256] = {{0},{0},{0},{0},{0},{0}};

		char buffer1[4096] = {0};
		char buffer2[4096] = {0};
		BOOL flag = FALSE;

		list = parameter;

		//*****************************************************wmi operation type***********************************************
		result = enif_get_list_cell(env, list, &head, &tail);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get wmi operation type term error"));
		}

		result = enif_get_int(env, head, &type);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get wmi operation type int error"));
		}

		count = parse(env, tail, (char*)p, 6, 256);

		//Com init
		CoInitialize(NULL);

		switch((WMI_OPERATION_TYPE)type)
		{
		case cpu:
			if(count == 1)
			{
				flag = GetAllCPURate(login, buffer1);
			}
			else
			{
				flag = GetAllCPURate(login, buffer1);
			}
			break;
		case memory:
			if(count == 1)
			{
				flag = GetMemoryInfo(login, buffer1);
			}
			else
			{
				flag = GetMemoryInfo(login, buffer1);
			}
			break;
		case disk:
			if(count == 1)
			{
				flag = GetDiskInfo(login, p[0], buffer1);
			}
			else
			{
				flag = EnumDisksInfo(login, buffer1);
			}
			break;
		case service:
			if(count == 1)
			{
				flag = GetServiceInfo(login, p[0], buffer1);
			}
			else
			{
				flag = GetNTServices(login, buffer1);
			}
			break;
		case process:
			if(count == 1)
			{
				flag = GetProcessInfo(login, p[0], buffer1);
			}
			else
			{
				flag = EnumProcessInfo(login, buffer1);
			}
			break;
		case network:
			if(count == 1)
			{
				flag = GetNetWorkInfo(login, p[0], buffer1);
			}
			else
			{
				flag = EnumNetWorks(login, buffer1);
			}
			break;
		case directory:
			if(count == 3)
			{
				flag = GetDirectoryInfo(login, p[0], p[1], p[2], buffer1);
			}
			break;
		default:
			break;
		}

		//Com destory
		CoUninitialize();

		if(flag)
		{
			if(AnsiToUtf8(buffer1, buffer2, sizeof(buffer2)))
			{
				return enif_make_tuple(env, 2, enif_make_atom(env, "ok"), enif_make_string(env, buffer2));
			}
			else
			{
				return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "convert ok result error"));
			}
		}
		else
		{
			if(AnsiToUtf8(buffer1, buffer2, sizeof(buffer2)))
			{
				return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, buffer2));
			}
			else
			{
				return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "convert error result error"));
			}
		}

	}

	static ERL_NIF_TERM execute(ErlNifEnv* env, ERL_NIF_TERM arg)
	{

#if _DEBUG
		_CrtMemState Sh1,Sh2,Sh_Diff;
#endif

		ERL_NIF_TERM list, head, tail, parameter;

		int result = 0;
		ERL_NIF_TERM temp;

		WMI_LOGIN_INFO login;

#if _DEBUG
		_CrtSetDbgFlag(_CrtSetDbgFlag(_CRTDBG_REPORT_FLAG) | _CRTDBG_LEAK_CHECK_DF);
		_CrtMemCheckpoint(&Sh1);
#endif

		memset(&login, 0, sizeof(WMI_LOGIN_INFO));

		list = arg;

		//*****************************************************os***********************************************
		result = enif_get_list_cell(env, list, &head, &tail);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get os error"));
		}

		result = enif_get_int(env, head, &login.os);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get os error"));
		}

		//*****************************************************machine***********************************************
		list = tail;
		result = enif_get_list_cell(env, list, &head, &tail);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get machine error"));
		}

		result = enif_get_string(env, head, login.machine, BUFFER_SIZE);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get machine error"));
		}

		//*******************************************************user*************************************************
		list = tail;
		result = enif_get_list_cell(env, list, &head, &tail);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get user error"));
		}

		result = enif_get_string(env, head, login.user, BUFFER_SIZE);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get user error"));
		}

		//*****************************************************password************************************************
		list = tail;
		result = enif_get_list_cell(env, list, &head, &tail);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get password error"));
		}

		result = enif_get_string(env, head, login.password, BUFFER_SIZE);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get password error"));
		}

		//*****************************************************parameter************************************************
		list = tail;
		result = enif_get_list_cell(env, list, &parameter, &tail);
		if(!result)
		{
			return enif_make_tuple(env, 2, enif_make_atom(env, "error"), enif_make_string(env, "get parameter error"));
		}

		temp = dispatch(env, &login, parameter);

#if _DEBUG
		_CrtMemCheckpoint(&Sh2);
		_CrtMemDifference(&Sh_Diff, &Sh1, &Sh2);
		_CrtMemDumpAllObjectsSince(&Sh_Diff);
#endif

		return temp;
	}

	int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
	{
		//CoInitialize(NULL);
		return 0;
	}

	void unload(ErlNifEnv* env, void* priv_data) 
	{
		//CoUninitialize();
	}

	static ErlNifFunc nif_funcs[] =
	{
		{"execute", 1, execute}
	};

	ERL_NIF_INIT(wmiproxy,nif_funcs,load,NULL,NULL,unload) 

}