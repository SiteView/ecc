#include "process.h"


int EnumProcessInfo(PWMI_LOGIN_INFO login, char* buffer)
{

	string strResult = "";
	WbemScripting::ISWbemServicesPtr services;
	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}

	try
	{
		string strQuery = "SELECT * FROM Win32_PerfRawData_PerfProc_Process where Name<>'_Total'"; 
		WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(strQuery.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL); 		
		if(NULL == objects) 
		{
			services.Release();
			return 0;
		}

		IEnumVARIANTPtr obj_enum = objects->Get_NewEnum();
		if(NULL == obj_enum) 
		{
			services.Release();
			objects.Release();
			return 0;
		}

		ULONG fetched; 
		VARIANT var; 
		char temp[1024] = {0};
		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("Name",0);
			_variant_t value = prop->GetValue();

			memset(temp, 0, 1024);
			sprintf(temp, "%s$", (const char*)_bstr_t(value));
			strResult += temp;

			prop.Release();
			properties.Release();
			object.Release();
			VariantClear(&var);
		}

		if(NULL != obj_enum)
		{
			obj_enum.Release();
			obj_enum = NULL;
		}

		if(NULL != objects) 
		{
			objects.Release();
			objects = NULL;
		}

		if(NULL != services)
		{
			services.Release();
			services = NULL;
		}

	}
	catch (_com_error err) 
	{ 
		IErrorInfo * ei = err.ErrorInfo();
		if(ei)
		{
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			char* pstr = _com_util::ConvertBSTRToString(strDesEI);
			sprintf(buffer, "error=Error ocured:%x: %s", (unsigned)err.Error(), pstr);

			ei->Release();
			free(pstr);
			::SysFreeString(strDesEI);
		}
		else
		{
			sprintf(buffer, "error=Error ocured:%x: %s", (unsigned)err.Error(), err.ErrorMessage());
		}

		if(services != NULL)
		{
			services.Release();
			services = NULL;
		}

		return 0;
	} 
	catch(...) 
	{ 
		sprintf(buffer, "error=Error ocured: %d",::GetLastError());

		if(services != NULL)
		{
			services.Release();
			services = NULL;
		}

		return 0;
	} 

	strcpy(buffer, strResult.c_str());
	return 1;
}


int GetProcessInfo(PWMI_LOGIN_INFO login, const char * strProcessName, char* buffer)
{

	string strResult = "";
	WbemScripting::ISWbemServicesPtr services;
	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}
	try
	{

		//取第一次进程CPU占用率数据
		string strQuery;
		char temp[1024] = {0};
		sprintf(temp, "SELECT * FROM Win32_PerfRawData_PerfProc_Process where Name='%s'", strProcessName); 
		strQuery = temp;

		WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(strQuery.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL); 

		if(NULL == objects) 
		{
			services.Release();
			return 0;
		}

		IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 

		if(NULL == obj_enum) 
		{
			services.Release();
			objects.Release();
			return 0;
		}


		ULONG fetched; 
		VARIANT var; 
		int nThreadCount = 0;	
		int nCount(0);

		float fPercentProcessorTime=0, fWorkingsetsize=0, fPrivateBytes=0, fPercentProcessorTime1=0, fPercentProcessorTime2=0;
		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			nCount++;

			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("PercentProcessorTime",0);
			fPercentProcessorTime1 += (float)prop->GetValue()/100000;

			prop.Release();
			properties.Release();
			object.Release();
			VariantClear(&var);

		}

		obj_enum.Release();
		obj_enum = NULL;
		objects.Release();
		objects = NULL;


		Sleep(1000);
		//取第二次进程CPU占用率数据
		objects = services->ExecQuery(strQuery.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL); 

		if(NULL == objects) 
		{
			services.Release();
			return 0;
		}

		obj_enum = objects->Get_NewEnum(); 

		if(NULL == obj_enum) 
		{
			services.Release();
			objects.Release();
			return 0;
		}

		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("ThreadCount",0);
			nThreadCount += (int)prop->GetValue(); 
			prop.Release();

			prop = properties->Item("PercentProcessorTime",0);
			fPercentProcessorTime2 += (float)prop->GetValue()/100000;
			prop.Release();

			prop = properties->Item("WorkingSet",0);
			fWorkingsetsize += (float)prop->GetValue()/1024;
			prop.Release();

			prop = properties->Item("PrivateBytes",0);
			fPrivateBytes += (float)prop->GetValue()/1024;
			prop.Release();

			properties.Release();
			object.Release();
			VariantClear(&var);

		}

		fPercentProcessorTime = fPercentProcessorTime2 - fPercentProcessorTime1;
		if(fPercentProcessorTime < 0)
		{
			fPercentProcessorTime = 0;
		}
		if(fPercentProcessorTime > 100)
		{
			fPercentProcessorTime = 100;
		}


		obj_enum.Release();
		obj_enum = NULL;
		objects.Release();
		objects = NULL;


		float fTotalPhyMem=0, fTotalVirMem=0;
		objects = services->InstancesOf("CIM_OperatingSystem", WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY, NULL);

		if(NULL == objects) 
		{
			services.Release();
			return 0;
		}

		obj_enum = objects->Get_NewEnum(); 

		if(NULL == obj_enum) 
		{
			services.Release();
			objects.Release();
			return 0;
		}

		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;

			WbemScripting::ISWbemPropertyPtr prop = properties->Item("TotalVisibleMemorySize",0);
			fTotalPhyMem = (float)prop->GetValue();
			prop.Release();

			prop = properties->Item("TotalVirtualMemorySize",0);
			fTotalVirMem = (float)prop->GetValue();
			prop.Release();

			properties.Release();
			object.Release();
			VariantClear(&var);

		}

		memset(temp, 0, 1024);
		sprintf(temp, "ProcessCount=%d$ThreadCount=%d$PercentProcessorTime=%.2f$WorkingSet=%.2f$\
						 MemUtilization=%.2f$PrivateBytes=%.2f$VirUtilization=%.2f$",
						 /*objects->GetCount()*/nCount, nThreadCount, fPercentProcessorTime, fWorkingsetsize, fWorkingsetsize/fTotalPhyMem*100,
						 fPrivateBytes, fPrivateBytes/fTotalVirMem*100);
		strResult = temp;

		if(NULL != obj_enum)
		{
			obj_enum.Release();
			obj_enum = NULL;
		}

		if(NULL != objects) 
		{
			objects.Release();
			objects = NULL;
		}

		if(NULL != services)
		{
			services.Release();
			services = NULL;
		}

	}
	catch (_com_error err) 
	{ 
		IErrorInfo * ei = err.ErrorInfo();
		if(ei)
		{
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			char* pstr = _com_util::ConvertBSTRToString(strDesEI);
			sprintf(buffer, "error=Error ocured:%x: %s", (unsigned)err.Error(), pstr);

			ei->Release();
			free(pstr);
			::SysFreeString(strDesEI);
		}
		else
		{
			sprintf(buffer, "error=Error ocured:%x: %s", (unsigned)err.Error(), err.ErrorMessage());
		}

		if(services != NULL)
		{
			services.Release();
			services = NULL;
		}

		return 0;
	} 
	catch(...) 
	{ 
		sprintf(buffer, "error=Error ocured: %d",::GetLastError());

		if(services != NULL)
		{
			services.Release();
			services = NULL;
		}

		return 0;
	} 

	strcpy(buffer, strResult.c_str());
	return 1;
}