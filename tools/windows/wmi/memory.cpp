#include "memory.h"


int GetMemoryInfo(PWMI_LOGIN_INFO login, char* buffer)
{

	string strResult = "";
	WbemScripting::ISWbemServicesPtr services;
	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}
	try
	{
		WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("CIM_OperatingSystem", WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY, NULL);
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

		double fTotalPhyMem=0, fFreePhyMem=0, fTotalVirMem =0, fFreeVirMem=0;
		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;

			WbemScripting::ISWbemPropertyPtr prop = properties->Item("TotalVisibleMemorySize",0);
			fTotalPhyMem = ((double)prop->GetValue())/1024;
			prop.Release();

			prop = properties->Item("TotalVirtualMemorySize",0);
			fTotalVirMem = ((double)prop->GetValue())/1024;
			prop.Release();

			prop = properties->Item("FreePhysicalMemory",0);
			fFreePhyMem = ((double)prop->GetValue())/1024;
			prop.Release();

			prop = properties->Item("FreeVirtualMemory",0);
			fFreeVirMem = ((double)prop->GetValue())/1024;
			prop.Release();

			memset(temp, 0, 1024);
			sprintf(temp, "TotalPhyMem=%.2f$TotalVirMemory=%.2f$FreePhyMem=%.2f$FreeVirMem=%.2f$PhyMemUsage=%.2f$PercentUsed=%.2f$", 
				fTotalPhyMem, fTotalVirMem, fFreePhyMem, fFreeVirMem, (fTotalPhyMem-fFreePhyMem)/fTotalPhyMem*100, 
				(fTotalVirMem-fFreeVirMem)/fTotalVirMem*100);

			strResult = temp;

			properties.Release();
			object.Release();
			VariantClear(&var);

		}

		obj_enum.Release();
		obj_enum = NULL;
		objects.Release();
		objects = NULL;


		//取第一次错误页面处理数据
		objects = services->InstancesOf("Win32_PerfRawData_PerfOS_Memory", WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY, NULL);
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

		double fPagesPersec1=0, fTimestamp_PerfTime1=0, fPagesPersec2=0, fTimestamp_PerfTime2=0, fFrequency_PerfTime=0;
		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;

			WbemScripting::ISWbemPropertyPtr prop = properties->Item("PagesPersec",0);
			fPagesPersec1 = (double)prop->GetValue();
			prop.Release();

			prop = properties->Item("Timestamp_PerfTime",0);
			fTimestamp_PerfTime1 = (double)prop->GetValue();
			prop.Release();

			properties.Release();		
			object.Release();
			VariantClear(&var);

		}

		obj_enum.Release();
		obj_enum = NULL;
		objects.Release();
		objects = NULL;

		//取第二次错误页面处理数据
		objects = services->InstancesOf("Win32_PerfRawData_PerfOS_Memory", WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY, NULL);
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

			WbemScripting::ISWbemPropertyPtr prop = properties->Item("PagesPersec",0);
			fPagesPersec2 = (double)prop->GetValue();
			prop.Release();

			prop = properties->Item("Timestamp_PerfTime",0);
			fTimestamp_PerfTime2 = (double)prop->GetValue();
			prop.Release();

			prop = properties->Item("Frequency_PerfTime",0);
			fFrequency_PerfTime = (double)prop->GetValue();
			prop.Release();

			properties.Release();
			object.Release();
			VariantClear(&var);

		}

		double fPagesPersec = (fPagesPersec2-fPagesPersec1)/((fTimestamp_PerfTime2-fTimestamp_PerfTime1)/fFrequency_PerfTime);
		memset(temp, 0, 1024);
		sprintf(temp,"PagesPerSec=%.2f$", fPagesPersec);
		strResult += temp;

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