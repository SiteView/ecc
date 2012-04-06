#include "network.h"


int EnumNetWorks(PWMI_LOGIN_INFO login, char* buffer)
{
	string strResult = "";
	WbemScripting::ISWbemServicesPtr services;
	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}
	try
	{
		WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("Win32_PerfRawData_Tcpip_NetworkInterface", WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY, NULL);	
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

			memset(temp, 0, 1024);
			sprintf(temp, "%s$", (const char*)_bstr_t(prop->GetValue()));
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


int GetNetWorkInfo(PWMI_LOGIN_INFO login, const char* networkname, char* buffer)
{
	string strResult = "";
	WbemScripting::ISWbemServicesPtr services;
	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}
	try
	{

		string strQuery;
		char temp[1024] = {0};
		sprintf(temp,"SELECT * FROM Win32_PerfRawData_Tcpip_NetworkInterface where Name='%s'", networkname);
		strQuery = temp;

			
		//取第一次网络设备数据			
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
		int nPacketsOutboundErrors = 0, nPacketsReceivedErrors = 0; 
		float fBytesReceivedPerSec1 = 0, fBytesSentPerSec1 =0, fTimestamp_PerfTime1 = 0,
			fFrequency_PerfTime = 0;
		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;

			WbemScripting::ISWbemPropertyPtr prop = properties->Item("BytesReceivedPerSec",0);
			fBytesReceivedPerSec1 = (float)prop->GetValue();
			prop.Release();

			prop = properties->Item("BytesSentPerSec",0);
			fBytesSentPerSec1 = (float)prop->GetValue();
			prop.Release();

			prop = properties->Item("Timestamp_PerfTime",0);
			fTimestamp_PerfTime1 = (float)prop->GetValue();
			prop.Release();

			properties.Release();
			object.Release();
			VariantClear(&var);

		}

		obj_enum.Release();
		obj_enum = NULL;
		objects.Release();
		objects = NULL;


		//取第二次网络设备数据
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

		float fBytesReceivedPerSec2 = 0, fBytesSentPerSec2 =0, fTimestamp_PerfTime2 = 0;
		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;

			WbemScripting::ISWbemPropertyPtr prop = properties->Item("BytesReceivedPerSec",0);
			fBytesReceivedPerSec2 = (float)prop->GetValue();
			prop.Release();

			prop = properties->Item("BytesSentPerSec",0);
			fBytesSentPerSec2 = (float)prop->GetValue();
			prop.Release();

			prop = properties->Item("Timestamp_PerfTime",0);
			fTimestamp_PerfTime2 = (float)prop->GetValue();
			prop.Release();

			prop = properties->Item("PacketsOutboundErrors",0);
			nPacketsOutboundErrors = (int)prop->GetValue();
			prop.Release();

			prop = properties->Item("PacketsReceivedErrors",0);
			nPacketsReceivedErrors = (int)prop->GetValue();
			prop.Release();

			prop = properties->Item("Frequency_PerfTime",0);
			fFrequency_PerfTime = (float)prop->GetValue();
			prop.Release();

			properties.Release();
			object.Release();
			VariantClear(&var);

		}
		float fInterval = (fTimestamp_PerfTime2-fTimestamp_PerfTime1)/fFrequency_PerfTime;
		memset(temp, 0, 1024);
		sprintf(temp, "BytesReceivedPerSec=%.2f$BytesSentPerSec=%.2f$PacketsErrors=%d", 
			(fBytesReceivedPerSec2-fBytesReceivedPerSec1)/fInterval, (fBytesSentPerSec2-fBytesSentPerSec1)/fInterval,
			nPacketsOutboundErrors+nPacketsReceivedErrors);
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