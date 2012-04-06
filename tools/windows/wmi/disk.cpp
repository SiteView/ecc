#include "disk.h"

int EnumDisksInfo(PWMI_LOGIN_INFO login, char* buffer)
{

	string strResult = "";
	WbemScripting::ISWbemServicesPtr services;
	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}
	try
	{
		_bstr_t strQuery ="SELECT * FROM Win32_LogicalDisk where MediaType=12"; 
		WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(strQuery,"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL); 			
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
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("DeviceID",0);

			memset(temp, 0, 1024);
			sprintf(temp,"%s$", (const char*)_bstr_t(prop->GetValue()));
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


int GetDiskInfo(PWMI_LOGIN_INFO login, const char* disk, char* buffer)
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
		sprintf(temp, "SELECT * FROM Win32_LogicalDisk where MediaType=12 and DeviceID='%s'", disk);
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
			objects.Release();
			services.Release();
			return 0;
		}

		ULONG fetched; 
		VARIANT var; 
		float nSize = 0, nFreeSize =0;
		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;

			WbemScripting::ISWbemPropertyPtr prop = properties->Item("Size",0);
			_variant_t value = prop->GetValue();
			prop.Release();

			nSize = (float)value;		

			prop = properties->Item("FreeSpace",0);
			value = prop->GetValue();
			prop.Release();

			nFreeSize = (float)value;	

			memset(temp, 0, 1024);
			sprintf(temp, "percentFull=%.2f$Mbfree=%.2f$TotalSize=%.2f$",
				((nSize - nFreeSize) / nSize) * 100 , nFreeSize/1048576, nSize/1048576);

			strResult = temp;

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