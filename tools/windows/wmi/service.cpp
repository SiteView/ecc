#include "service.h"


int GetNTServices(PWMI_LOGIN_INFO login, char* buffer)
{

	string strResult = "";
	WbemScripting::ISWbemServicesPtr services;
	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}
	try
	{
		WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("Win32_Service", WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY, NULL);
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
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("DisplayName",0);

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


int GetServiceInfo(PWMI_LOGIN_INFO login, const char *servicename, char* buffer)
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
		sprintf(temp, "SELECT * FROM Win32_Service where DisplayName='%s'", servicename);
		strQuery = temp;

		WbemScripting::ISWbemObjectSetPtr objects1 = services->ExecQuery(strQuery.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL); 
		if(NULL == objects1) 
		{
			services.Release();
			return 0;
		}

		IEnumVARIANTPtr obj_enum1 = objects1->Get_NewEnum(); 
		if(NULL == obj_enum1) 
		{
			objects1.Release();
			services.Release();
			return 0;
		}

		ULONG fetched1; 
		VARIANT var1; 
		int nProecssID = 0;
		while (obj_enum1->Next(1,&var1,&fetched1) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object1 = var1;
			WbemScripting::ISWbemPropertySetPtr properties1 = object1->Properties_;
			WbemScripting::ISWbemPropertyPtr prop1 = properties1->Item("ProcessId",0);
			nProecssID = (int)prop1->GetValue();
			prop1.Release();

			if(nProecssID == 0)
			{
				strResult = "Processes=0$Started=0$ProcessName=NA$";
			}
			else
			{
				memset(temp, 0, 1024);
				sprintf(temp, "SELECT * FROM Win32_PerfRawData_PerfProc_Process where IDProcess=%d", nProecssID);
				strQuery = temp;

				WbemScripting::ISWbemObjectSetPtr objects2 = services->ExecQuery(strQuery.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL); 
				if(NULL == objects2) 
				{
					obj_enum1.Release();
					objects1.Release();
					VariantClear(&var1);

					services.Release();
					return 0;
				}

				IEnumVARIANTPtr obj_enum2 = objects2->Get_NewEnum(); 					
				if(NULL == obj_enum2) 
				{
					objects2.Release();

					obj_enum1.Release();
					objects1.Release();
					VariantClear(&var1);

					services.Release();
					return 0;
				}

				ULONG fetched2; 
				VARIANT var2;

				while (obj_enum2->Next(1,&var2,&fetched2) == S_OK) 
				{ 
					WbemScripting::ISWbemObjectPtr object2 = var2;
					WbemScripting::ISWbemPropertySetPtr properties2 = object2->Properties_;
					WbemScripting::ISWbemPropertyPtr prop2 = properties2->Item("Name",0);

					memset(temp, 0, 1024);
					sprintf(temp, "Processes=1$Started=1$ProcessName=%s$", (const char*)_bstr_t(prop2->GetValue()));
					strResult = temp;

					prop2.Release();
					properties2.Release();
					object2.Release();
					VariantClear(&var2);

				}

				obj_enum2.Release();
				obj_enum2 = NULL;
				objects2.Release();
				objects2 = NULL;

			}

			prop1 = properties1->Item("State",0);
			memset(temp, 0, 1024);
			sprintf(temp, "State=%s$", (const char*)_bstr_t(prop1->GetValue()));
			prop1.Release();
			strResult += temp;

			prop1 = properties1->Item("Status",0);
			memset(temp, 0, 1024);
			sprintf(temp, "Status=%s$", (const char*)_bstr_t(prop1->GetValue()));
			prop1.Release();
			strResult += temp;

			properties1.Release();
			object1.Release();
			VariantClear(&var1);

		}

		if(NULL != obj_enum1)
		{
			obj_enum1.Release();
			obj_enum1 = NULL;
		}

		if(NULL != objects1) 
		{
			objects1.Release();
			objects1 = NULL;
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