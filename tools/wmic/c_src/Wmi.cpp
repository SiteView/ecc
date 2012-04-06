
#include "stdafx.h"
#include ".\wmi.h"



#pragma comment(lib,"WbemUuid.Lib")

static bool bInitialize = false;

void initialize()
{
	if(!bInitialize)
	{
		HRESULT hres;  

		hres =  CoInitializeEx(0, COINIT_MULTITHREADED); 
		if (FAILED(hres))
		{
			cout << "Failed to initialize COM library. Error code = 0x" <<  hres << endl;
	       
		}

		hres =  CoInitializeSecurity(
			NULL, 
			-1,                          // COM authentication
			NULL,                        // Authentication services
			NULL,                        // Reserved
			RPC_C_AUTHN_LEVEL_DEFAULT,   // Default authentication 
			RPC_C_IMP_LEVEL_IMPERSONATE, // Default Impersonation  
			NULL,                        // Authentication info
			EOAC_NONE,                   // Additional capabilities 
			NULL                         // Reserved
			);

	                      
		if (FAILED(hres))
		{
			cout << "Failed to initialize security. Error code = 0x" <<  hres << endl;
			CoUninitialize();
	       
		}
		bInitialize = true;
	}
	

}
CWmi::CWmi(void)
{
	//CoInitialize(NULL);
	memset(szMess,0,256);

}

CWmi::~CWmi(void)
{
	//CoUninitialize();
}


bool CWmi::Open(ei_x_buff *x,char *server,char *username,char *password)
{
	try 
	{ 
		
		WbemScripting::ISWbemLocatorPtr locator; 
		locator.CreateInstance(WbemScripting::CLSID_SWbemLocator);
		
		if(locator != NULL) 
		{ 
			if (strcmp(server,"") == 0) 
			{
				ei_x_encode_atom(x, "error");		
				ei_x_encode_string(x, "IP Address error!");  
				return false;
			}
			if ((strcmp(server,"localhost") == 0) || (strcmp(server,"127.0.0.1") == 0))
			{
				services = locator->ConnectServer(".","root\\cimv2","","","","",0,NULL);				
			}
			else 
			{
				services = locator->ConnectServer(server,"root\\cimv2",username,password,"","",0,NULL); 				
			}
			locator.Release();
		}
	}
	catch (_com_error err) 
	{ 
		/*IErrorInfo * ei = err.ErrorInfo();
		BSTR strDesEI;
		ei->GetDescription(&strDesEI);*/
		/*char* pstr = _com_util::ConvertBSTRToString(strDesEI);
		sprintf(szMess,"error=Error ocured:%x: %s", (unsigned)err.Error(), pstr);
		printf("error=Error ocured:%x: %s", (unsigned)err.Error(), pstr);*/

		
		sprintf(szMess,"error=Error ocured"); 
		//printf("error=Error ocured:%", (unsigned)err.Error());


		ei_x_encode_atom(x, "error");		
		ei_x_encode_string(x, szMess);     
		/*ei->Release();
		free(pstr);
		::SysFreeString(strDesEI);*/
		return false;
	} 
	catch(...) 
	{ 
		//printf("error=Error ocured: %d",::GetLastError());
		sprintf(szMess,"error=Error ocured");
		ei_x_encode_atom(x, "error");		
		ei_x_encode_string(x, szMess);
		return false;
	} 
	return true;
} 

DWORD CWmi::MyVariantToBSTR(VARIANT *out_pvarDes,VARIANT *in_pvarSrc)
{
	DWORD dwResult=1;
	OLECHAR *sOleText=new OLECHAR[256];
	if(in_pvarSrc&&out_pvarDes)
	{
		VariantInit(out_pvarDes);
		if(in_pvarSrc->vt>0xfff)
		{
			dwResult=0;
			out_pvarDes->vt=VT_BSTR;
			mbstowcs(sOleText,_T("ArrayType"),strlen(_T("ArrayType"))+1);
			out_pvarDes->bstrVal=SysAllocString(sOleText);
		}else if(in_pvarSrc->vt==VT_BSTR)
		{
			VariantCopyInd(out_pvarDes,in_pvarSrc);
			dwResult=0;
		}else if(in_pvarSrc->vt==VT_NULL)
		{
			dwResult=0;
			out_pvarDes->vt=VT_BSTR;
			mbstowcs(sOleText,_T("Null"),strlen(_T("Null"))+1);
			out_pvarDes->bstrVal=SysAllocString(sOleText);
		}else
		{
			VariantChangeType(out_pvarDes,in_pvarSrc,0,VT_BSTR);			
			dwResult=0;
		}		
	}
	return dwResult;
}

bool CWmi::Execute(ei_x_buff *x,char* wql)
{	
	try
	{
		//printf("\r\n\r\n===========%s===========\r\n", wql);

		WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(wql,"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL); 
		if(NULL == objects) 
		{
			
			ei_x_encode_tuple_header(x, 2);	
			ei_x_encode_atom(x, "error");		
		    ei_x_encode_string(x, "ExecQuery Error!");
			
			return false;
		}

		IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
		if(NULL == obj_enum) 
		{
			objects.Release();
			ei_x_encode_tuple_header(x, 2);	
			ei_x_encode_atom(x, "error");		
		    ei_x_encode_string(x, "Get_NewEnum Error!");
			return false;
		}

		ULONG fetched1; 
		VARIANT var1;  
		
		ei_x_encode_tuple_header(x, 2);	
		ei_x_encode_atom(x, "ok");
        int count = 0;
		//printf("\r\n\r\n===========%s===========\r\n", wql);
		while (obj_enum->Next(1,&var1,&fetched1) == S_OK) 
		{ 
			
			WbemScripting::ISWbemObjectPtr object = var1;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;

			IEnumVARIANTPtr pro_enum=properties->Get_NewEnum(); 
			if(NULL == pro_enum)
			{
				continue;
			}
			//printf("\r\n\r\n===========%d===========\r\n", properties->Count);
			ei_x_encode_list_header(x,1);
			ei_x_encode_tuple_header(x,properties->Count);
			ULONG fetched2; 
			VARIANT var2; 	
			 count++;
			while(pro_enum->Next(1,&var2,&fetched2) == S_OK) 
			{ 
				WbemScripting::ISWbemPropertyPtr prop=var2.punkVal; 
				_bstr_t name=prop->GetName(); 
				_variant_t value=prop->GetValue(); 

                ei_x_encode_tuple_header(x, 2);	
				ei_x_encode_atom(x, name);				
                //printf("---->%s=%d\r\n",  (const char*)name, value.vt);
				switch(value.vt)
				{
					case VT_BOOL:
						if (value.boolVal)
						{
							ei_x_encode_atom(x, "True");
						}
						else
						{
							ei_x_encode_atom(x, "False");
						}
						break;
					case VT_ARRAY: 
						ei_x_encode_atom(x, "ArrayType");
						break;
					case VT_I4: 						
					case VT_I2: 					
						ei_x_encode_ulong(x,value.uintVal);
						/*ei_x_encode_long(x,value.intVal);*/
						break;
					case 8204:
						ei_x_encode_atom(x, "ArrayType");
						break;
					case VT_NULL:
					case VT_EMPTY: 
						ei_x_encode_atom(x, "undefined");
					break;
					case VT_BSTR:	
						{
							value.ChangeType(VT_BSTR);
							_bstr_t bs(value);
							::std::string  strTemp;						
							strTemp = std::string(static_cast<const char*>(bs));
							//printf("%s  ",strTemp.c_str());
							ei_x_encode_binary(x, strTemp.c_str(),strTemp.length());
						}
						break;
					default:	
						{
							printf("---->%s=%d\r\n",  (const char*)name, value.vt);
							value.ChangeType(VT_BSTR);
							_bstr_t bs(value);
							::std::string  strTemp;						
							strTemp = std::string(static_cast<const char*>(bs));
							//printf("%s  ",strTemp.c_str());
							ei_x_encode_binary(x, strTemp.c_str(),strTemp.length());
						}
				} 
				prop.Release();
				VariantClear(&var2);

			}			

			if(NULL != pro_enum)
			{
				pro_enum.Release();
				pro_enum = NULL;
			}

			properties.Release();
			object.Release();
			VariantClear(&var1);

		}

		//printf("\r\n\r\ncount===========%d===========\r\n", count);
		if (count == 0)
		{
			ei_x_encode_atom(x, "empty");
		}
		else
		{
			ei_x_encode_empty_list(x);
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

		

	}
	catch (_com_error err) 
	{ 
		/*IErrorInfo * ei = err.ErrorInfo();*/
		//if(ei)
		//{
		//	BSTR strDesEI;
		//	ei->GetDescription(&strDesEI);
		//	/*char* pstr = _com_util::ConvertBSTRToString(strDesEI);*/
		//	/*printf("error=Error ocured:%x: %s", (unsigned)err.Error(), pstr);
		//	sprintf(szMess,"error=Error ocured:%x: %s", (unsigned)err.Error(), pstr);*/
		//	sprintf(szMess,"error=Error ocured:%x", (unsigned)err.Error());
		//	printf("error=Error ocured:%", (unsigned)err.Error());
		//	ei_x_encode_tuple_header(x, 2);	
		//	ei_x_encode_atom(x, "error");		
		//	ei_x_encode_string(x, szMess);
		//	ei->Release();
		//	free(pstr);
		//	::SysFreeString(strDesEI);
		//}
		//else
		//{
			//printf("error=Error ocured:%x: %s", (unsigned)err.Error(), err.ErrorMessage());
			sprintf(szMess,"error=Error ocured");
			ei_x_encode_tuple_header(x, 2);	
			ei_x_encode_atom(x, "error");		
			ei_x_encode_string(x, szMess);
		/*}*/

		return false;
	} 
	catch(...) 
	{ 
		//printf("error=Error ocured: %d",::GetLastError());
		sprintf(szMess,"error=Error ocured");
		ei_x_encode_tuple_header(x, 2);	
		ei_x_encode_atom(x, "error");		
		ei_x_encode_string(x, szMess);

		return false;
	} 
	return true;
}

void CWmi::Close(void)
{
	services.Release();
	services = NULL;
}
