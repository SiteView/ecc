#include "cpu.h"


int GetCPURate(PWMI_LOGIN_INFO login, char* buffer)
{
	char temp[1024] = {0};
	map<int, float> lPerProTime1;						//保存第一次获取的PID及其占用CPU时间
	multimap<float,string, greater<float> > resultMap;	//统计结果
	int iPIDNum1=0,iPIDNum2=0;							//PID号
	string strProName ="";								//获取到的进程名列表
	float iTotalRate=0;									//不包含Idle运行的CPU占用时间
	float fTotalTime=0.0;								//包含Idle运行的CPU占用时间
	string strResult = "";
	string strReturn = "detailutilization=";
	string strWQL = "SELECT * FROM Win32_PerfRawData_PerfProc_Process where Name<>'_Total'";
	WbemScripting::ISWbemServicesPtr services;

	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}
	try
	{
		WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(strWQL.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL);
		if(NULL == objects)
		{
			services.Release();
			services = NULL;
			return 0;
		}

		IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
		if(NULL == obj_enum) 
		{
			services.Release();
			services = NULL;
			objects.Release();
			objects = NULL;
			return 0;
		}

		/************************第一次取进程CPU占用率数据************************/
		ULONG fetched; 
		VARIANT var; 

		float fPercentProcessorTime1=0, fPercentProcessorTime2=0;	
		int nRoundNum = 0;
		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			nRoundNum++;

			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("PercentProcessorTime",0);
			fPercentProcessorTime1 = (float)prop->GetValue()/100000;
			prop.Release();

			prop = properties->Item("IDProcess",0);									//第一次获取PID，用于与下一次获取的对比。
			iPIDNum1 = (int)prop->GetValue();
			lPerProTime1[iPIDNum1] = fPercentProcessorTime1;
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

		if(nRoundNum==0)
		{
			sprintf(buffer,"error=%s","WMI库缺少Win32_PerfRawData_PerfProc_Process类");

			if(NULL != services)
			{
				services.Release();
				services = NULL;
			}

			return 0;
		}

		Sleep(1000);
		/************************第二次取进程CPU占用率数据************************/
		objects = services->ExecQuery(strWQL.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL); 
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

		float interval=0;

		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("PercentProcessorTime",0);
			fPercentProcessorTime2 = (float)prop->GetValue()/100000;
			prop = properties->Item("IDProcess",0);									//第二次获取PID
			iPIDNum2 = prop->GetValue();
			prop.Release();

			prop = properties->Item("Name",0);										//获取进程名列表
			strProName = prop->GetValue();
			prop.Release();

			/************************对数据进行处理************************/
			map<int, float>::iterator item = lPerProTime1.find(iPIDNum2);
			if(item != lPerProTime1.end())
			{				
				fPercentProcessorTime1=item->second;
				interval=fPercentProcessorTime2 - fPercentProcessorTime1;
				if(interval!=0)
				{
					if(iPIDNum2!=0)
					{
						iTotalRate+=interval;
						resultMap.insert(make_pair<float,string>(interval , strProName));
					}
					fTotalTime+=interval;						
				}
			}
			else
			{
				if(iPIDNum2!=0)
				{
					iTotalRate+=fPercentProcessorTime2;
					fTotalTime+=fPercentProcessorTime2;
					resultMap.insert(make_pair<float,string>(fPercentProcessorTime2,strProName));
				}
			}

			properties.Release();
			object.Release();

			VariantClear(&var);

		}

		memset(temp, 0, 1024);
		sprintf(temp,"utilization=%0.2f$",iTotalRate*100/fTotalTime);
		strResult = temp;
		strResult += "PerProUse=";

		multimap<float,string,greater<float> >::iterator resultItem = resultMap.begin();
		while(resultItem!=resultMap.end())
		{
			memset(temp, 0, 1024);
			sprintf(temp,"%s:%0.2f$",resultItem->second, (100*(resultItem->first)/fTotalTime));
			strResult+=temp;
			resultItem++;
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

int GetAllCPURate(PWMI_LOGIN_INFO login, char* buffer)
{

	string strResult = "detailutilization=";
	WbemScripting::ISWbemServicesPtr services;
	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}
	try
	{
		WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("Win32_Processor", WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY, NULL);
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
		int nCpuRate = 0, nCount =0, nTotalRate = 0;
		char temp[1024] = {0};
		string strTemp = "";
		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 
			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("LoadPercentage",0);
			_variant_t value = prop->GetValue();
			prop.Release();

			nCpuRate = (int)value;	

			prop = properties->Item("DeviceID",0);
			memset(temp, 0, 1024);
			sprintf(temp,"%s:%d-", (const char*)_bstr_t(prop->GetValue()), nCpuRate);
			prop.Release();

			strTemp += temp;
			nTotalRate += nCpuRate;
			nCount++;

			properties.Release();
			object.Release();

			VariantClear(&var);

		}

		strTemp.erase(strTemp.end() - 1);
		memset(temp, 0, 1024);
		sprintf(temp,"utilization=%d", nTotalRate/nCount);
		strResult += strTemp + ";" + temp;

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