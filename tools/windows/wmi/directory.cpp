#include "directory.h"

int replace(string& str, const string& pattern,  const string& newpat)
{
	int count = 0;
	const size_t nsize = newpat.size();
	const size_t psize = pattern.size();

	for(size_t pos = str.find(pattern, 0); 
		pos != std::string::npos;
		pos = str.find(pattern,pos + nsize))
	{
		str.replace(pos, psize, newpat);
		count++;
	}

	return count;
} 


int GetSubDirectory(WbemScripting::ISWbemServicesPtr services, const char* parent, list<DIRECTORY>& directories)
{
	string strWQL = "ASSOCIATORS OF  {Win32_Directory.Name=\"";
	strWQL += parent;
	strWQL +=  "\"} Where AssocClass = Win32_Subdirectory ResultRole = PartComponent";
	WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(strWQL.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL);
	if(NULL == objects)
	{
		return 0;
	}

	IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
	if(NULL == obj_enum) 
	{
		objects.Release();
		objects = NULL;
		return 0;
	}

	list<string> children;
	string child = "";
	string drive = "";
	string path = "";

	VARIANT var; 
	ULONG fetched;

	while (obj_enum->Next(1,&var,&fetched) == S_OK) 
	{ 

		WbemScripting::ISWbemObjectPtr object = var;
		WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
		WbemScripting::ISWbemPropertyPtr prop = properties->Item("Name",0);
		child = (const char*)_bstr_t(prop->GetValue());
		prop.Release();

		replace(child, "\\", "\\\\");

		children.push_back(child);

		DIRECTORY directory;
		directory.drive = child.substr(0, 2);
		directory.path = child.substr(2, child.length() - 2) + "\\\\";

		directories.push_back(directory);

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

	int result = 1;

	list<string>::iterator item;
	for(item = children.begin(); item != children.end(); item++)
	{
		result = GetSubDirectory(services, (*item).c_str(), directories);
		if(!result)
		{
			break;
		}
	}

	if(result == 0)
	{
		return 0;
	}
	else
	{
		return 1;
	}
}

int CountFileInfo(WbemScripting::ISWbemServicesPtr services, list<DIRECTORY>& directories, const char* match, ULONG* count, ULONG* size, unsigned _int64* min, unsigned _int64* max)
{
	BOOLEAN matchFlag = strlen(match) > 0 ? TRUE : FALSE;

	string strWQL = "";
	list<DIRECTORY>::iterator item;

	for(item = directories.begin(); item != directories.end(); item++)
	{
		strWQL = "SELECT * FROM CIM_DATAFILE WHERE Drive =\"";
		strWQL += (*item).drive; 
		strWQL += "\" AND Path =\"";
		strWQL += (*item).path;
		strWQL += "\"";

		printf("strWQL---->%s\r\n", strWQL.c_str());
		WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(strWQL.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL);
		if(NULL == objects)
		{
			return 0;
		}

		IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
		if(NULL == obj_enum) 
		{
			objects.Release();
			objects = NULL;
			return 0;
		}

		VARIANT var; 
		ULONG fetched;

		string modifiedTime = "";
		unsigned _int64 time = 0;

		if(matchFlag)
		{
			string fileName = "";

			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 

				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("FileName",0);
				fileName = (const char*)_bstr_t(prop->GetValue());
				prop.Release();

				fileName += ".";

				prop = properties->Item("Extension",0);
				fileName += (const char*)_bstr_t(prop->GetValue());
				prop.Release();

				transform(fileName.begin(), fileName.end(), fileName.begin(), toupper);

				if(fileName.find(match) != string::npos)
				{
					(*count) ++;

					prop = properties->Item("FileSize",0);
					(*size) += (ULONG)(prop->GetValue());
					prop.Release();

					prop = properties->Item("LastModified",0);									//第一次获取PID，用于与下一次获取的对比。
					modifiedTime = (const char*)_bstr_t(prop->GetValue());
					prop.Release();

					modifiedTime = modifiedTime.substr(0, 14);
					sscanf(modifiedTime.c_str(), "%I64u",&time);

					//prop = properties->Item("Name",0);
					//fileName = (const char*)_bstr_t(prop->GetValue());
					//prop.Release();
					//printf("%s--->%I64u\r\n", fileName.c_str(), time);

					if(time > *max)
					{
						*max = time;
					}

					if(time < *min)
					{
						*min = time;
					}

				}

				properties.Release();
				object.Release();

				VariantClear(&var);
			}
		}
		else
		{
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 

				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("FileSize",0);
				(*size) += (ULONG)(prop->GetValue());
				prop.Release();


				prop = properties->Item("LastModified",0);									//第一次获取PID，用于与下一次获取的对比。
				modifiedTime = (const char*)_bstr_t(prop->GetValue());
				prop.Release();

				modifiedTime = modifiedTime.substr(0, 14);
				sscanf(modifiedTime.c_str(), "%I64u",&time);

				//prop = properties->Item("Name",0);
				//fileName = (const char*)_bstr_t(prop->GetValue());
				//prop.Release();
				//printf("%s--->%I64u\r\n", fileName.c_str(), time);

				if(time > *max)
				{
					*max = time;
				}

				if(time < *min)
				{
					*min = time;
				}

				(*count) ++;

				properties.Release();
				object.Release();

				VariantClear(&var);
			}
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

	return 1;
}
BOOLEAN IsDirectoryExsit(WbemScripting::ISWbemServicesPtr services, const char* path)
{

	string strWQL = "SELECT * FROM Win32_Directory WHERE Name =\"";
	strWQL += path;
	strWQL +=  "\"";
	WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(strWQL.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL);
	if(NULL == objects)
	{
		return FALSE;
	}

	IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
	if(NULL == obj_enum) 
	{
		objects.Release();
		objects = NULL;
		return 0;
	}

	VARIANT var; 
	ULONG fetched;

	BOOLEAN result = FALSE;

	while (obj_enum->Next(1,&var,&fetched) == S_OK) 
	{ 
		VariantClear(&var);
		result = TRUE;
		break;
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

	return result;

}
int DirectoryInfoNoMatch(WbemScripting::ISWbemServicesPtr services, const char* path, BOOLEAN recursive, ULONG* count, ULONG* size, unsigned _int64* min, unsigned _int64* max)
{
	string strWQL = "ASSOCIATORS OF  {Win32_Directory.Name=\"";
	strWQL += path;
	strWQL +=  "\"} Where ResultClass = CIM_DataFile";
	WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(strWQL.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL);
	if(NULL == objects)
	{
		return 0;
	}

	IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
	if(NULL == obj_enum) 
	{
		objects.Release();
		objects = NULL;
		return 0;
	}

	VARIANT var; 
	ULONG fetched;

	string fileName = "";
	string modifiedTime = "";
	unsigned _int64 time = 0;

	while (obj_enum->Next(1,&var,&fetched) == S_OK) 
	{ 

		WbemScripting::ISWbemObjectPtr object = var;
		WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
		WbemScripting::ISWbemPropertyPtr prop = properties->Item("FileSize",0);
		(*size) += (ULONG)(prop->GetValue());
		prop.Release();

		prop = properties->Item("LastModified",0);									//第一次获取PID，用于与下一次获取的对比。
		modifiedTime = (const char*)_bstr_t(prop->GetValue());
		prop.Release();

		modifiedTime = modifiedTime.substr(0, 14);
		sscanf(modifiedTime.c_str(), "%I64u",&time);

		//prop = properties->Item("Name",0);
		//fileName = (const char*)_bstr_t(prop->GetValue());
		//prop.Release();
		//printf("%s--->%I64u\r\n", fileName.c_str(), time);

		if(time > *max)
		{
			*max = time;
		}

		if(time < *min)
		{
			*min = time;
		}

		(*count) ++;

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

	if(recursive)
	{
		strWQL = "ASSOCIATORS OF  {Win32_Directory.Name=\"";
		strWQL += path;
		strWQL += "\"} Where AssocClass = Win32_Subdirectory ResultRole = PartComponent";
		objects = services->ExecQuery(strWQL.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL);
		if(NULL == objects)
		{
			return 0;
		}

		IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
		if(NULL == obj_enum) 
		{
			objects.Release();
			objects = NULL;
			return 0;
		}

		string subDirectory = "";
		int result = 1;

		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 

			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("Name",0);
			subDirectory = (const char*)_bstr_t(prop->GetValue());
			prop.Release();

			replace(subDirectory, "\\", "\\\\");

			result = DirectoryInfoNoMatch(services, subDirectory.c_str(), recursive, count, size, min, max);

			properties.Release();
			object.Release();

			VariantClear(&var);

			if(result == 0)
			{
				break;
			}
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

		if(result == 0)
		{
			return 0;
		}
		else
		{
			return 1;
		}
	}

	return 1;
}




int DirectoryInfoWithMatch(WbemScripting::ISWbemServicesPtr services, const char* path, BOOLEAN recursive, const char* match, ULONG* count, ULONG* size, unsigned _int64* min, unsigned _int64* max)
{
	string strWQL = "ASSOCIATORS OF  {Win32_Directory.Name=\"";
	strWQL += path; 
	strWQL += "\"} Where ResultClass = CIM_DataFile";
	WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(strWQL.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL);
	if(NULL == objects)
	{
		return 0;
	}

	IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
	if(NULL == obj_enum) 
	{
		objects.Release();
		objects = NULL;
		return 0;
	}

	VARIANT var; 
	ULONG fetched;

	string fileName = "";
	string modifiedTime = "";
	unsigned _int64 time = 0;

	while (obj_enum->Next(1,&var,&fetched) == S_OK) 
	{ 

		WbemScripting::ISWbemObjectPtr object = var;
		WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
		WbemScripting::ISWbemPropertyPtr prop = properties->Item("FileName",0);
		fileName = (const char*)_bstr_t(prop->GetValue());
		prop.Release();

		fileName += ".";

		prop = properties->Item("Extension",0);
		fileName += (const char*)_bstr_t(prop->GetValue());
		prop.Release();

		if(fileName.find(match) >= 0)
		{
			(*count) ++;

			prop = properties->Item("FileSize",0);
			(*size) += (ULONG)(prop->GetValue());
			prop.Release();

			prop = properties->Item("LastModified",0);									//第一次获取PID，用于与下一次获取的对比。
			modifiedTime = (const char*)_bstr_t(prop->GetValue());
			prop.Release();

			modifiedTime = modifiedTime.substr(0, 14);
			sscanf(modifiedTime.c_str(), "%I64u",&time);

			//prop = properties->Item("Name",0);
			//fileName = (const char*)_bstr_t(prop->GetValue());
			//prop.Release();
			//printf("%s--->%I64u\r\n", fileName.c_str(), time);

			if(time > *max)
			{
				*max = time;
			}

			if(time < *min)
			{
				*min = time;
			}

		}

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

	if(recursive)
	{
		strWQL = "ASSOCIATORS OF  {Win32_Directory.Name=\"";
		strWQL +=  path;
		strWQL +=  "\"} Where AssocClass = Win32_Subdirectory ResultRole = PartComponent";
		objects = services->ExecQuery(strWQL.c_str(),"WQL",WBEM_FLAG_FORWARD_ONLY | WBEM_FLAG_RETURN_IMMEDIATELY,NULL);
		if(NULL == objects)
		{
			return 0;
		}

		IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
		if(NULL == obj_enum) 
		{
			objects.Release();
			objects = NULL;
			return 0;
		}

		string subDirectory = "";
		int result = 1;

		while (obj_enum->Next(1,&var,&fetched) == S_OK) 
		{ 

			WbemScripting::ISWbemObjectPtr object = var;
			WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
			WbemScripting::ISWbemPropertyPtr prop = properties->Item("Name",0);
			subDirectory = (const char*)_bstr_t(prop->GetValue());
			prop.Release();

			replace(subDirectory, "\\", "\\\\");

			result = DirectoryInfoWithMatch(services, subDirectory.c_str(), recursive, match, count, size, min, max);

			properties.Release();
			object.Release();

			VariantClear(&var);

			if(result == 0)
			{
				break;
			}
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

		if(result == 0)
		{
			return 0;
		}
		else
		{
			return 1;
		}
	}

	return 1;
}


int GetDirectoryInfo(PWMI_LOGIN_INFO login, const char* path, const char* recursive, const char* match, char* buffer)
{
	char temp[1024] = {0};
	string strResult = "";

	BOOLEAN recursiveFlag = strcmp("true", recursive) == 0 ? TRUE : FALSE;

	WbemScripting::ISWbemServicesPtr services;

	unsigned long count = 0;
	unsigned long size = 0;
	unsigned _int64 min = 21000101000000;
	unsigned _int64 max = 0;

	if(!ConnectServer(login, buffer, services))
	{
		return 0;
	}
	try
	{

		if(IsDirectoryExsit(services, path))
		{

			int result = 1;

			list<DIRECTORY> directories;

			string strPath = path;

			string strMatch = match;

			transform(strMatch.begin(), strMatch.end(), strMatch.begin(), toupper);

			DIRECTORY directory;
			directory.drive = strPath.substr(0, 2);
			directory.path = strPath.substr(2, strPath.length() - 2);

			directories.push_back(directory);

			if(recursiveFlag)
			{
				result = GetSubDirectory(services, path, directories);
			}

			if(result)
			{
				result = CountFileInfo(services, directories, strMatch.c_str(), &count, &size, &min, &max);
				if(!result)
				{
					count = -1;
					min = 0;
					max = 0;
				}
			}
			else
			{
				count = -1;
				min = 0;
				max = 0;
			}
		}
		else
		{
			min = 0;
			max = 0;
		}

		memset(temp, 0, 1024);
		sprintf(temp,"count=%ld$size=%ld$min=%I64u$max=%I64u",count, size, min, max);
		strResult = temp;

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