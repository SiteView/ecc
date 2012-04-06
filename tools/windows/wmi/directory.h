#pragma once
#include "monitor.h"

typedef struct _directory_
{
	string drive;
	string path;
} DIRECTORY, *PDIRECTORY;

int GetDirectoryInfo(PWMI_LOGIN_INFO login, const char* path, const char* recursive, const char* match, char* buffer);