#pragma once
#include "monitor.h"

int EnumProcessInfo(PWMI_LOGIN_INFO login, char* buffer);
int GetProcessInfo(PWMI_LOGIN_INFO login, const char * strProcessName, char* buffer);