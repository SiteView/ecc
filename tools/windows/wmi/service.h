#pragma once
#include "monitor.h"

int GetNTServices(PWMI_LOGIN_INFO login, char* buffer);
int GetServiceInfo(PWMI_LOGIN_INFO login, const char *servicename, char* buffer);