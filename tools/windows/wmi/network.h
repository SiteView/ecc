#pragma once
#include "monitor.h"

int EnumNetWorks(PWMI_LOGIN_INFO login, char* buffer);
int GetNetWorkInfo(PWMI_LOGIN_INFO login, const char* networkname, char* buffer);