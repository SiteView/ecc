#pragma once
#include "monitor.h"

int EnumDisksInfo(PWMI_LOGIN_INFO login, char* buffer);
int GetDiskInfo(PWMI_LOGIN_INFO login, const char* disk, char* buffer);