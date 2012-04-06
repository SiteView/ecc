#pragma once
#include "monitor.h"

int GetCPURate(PWMI_LOGIN_INFO login, char* buffer);
int GetAllCPURate(PWMI_LOGIN_INFO login, char* buffer);