typedef struct _THREAD_INFO
{
LARGE_INTEGER CreateTime;
DWORD dwUnknown1;
DWORD dwStartAddress;
DWORD StartEIP;
DWORD dwOwnerPID;
DWORD dwThreadId;
DWORD dwCurrentPriority;
DWORD dwBasePriority;
DWORD dwContextSwitches;
DWORD Unknown;
DWORD WaitReason;

}THREADINFO, *PTHREADINFO;

typedef struct _UNICODE_STRING
{
 USHORT Length;
 USHORT MaxLength;
 PWSTR Buffer;
} UNICODE_STRING;

typedef struct _PROCESS_INFO
{
DWORD dwOffset;
DWORD dwThreadsCount;
DWORD dwUnused1[6];
LARGE_INTEGER CreateTime;
LARGE_INTEGER UserTime;
LARGE_INTEGER KernelTime;
UNICODE_STRING ProcessName;

DWORD dwBasePriority;
DWORD dwProcessID;
DWORD dwParentProcessId;
DWORD dwHandleCount;
DWORD dwUnused3[2];

DWORD dwVirtualBytesPeak;
DWORD dwVirtualBytes;
ULONG dwPageFaults;
DWORD dwWorkingSetPeak;
DWORD dwWorkingSet;
DWORD dwQuotaPeakPagedPoolUsage;
DWORD dwQuotaPagedPoolUsage;
DWORD dwQuotaPeakNonPagedPoolUsage;
DWORD dwQuotaNonPagedPoolUsage;
DWORD dwPageFileUsage;
DWORD dwPageFileUsagePeak;

DWORD dCommitCharge;
THREADINFO ThreadSysInfo[1];

} PROCESSINFO, *PPROCESSINFO;

int GetProcessCpuRateByID(int id)
{
	int cpuusage;
	PVOID pProcInfo = NULL;
	DWORD dwInfoSize = 0x20000;
	PPROCESSINFO pProcessInfo;
	DWORD dwWorkingSet;
	long ( __stdcall *NtQuerySystemInformation )( DWORD, PVOID, DWORD, DWORD );


	static __int64 LastTotalProcessCPUUsage = 0;
	static __int64 LastCurrentProcessCPUUsage = 0;

	int CurrentDelta;
	int TotalDelta;

	__int64 TotalProcessCPUUsage = 0;
	__int64 CurrentProcessCPUUsage = 0;

	/////////////////////////////////

	pProcInfo = (PVOID)(new byte[dwInfoSize]);

	NtQuerySystemInformation = (long(__stdcall*)(DWORD,PVOID,DWORD,DWORD))
	GetProcAddress( GetModuleHandle( "ntdll.dll" ),"NtQuerySystemInformation" );

	NtQuerySystemInformation(5,pProcInfo,dwInfoSize,0);

	pProcessInfo = (PPROCESSINFO)pProcInfo;

	do
	{
	TotalProcessCPUUsage += (__int64)pProcessInfo->KernelTime.QuadPart + (__int64)pProcessInfo->UserTime.QuadPart;

	if(pProcessInfo->dwProcessID == id)
	{
	dwWorkingSet = pProcessInfo->dwWorkingSet; 
	CurrentProcessCPUUsage += (__int64)pProcessInfo->KernelTime.QuadPart + (__int64)pProcessInfo->UserTime.QuadPart;
	}

	/////////
	if(pProcessInfo->dwOffset == 0)
	{
	break;
	}

	pProcessInfo = (PPROCESSINFO)((byte*)pProcessInfo + pProcessInfo->dwOffset);
	}
	while(true);

	TotalDelta = TotalProcessCPUUsage - LastTotalProcessCPUUsage;
	CurrentDelta = CurrentProcessCPUUsage - LastCurrentProcessCPUUsage;

	if(TotalDelta != 0)
	cpuusage = 100 * CurrentDelta / TotalDelta;

	//this->Caption = "CPU = " + IntToStr(100 * CurrentDelta / TotalDelta) + 
	//"Memory = "+ IntToStr(dwWorkingSet / 1024) " K";

	LastTotalProcessCPUUsage = TotalProcessCPUUsage;
	LastCurrentProcessCPUUsage = CurrentProcessCPUUsage;

	delete[] pProcInfo;
	return cpuusage;

}





