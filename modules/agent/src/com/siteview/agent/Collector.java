package com.siteview.agent;

import com.siteview.agent.info.*;

public abstract class Collector {
	//abstract Cpu getCpu();

	abstract CpuInfo[] getCpuInfoList();

	//abstract Cpu[] getCpuList();

	
	abstract CpuPercInfo getCpuPerc();

	abstract CpuPercInfo[] getCpuPercList();

	abstract DirStatInfo getDirStat(String name);

	abstract DirUsageInfo getDirUsage(String name);

	abstract DiskUsageInfo getDiskUsage(String name);

	// abstract FileInfo getFileInfo(String name);
	abstract FileSystemInfo[] getFileSystemList();

	//abstract FileSystemMapInfo getFileSystemMap();

	abstract FileSystemUsageInfo getFileSystemUsage(String name);

	abstract String getFQDN();

	// abstract FileInfo getLinkInfo(String name);
	abstract double[] getLoadAverage();

	abstract MemInfo getMem();

	abstract FileSystemUsageInfo getMountedFileSystemUsage(String name);

	abstract MultiProcCpuInfo getMultiProcCpu(String query);

	abstract ProcMemInfo getMultiProcMem(String query);

	abstract NetConnectionInfo[] getNetConnectionList(int flag);

	abstract NetInfo getNetInfo();

	abstract NetInterfaceConfigInfo getNetInterfaceConfig();

	abstract NetInterfaceConfigInfo getNetInterfaceConfig(String name);

	abstract String[] getNetInterfaceList();

	abstract NetInterfaceStatInfo getNetInterfaceStat(String name);

	abstract String getNetListenAddress(int port);

	abstract NetRouteInfo[] getNetRouteList();

	abstract String getNetServicesName(int protocol, long port);

	abstract NetStatInfo getNetStat();

	abstract NetStatInfo getNetStat(byte[] address, long port);

	abstract NfsClientV2Info getNfsClientV2();

	abstract NfsClientV3Info getNfsClientV3();

	abstract NfsServerV2Info getNfsServerV2();

	abstract NfsServerV3Info getNfsServerV3();

	abstract String[] getProcArgs(long pid);

	abstract ProcCpuInfo getProcCpu(long pid);

	abstract ProcCredInfo getProcCred(long pid);

	abstract ProcCredNameInfo getProcCredName(long pid);

	//abstract Map getProcEnv(long pid);

	abstract String getProcEnv(long pid, String key);

	//abstract Map getProcEnv(java.lang.String pid);

	abstract ProcExeInfo getProcExe(long pid);

	abstract ProcFdInfo getProcFd(long pid);

	abstract long[] getProcList();

	abstract ProcMemInfo getProcMem(long pid);

	//abstract List getProcModules(long pid);

	abstract long getProcPid(int protocol, long port);

	abstract ProcStatInfo getProcStat();

	abstract ProcStateInfo getProcState(long pid);

	abstract ProcTimeInfo getProcTime(long pid);

	abstract ResourceLimitInfo getResourceLimit();

	abstract long getServicePid(java.lang.String name);

	abstract int getSigNum(String name);

	abstract SwapInfo getSwap();

	abstract TcpInfo getTcp();

	abstract ThreadCpuInfo getThreadCpu();

	abstract UptimeInfo getUptime();

	abstract WhoInfo[] getWhoList();
	
}
