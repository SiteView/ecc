package com.siteview.agent;

import org.hyperic.sigar.CpuPerc;
import org.hyperic.sigar.DirStat;
import org.hyperic.sigar.DirUsage;
import org.hyperic.sigar.DiskUsage;
import org.hyperic.sigar.FileSystem;
import org.hyperic.sigar.FileSystemUsage;
import org.hyperic.sigar.Mem;
import org.hyperic.sigar.MultiProcCpu;
import org.hyperic.sigar.NetConnection;
import org.hyperic.sigar.NetInterfaceConfig;
import org.hyperic.sigar.NetInterfaceStat;
import org.hyperic.sigar.NetRoute;
import org.hyperic.sigar.NetStat;
import org.hyperic.sigar.NfsClientV2;
import org.hyperic.sigar.NfsClientV3;
import org.hyperic.sigar.NfsServerV2;
import org.hyperic.sigar.NfsServerV3;
import org.hyperic.sigar.ProcCpu;
import org.hyperic.sigar.ProcCred;
import org.hyperic.sigar.ProcCredName;
import org.hyperic.sigar.ProcExe;
import org.hyperic.sigar.ProcFd;
import org.hyperic.sigar.ProcMem;
import org.hyperic.sigar.ProcStat;
import org.hyperic.sigar.ProcState;
import org.hyperic.sigar.ProcTime;
import org.hyperic.sigar.ResourceLimit;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;
import org.hyperic.sigar.Swap;
import org.hyperic.sigar.Tcp;
import org.hyperic.sigar.ThreadCpu;
import org.hyperic.sigar.Uptime;
import org.hyperic.sigar.Who;

import com.siteview.agent.info.*;

public class SigarCollector extends Collector {

	private Sigar sigar;

	SigarCollector() {
		sigar = new Sigar();
	}

	@Override
	public CpuInfo[] getCpuInfoList() {
		CpuInfo[] cpuInfos = null;

		try {
			org.hyperic.sigar.CpuInfo[] sigarCpuInfos = sigar.getCpuInfoList();
			int count = sigarCpuInfos.length;
			cpuInfos = new CpuInfo[count];
			for (int i = 0; i < count; i++) {
				cpuInfos[i] = transform(sigarCpuInfos[i]);
			}
		} catch (SigarException e) {

		}
		return cpuInfos;
	}

	@Override
	CpuPercInfo getCpuPerc() {
		CpuPercInfo cpuPercInfo = null;
		try {
			CpuPerc cpuPerc = sigar.getCpuPerc();
			cpuPercInfo = transform(cpuPerc);
		} catch (SigarException e) {

		}
		return cpuPercInfo;
	}

	@Override
	CpuPercInfo[] getCpuPercList() {
		CpuPercInfo[] cpuPercList = null;
		try {
			CpuPerc[] sigarCpuPercList = sigar.getCpuPercList();
			int count = sigarCpuPercList.length;
			cpuPercList = new CpuPercInfo[count];
			for (int i = 0; i < count; i++) {
				cpuPercList[i] = transform(sigarCpuPercList[i]);
			}
		} catch (SigarException e) {
		}
		return cpuPercList;
	}

	@Override
	DirStatInfo getDirStat(String name) {
		DirStatInfo dirStatInfo = null;
		try {
			dirStatInfo = transform(sigar.getDirStat(name));
		} catch (SigarException e) {
		}
		return dirStatInfo;
	}

	@Override
	DirUsageInfo getDirUsage(String name) {
		DirUsageInfo dirUsageInfo = null;
		try {
			dirUsageInfo = transform(sigar.getDirUsage(name));
		} catch (SigarException e) {
		}
		return dirUsageInfo;
	}

	@Override
	DiskUsageInfo getDiskUsage(String name) {
		DiskUsageInfo diskUsageInfo = null;
		try {
			diskUsageInfo = transform(sigar.getDiskUsage(name));
		} catch (SigarException e) {
		}
		return diskUsageInfo;
	}

	@Override
	String getFQDN() {
		String result = null;
		try {
			result = sigar.getFQDN();
		} catch (SigarException e) {
		}
		return result;
	}

	@Override
	FileSystemInfo[] getFileSystemList() {
		FileSystemInfo[] fileSystemInfo = null;
		try {
			FileSystem[] sigarfileSystem = sigar.getFileSystemList();
			int count = sigarfileSystem.length;
			fileSystemInfo = new FileSystemInfo[count];
			for (int i = 0; i < count; i++) {
				fileSystemInfo[i] = transform(sigarfileSystem[i]);
			}
		} catch (SigarException e) {

		}
		return fileSystemInfo;
	}

	@Override
	FileSystemUsageInfo getFileSystemUsage(String name) {
		FileSystemUsageInfo fileSystemUsageInfo = null;
		try {
			fileSystemUsageInfo = transform(sigar.getFileSystemUsage(name));
		} catch (SigarException e) {
		}
		return fileSystemUsageInfo;
	}

	@Override
	double[] getLoadAverage() {
		double[] result = null;
		try {
			result = sigar.getLoadAverage();
		} catch (SigarException e) {
		}
		return result;
	}

	@Override
	MemInfo getMem() {
		MemInfo memInfo = null;
		try {
			memInfo = transform(sigar.getMem());
		} catch (SigarException e) {
		}
		return memInfo;
	}

	@Override
	FileSystemUsageInfo getMountedFileSystemUsage(String name) {
		FileSystemUsageInfo rileSystemUsageInfo = null;
		try {
			rileSystemUsageInfo = transform(sigar
					.getMountedFileSystemUsage(name));
		} catch (SigarException e) {
		}
		return rileSystemUsageInfo;
	}

	@Override
	MultiProcCpuInfo getMultiProcCpu(String query) {
		MultiProcCpuInfo multiProcCpuInfo = null;
		try {
			multiProcCpuInfo = transform(sigar.getMultiProcCpu(query));
		} catch (SigarException e) {
		}
		return multiProcCpuInfo;
	}

	@Override
	ProcMemInfo getMultiProcMem(String query) {
		ProcMemInfo procMemInfo = null;
		try {
			procMemInfo = transform(sigar.getMultiProcMem(query));
		} catch (SigarException e) {
		}
		return procMemInfo;
	}

	@Override
	NetConnectionInfo[] getNetConnectionList(int flag) {
		NetConnectionInfo[] netConnectionInfo = null;

		try {
			NetConnection[] sigarNetConnectionInfo = sigar
					.getNetConnectionList(flag);
			int count = sigarNetConnectionInfo.length;
			netConnectionInfo = new NetConnectionInfo[count];
			for (int i = 0; i < count; i++) {
				netConnectionInfo[i] = transform(sigarNetConnectionInfo[i]);
			}
		} catch (SigarException e) {

		}
		return netConnectionInfo;
	}

	@Override
	NetInfo getNetInfo() {
		NetInfo netInfo = null;
		try {
			netInfo = transform(sigar.getNetInfo());
		} catch (SigarException e) {
		}
		return netInfo;
	}

	@Override
	NetInterfaceConfigInfo getNetInterfaceConfig() {
		NetInterfaceConfigInfo netInterfaceConfigInfo = null;
		try {
			netInterfaceConfigInfo = transform(sigar.getNetInterfaceConfig());
		} catch (SigarException e) {
		}
		return netInterfaceConfigInfo;
	}

	@Override
	NetInterfaceConfigInfo getNetInterfaceConfig(String name) {
		NetInterfaceConfigInfo netInterfaceConfigInfo = null;
		try {
			netInterfaceConfigInfo = transform(sigar
					.getNetInterfaceConfig(name));
		} catch (SigarException e) {
		}
		return netInterfaceConfigInfo;
	}

	@Override
	String[] getNetInterfaceList() {
		String[] result = null;
		try {
			result = sigar.getNetInterfaceList();
		} catch (SigarException e) {
		}
		return result;
	}

	@Override
	NetInterfaceStatInfo getNetInterfaceStat(String name) {
		NetInterfaceStatInfo netInterfaceStatInfo = null;
		try {
			netInterfaceStatInfo = transform(sigar.getNetInterfaceStat(name));
		} catch (SigarException e) {
		}
		return netInterfaceStatInfo;
	}

	@Override
	String getNetListenAddress(int port) {
		String result = null;
		try {
			result = sigar.getNetListenAddress(port);
		} catch (SigarException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return result;
	}

	@Override
	NetRouteInfo[] getNetRouteList() {
		NetRouteInfo[] netRouteInfo = null;

		try {
			NetRoute[] sigarNetRoute = sigar.getNetRouteList();
			int count = sigarNetRoute.length;
			netRouteInfo = new NetRouteInfo[count];
			for (int i = 0; i < count; i++) {
				netRouteInfo[i] = transform(sigarNetRoute[i]);
			}
		} catch (SigarException e) {

		}
		return netRouteInfo;
	}

	@Override
	String getNetServicesName(int protocol, long port) {
		return sigar.getNetServicesName(protocol, port);
	}

	@Override
	NetStatInfo getNetStat() {
		NetStatInfo netStatInfo = null;
		try {
			netStatInfo = transform(sigar.getNetStat());
		} catch (SigarException e) {
		}
		return netStatInfo;
	}

	@Override
	NetStatInfo getNetStat(byte[] address, long port) {
		NetStatInfo netStatInfo = null;
		try {
			netStatInfo = transform(sigar.getNetStat(address, port));
		} catch (SigarException e) {
		}
		return netStatInfo;
	}

	@Override
	NfsClientV2Info getNfsClientV2() {
		NfsClientV2Info nfsClientV2Info = null;
		try {
			nfsClientV2Info = transform(sigar.getNfsClientV2());
		} catch (SigarException e) {
		}
		return nfsClientV2Info;
	}

	@Override
	NfsClientV3Info getNfsClientV3() {
		NfsClientV3Info nfsClientV3Info = null;
		try {
			nfsClientV3Info = transform(sigar.getNfsClientV3());
		} catch (SigarException e) {
		}
		return nfsClientV3Info;
	}

	@Override
	NfsServerV2Info getNfsServerV2() {
		NfsServerV2Info nfsServerV2Info = null;
		try {
			nfsServerV2Info = transform(sigar.getNfsServerV2());
		} catch (SigarException e) {
		}
		return nfsServerV2Info;
	}

	@Override
	NfsServerV3Info getNfsServerV3() {
		NfsServerV3Info nfsServerV3Info = null;
		try {
			nfsServerV3Info = transform(sigar.getNfsServerV3());
		} catch (SigarException e) {
		}
		return nfsServerV3Info;
	}

	@Override
	String[] getProcArgs(long pid) {
		String[] result = null;
		try {
			result = sigar.getProcArgs(pid);
		} catch (SigarException e) {
		}
		return result;
	}

	@Override
	ProcCpuInfo getProcCpu(long pid) {
		ProcCpuInfo procCpuInfo = null;
		try {
			procCpuInfo = transform(sigar.getProcCpu(pid));
		} catch (SigarException e) {
		}
		return procCpuInfo;
	}

	@Override
	ProcCredInfo getProcCred(long pid) {
		ProcCredInfo procCredInfo = null;
		try {
			procCredInfo = transform(sigar.getProcCred(pid));
		} catch (SigarException e) {
		}
		return procCredInfo;
	}

	@Override
	ProcCredNameInfo getProcCredName(long pid) {
		ProcCredNameInfo procCredNameInfo = null;
		try {
			procCredNameInfo = transform(sigar.getProcCredName(pid));
		} catch (SigarException e) {
		}
		return procCredNameInfo;
	}

	@Override
	String getProcEnv(long pid, String key) {
		String result = null;
		try {
			result = sigar.getProcEnv(pid, key);
		} catch (SigarException e) {

		}
		return result;
	}

	@Override
	ProcExeInfo getProcExe(long pid) {
		ProcExeInfo procExeInfo = null;
		try {
			procExeInfo = transform(sigar.getProcExe(pid));
		} catch (SigarException e) {
		}
		return procExeInfo;
	}

	@Override
	ProcFdInfo getProcFd(long pid) {
		ProcFdInfo procFdInfo = null;
		try {
			procFdInfo = transform(sigar.getProcFd(pid));
		} catch (SigarException e) {
		}
		return procFdInfo;
	}

	@Override
	long[] getProcList() {
		long[] result = null;
		try {
			result = sigar.getProcList();
		} catch (SigarException e) {
		}
		return result;
	}

	@Override
	ProcMemInfo getProcMem(long pid) {
		ProcMemInfo procMemInfo = null;
		try {
			procMemInfo = transform(sigar.getProcMem(pid));
		} catch (SigarException e) {
		}
		return procMemInfo;
	}

	@Override
	long getProcPid(int protocol, long port) {
		return 0;
	}

	@Override
	ProcStatInfo getProcStat() {
		ProcStatInfo procStatInfo = null;
		try {
			procStatInfo = transform(sigar.getProcStat());
		} catch (SigarException e) {
		}
		return procStatInfo;
	}

	@Override
	ProcStateInfo getProcState(long pid) {
		ProcStateInfo procStateInfo = null;
		try {
			procStateInfo = transform(sigar.getProcState(pid));
		} catch (SigarException e) {
		}
		return procStateInfo;
	}

	@Override
	ProcTimeInfo getProcTime(long pid) {
		ProcTimeInfo procTimeInfo = null;
		try {
			procTimeInfo = transform(sigar.getProcTime(pid));
		} catch (SigarException e) {
		}
		return procTimeInfo;
	}

	@Override
	ResourceLimitInfo getResourceLimit() {
		ResourceLimitInfo resourceLimitInfo = null;
		try {
			resourceLimitInfo = transform(sigar.getResourceLimit());
		} catch (SigarException e) {
		}
		return resourceLimitInfo;
	}

	@Override
	long getServicePid(String name) {
		long result = -1;
		try {
			result = sigar.getServicePid(name);
		} catch (SigarException e) {
		}
		return result;
	}

	@Override
	int getSigNum(String name) {
		return Sigar.getSigNum(name);
	}

	@Override
	SwapInfo getSwap() {
		SwapInfo swapInfo = null;
		try {
			swapInfo = transform(sigar.getSwap());
		} catch (SigarException e) {
		}
		return swapInfo;
	}

	@Override
	TcpInfo getTcp() {
		TcpInfo tcpInfo = null;
		try {
			tcpInfo = transform(sigar.getTcp());
		} catch (SigarException e) {
		}
		return tcpInfo;
	}

	@Override
	ThreadCpuInfo getThreadCpu() {
		ThreadCpuInfo threadCpuInfo = null;
		try {
			threadCpuInfo = transform(sigar.getThreadCpu());
		} catch (SigarException e) {
		}
		return threadCpuInfo;
	}

	@Override
	UptimeInfo getUptime() {
		UptimeInfo uptimeInfo = null;
		try {
			uptimeInfo = transform(sigar.getUptime());
		} catch (SigarException e) {
		}
		return uptimeInfo;
	}

	@Override
	WhoInfo[] getWhoList() {
		WhoInfo[] whoInfo = null;

		try {
			Who[] sigarWhoInfos = sigar.getWhoList();
			int count = sigarWhoInfos.length;
			whoInfo = new WhoInfo[count];
			for (int i = 0; i < count; i++) {
				whoInfo[i] = transform(sigarWhoInfos[i]);
			}
		} catch (SigarException e) {

		}
		return whoInfo;
	}

	private CpuInfo transform(org.hyperic.sigar.CpuInfo sigarCpuInfo) {
		CpuInfo cpuInfo = new CpuInfo();
		cpuInfo.setCacheSize(sigarCpuInfo.getCacheSize());
		cpuInfo.setCoresPerSocket(sigarCpuInfo.getCoresPerSocket());
		cpuInfo.setMhz(sigarCpuInfo.getMhz());
		cpuInfo.setTotalCores(sigarCpuInfo.getTotalCores());
		cpuInfo.setTotalSockets(sigarCpuInfo.getTotalSockets());
		cpuInfo.setModel(sigarCpuInfo.getModel());
		cpuInfo.setVendor(sigarCpuInfo.getVendor());
		return cpuInfo;
	}

	private CpuPercInfo transform(CpuPerc sigarCpuPerc) {
		CpuPercInfo cpuPercInfo = new CpuPercInfo();
		cpuPercInfo.setCombined(sigarCpuPerc.getCombined());
		cpuPercInfo.setIdle(sigarCpuPerc.getIdle());
		cpuPercInfo.setIrq(sigarCpuPerc.getIrq());
		cpuPercInfo.setNice(sigarCpuPerc.getNice());
		cpuPercInfo.setSoftIrq(sigarCpuPerc.getSoftIrq());
		cpuPercInfo.setSys(sigarCpuPerc.getSys());
		cpuPercInfo.setUser(sigarCpuPerc.getUser());
		cpuPercInfo.setWait(sigarCpuPerc.getWait());
		cpuPercInfo.setStolen(sigarCpuPerc.getStolen());
		return cpuPercInfo;
	}

	private DirStatInfo transform(DirStat sigarDirStat) {
		DirStatInfo dirStatInfo = new DirStatInfo();
		dirStatInfo.setBlkdevs(sigarDirStat.getBlkdevs());
		dirStatInfo.setChrdevs(sigarDirStat.getChrdevs());
		dirStatInfo.setDiskUsage(sigarDirStat.getDiskUsage());
		dirStatInfo.setFiles(sigarDirStat.getFiles());
		dirStatInfo.setSockets(sigarDirStat.getSockets());
		dirStatInfo.setSubdirs(sigarDirStat.getSubdirs());
		dirStatInfo.setSymlinks(sigarDirStat.getSymlinks());
		dirStatInfo.setTotal(sigarDirStat.getTotal());
		return dirStatInfo;
	}

	private DirUsageInfo transform(DirUsage sigarDirUsage) {
		DirUsageInfo dirUsageInfo = new DirUsageInfo();
		dirUsageInfo.setBlkdevs(sigarDirUsage.getBlkdevs());
		dirUsageInfo.setChrdevs(sigarDirUsage.getChrdevs());
		dirUsageInfo.setDiskUsage(sigarDirUsage.getDiskUsage());
		dirUsageInfo.setFiles(sigarDirUsage.getFiles());
		dirUsageInfo.setSockets(sigarDirUsage.getSockets());
		dirUsageInfo.setSubdirs(sigarDirUsage.getSubdirs());
		dirUsageInfo.setSymlinks(sigarDirUsage.getSymlinks());
		dirUsageInfo.setTotal(sigarDirUsage.getTotal());
		return dirUsageInfo;
	}

	private FileSystemInfo transform(FileSystem sigarFileSystem) {
		FileSystemInfo fileSystemInfo = new FileSystemInfo();
		fileSystemInfo.setDevName(sigarFileSystem.getDevName());
		fileSystemInfo.setDirName(sigarFileSystem.getDirName());
		fileSystemInfo.setFlags(sigarFileSystem.getFlags());
		fileSystemInfo.setOptions(sigarFileSystem.getOptions());
		fileSystemInfo.setSysTypeName(sigarFileSystem.getSysTypeName());
		fileSystemInfo.setType(sigarFileSystem.getType());
		fileSystemInfo.setTypeName(sigarFileSystem.getTypeName());
		return fileSystemInfo;
	}

	private FileSystemUsageInfo transform(FileSystemUsage sigarFileSystemUsage) {
		FileSystemUsageInfo fileSystemUsageInfo = new FileSystemUsageInfo();
		fileSystemUsageInfo.setAvail(sigarFileSystemUsage.getAvail());
		fileSystemUsageInfo.setDiskQueue(sigarFileSystemUsage.getDiskQueue());
		fileSystemUsageInfo.setDiskReadBytes(sigarFileSystemUsage
				.getDiskReadBytes());
		fileSystemUsageInfo.setDiskReads(sigarFileSystemUsage.getDiskReads());
		fileSystemUsageInfo.setDiskServiceTime(sigarFileSystemUsage
				.getDiskServiceTime());
		fileSystemUsageInfo.setDiskWriteBytes(sigarFileSystemUsage
				.getDiskWriteBytes());
		fileSystemUsageInfo.setDiskWrites(sigarFileSystemUsage.getDiskWrites());
		fileSystemUsageInfo.setFiles(sigarFileSystemUsage.getFiles());
		fileSystemUsageInfo.setFree(sigarFileSystemUsage.getFree());
		fileSystemUsageInfo.setFreeFiles(sigarFileSystemUsage.getFreeFiles());
		fileSystemUsageInfo.setTotal(sigarFileSystemUsage.getTotal());
		fileSystemUsageInfo.setUsed(sigarFileSystemUsage.getUsed());
		fileSystemUsageInfo.setUsePercent(sigarFileSystemUsage.getUsePercent());
		return fileSystemUsageInfo;
	}

	private MemInfo transform(Mem sigarMem) {
		MemInfo memInfo = new MemInfo();
		memInfo.setActualFree(sigarMem.getActualFree());
		memInfo.setActualUsed(sigarMem.getActualUsed());
		memInfo.setFree(sigarMem.getFree());
		memInfo.setFreePercent(sigarMem.getFreePercent());
		memInfo.setRam(sigarMem.getRam());
		memInfo.setTotal(sigarMem.getTotal());
		memInfo.setUsed(sigarMem.getUsed());
		memInfo.setUsedPercent(sigarMem.getUsedPercent());
		return memInfo;
	}

	private MultiProcCpuInfo transform(MultiProcCpu sigarMultiProcCpu) {
		MultiProcCpuInfo multiProcCpuInfo = new MultiProcCpuInfo();
		return multiProcCpuInfo;
	}

	private ProcMemInfo transform(ProcMem sigarProcMem) {
		ProcMemInfo procMemInfo = new ProcMemInfo();
		procMemInfo.setMajorFaults(sigarProcMem.getMajorFaults());
		procMemInfo.setMinorFaults(sigarProcMem.getMinorFaults());
		procMemInfo.setPageFaults(sigarProcMem.getPageFaults());
		procMemInfo.setResident(sigarProcMem.getResident());
		procMemInfo.setShare(sigarProcMem.getShare());
		procMemInfo.setSize(sigarProcMem.getSize());
		return procMemInfo;
	}

	private NetConnectionInfo transform(NetConnection sigarNetConnection) {
		NetConnectionInfo netConnectionInfo = new NetConnectionInfo();
		netConnectionInfo.setLocalAddress(sigarNetConnection.getLocalAddress());
		netConnectionInfo.setLocalPort(sigarNetConnection.getLocalPort());
		netConnectionInfo.setReceiveQueue(sigarNetConnection.getReceiveQueue());
		netConnectionInfo.setRemoteAddress(sigarNetConnection
				.getRemoteAddress());
		netConnectionInfo.setRemotePort(sigarNetConnection.getRemotePort());
		netConnectionInfo.setSendQueue(sigarNetConnection.getSendQueue());
		netConnectionInfo.setState(sigarNetConnection.getState());
		netConnectionInfo.setType(sigarNetConnection.getType());
		return netConnectionInfo;
	}

	private NetInfo transform(org.hyperic.sigar.NetInfo sigarNetInfo) {
		NetInfo netInfo = new NetInfo();
		netInfo.setDefaultGateway(sigarNetInfo.getDefaultGateway());
		netInfo.setDomainName(sigarNetInfo.getDomainName());
		netInfo.setHostName(sigarNetInfo.getHostName());
		netInfo.setPrimaryDns(sigarNetInfo.getPrimaryDns());
		netInfo.setSecondaryDns(sigarNetInfo.getSecondaryDns());
		return netInfo;
	}

	private NetInterfaceConfigInfo transform(
			NetInterfaceConfig sigarNetInterfaceConfig) {
		NetInterfaceConfigInfo netInterfaceConfigInfo = new NetInterfaceConfigInfo();
		netInterfaceConfigInfo.setAddress(sigarNetInterfaceConfig.getAddress());
		netInterfaceConfigInfo.setBroadcast(sigarNetInterfaceConfig
				.getBroadcast());
		netInterfaceConfigInfo.setDescription(sigarNetInterfaceConfig
				.getDescription());
		netInterfaceConfigInfo.setDestination(sigarNetInterfaceConfig
				.getDestination());
		netInterfaceConfigInfo.setFlags(sigarNetInterfaceConfig.getFlags());
		netInterfaceConfigInfo.setHwaddr(sigarNetInterfaceConfig.getHwaddr());
		netInterfaceConfigInfo.setMetric(sigarNetInterfaceConfig.getMetric());
		netInterfaceConfigInfo.setMtu(sigarNetInterfaceConfig.getMtu());
		netInterfaceConfigInfo.setName(sigarNetInterfaceConfig.getName());
		netInterfaceConfigInfo.setNetmask(sigarNetInterfaceConfig.getNetmask());
		netInterfaceConfigInfo.setType(sigarNetInterfaceConfig.getType());
		return netInterfaceConfigInfo;
	}

	private NetInterfaceStatInfo transform(
			NetInterfaceStat sigarNetInterfaceStat) {
		NetInterfaceStatInfo netInterfaceStatInfo = new NetInterfaceStatInfo();
		netInterfaceStatInfo.setRxBytes(sigarNetInterfaceStat.getRxBytes());
		netInterfaceStatInfo.setRxDropped(sigarNetInterfaceStat.getRxDropped());
		netInterfaceStatInfo.setRxErrors(sigarNetInterfaceStat.getRxErrors());
		netInterfaceStatInfo.setRxFrame(sigarNetInterfaceStat.getRxFrame());
		netInterfaceStatInfo.setRxOverruns(sigarNetInterfaceStat
				.getRxOverruns());
		netInterfaceStatInfo.setRxPackets(sigarNetInterfaceStat.getRxPackets());
		netInterfaceStatInfo.setSpeed(sigarNetInterfaceStat.getSpeed());
		netInterfaceStatInfo.setTxBytes(sigarNetInterfaceStat.getTxBytes());
		netInterfaceStatInfo.setTxCarrier(sigarNetInterfaceStat.getTxCarrier());
		netInterfaceStatInfo.setTxCollisions(sigarNetInterfaceStat
				.getTxCollisions());
		netInterfaceStatInfo.setTxDropped(sigarNetInterfaceStat.getTxDropped());
		netInterfaceStatInfo.setTxErrors(sigarNetInterfaceStat.getTxErrors());
		netInterfaceStatInfo.setTxOverruns(sigarNetInterfaceStat
				.getTxOverruns());
		netInterfaceStatInfo.setTxPackets(sigarNetInterfaceStat.getTxPackets());
		return netInterfaceStatInfo;
	}

	private NetRouteInfo transform(NetRoute sigarNetRoute) {
		NetRouteInfo netRouteInfo = new NetRouteInfo();
		netRouteInfo.setDestination(sigarNetRoute.getDestination());
		netRouteInfo.setFlags(sigarNetRoute.getFlags());
		netRouteInfo.setGateway(sigarNetRoute.getGateway());
		netRouteInfo.setIfname(sigarNetRoute.getIfname());
		netRouteInfo.setIrtt(sigarNetRoute.getIrtt());
		netRouteInfo.setMask(sigarNetRoute.getMask());
		netRouteInfo.setMetric(sigarNetRoute.getMetric());
		netRouteInfo.setMtu(sigarNetRoute.getMtu());
		netRouteInfo.setRefcnt(sigarNetRoute.getRefcnt());
		netRouteInfo.setUse(sigarNetRoute.getUse());
		netRouteInfo.setWindow(sigarNetRoute.getWindow());
		return netRouteInfo;
	}

	private NetStatInfo transform(NetStat sigarNetStat) {
		NetStatInfo netStatInfo = new NetStatInfo();
		netStatInfo.setAllInboundTotal(sigarNetStat.getAllInboundTotal());
		netStatInfo.setAllOutboundTotal(sigarNetStat.getAllOutboundTotal());
		netStatInfo.setTcpInboundTotal(sigarNetStat.getTcpInboundTotal());
		netStatInfo.setTcpOutboundTotal(sigarNetStat.getTcpOutboundTotal());
		netStatInfo.setTcpStates(sigarNetStat.getTcpStates());
		return netStatInfo;
	}

	private NfsClientV2Info transform(NfsClientV2 sigarNfsClientV2) {
		NfsClientV2Info nfsClientV2Info = new NfsClientV2Info();
		return nfsClientV2Info;
	}

	private NfsClientV3Info transform(NfsClientV3 sigarNfsClientV2) {
		NfsClientV3Info nfsClientV3Info = new NfsClientV3Info();
		return nfsClientV3Info;
	}

	private NfsServerV2Info transform(NfsServerV2 sigarNfsServerV2) {
		NfsServerV2Info nfsServerV2Info = new NfsServerV2Info();
		return nfsServerV2Info;
	}

	private NfsServerV3Info transform(NfsServerV3 sigarNfsServerV3) {
		NfsServerV3Info nfsServerV3Info = new NfsServerV3Info();
		return nfsServerV3Info;
	}

	private ProcCpuInfo transform(ProcCpu sigarProcCpu) {
		ProcCpuInfo procCpuInfo = new ProcCpuInfo();
		procCpuInfo.setLastTime(sigarProcCpu.getLastTime());
		procCpuInfo.setPercent(sigarProcCpu.getPercent());
		procCpuInfo.setStartTime(sigarProcCpu.getStartTime());
		procCpuInfo.setSys(sigarProcCpu.getSys());
		procCpuInfo.setTotal(sigarProcCpu.getTotal());
		procCpuInfo.setUser(sigarProcCpu.getUser());
		return procCpuInfo;
	}

	private ProcCredInfo transform(ProcCred sigarProcCred) {
		ProcCredInfo procCredInfo = new ProcCredInfo();
		return procCredInfo;
	}

	private ProcStatInfo transform(ProcStat sigarProcStat) {
		ProcStatInfo procStatInfo = new ProcStatInfo();
		procStatInfo.setIdle(sigarProcStat.getIdle());
		procStatInfo.setRunning(sigarProcStat.getRunning());
		procStatInfo.setSleeping(sigarProcStat.getSleeping());
		procStatInfo.setStopped(sigarProcStat.getStopped());
		procStatInfo.setThreads(sigarProcStat.getThreads());
		procStatInfo.setTotal(sigarProcStat.getTotal());
		procStatInfo.setZombie(sigarProcStat.getZombie());
		return procStatInfo;
	}

	private ThreadCpuInfo transform(ThreadCpu sigarThreadCpu) {
		ThreadCpuInfo threadCpuInfo = new ThreadCpuInfo();
		threadCpuInfo.setSys(sigarThreadCpu.getSys());
		threadCpuInfo.setTotal(sigarThreadCpu.getTotal());
		threadCpuInfo.setUser(sigarThreadCpu.getUser());
		return threadCpuInfo;
	}

	private TcpInfo transform(Tcp sigarTcp) {
		TcpInfo tcpInfo = new TcpInfo();
		tcpInfo.setActiveOpens(sigarTcp.getActiveOpens());
		tcpInfo.setAttemptFails(sigarTcp.getAttemptFails());
		tcpInfo.setCurrEstab(sigarTcp.getCurrEstab());
		tcpInfo.setEstabResets(sigarTcp.getEstabResets());
		tcpInfo.setInErrs(sigarTcp.getInErrs());
		tcpInfo.setInSegs(sigarTcp.getInSegs());
		tcpInfo.setOutRsts(sigarTcp.getOutRsts());
		tcpInfo.setOutSegs(sigarTcp.getOutSegs());
		tcpInfo.setPassiveOpens(sigarTcp.getPassiveOpens());
		tcpInfo.setRetransSegs(sigarTcp.getRetransSegs());
		return tcpInfo;
	}

	private SwapInfo transform(Swap sigarTcp) {
		SwapInfo swapInfo = new SwapInfo();
		swapInfo.setFree(sigarTcp.getFree());
		swapInfo.setPageIn(sigarTcp.getPageIn());
		swapInfo.setPageOut(sigarTcp.getPageOut());
		swapInfo.setTotal(sigarTcp.getTotal());
		swapInfo.setUsed(sigarTcp.getUsed());
		return swapInfo;
	}

	private ResourceLimitInfo transform(ResourceLimit sigarResourceLimit) {
		ResourceLimitInfo resourceLimitInfo = new ResourceLimitInfo();
		resourceLimitInfo.setCoreCur(sigarResourceLimit.getCoreCur());
		resourceLimitInfo.setCoreMax(sigarResourceLimit.getCoreMax());
		resourceLimitInfo.setCpuCur(sigarResourceLimit.getCpuCur());
		resourceLimitInfo.setCpuMax(sigarResourceLimit.getCpuCur());
		resourceLimitInfo.setDataCur(sigarResourceLimit.getDataCur());
		resourceLimitInfo.setDataMax(sigarResourceLimit.getDataMax());
		resourceLimitInfo.setFileSizeCur(sigarResourceLimit.getFileSizeCur());
		resourceLimitInfo.setFileSizeMax(sigarResourceLimit.getFileSizeMax());
		resourceLimitInfo.setMemoryCur(sigarResourceLimit.getMemoryCur());
		resourceLimitInfo.setMemoryMax(sigarResourceLimit.getMemoryMax());
		resourceLimitInfo.setOpenFilesCur(sigarResourceLimit.getOpenFilesCur());
		resourceLimitInfo.setOpenFilesMax(sigarResourceLimit.getOpenFilesMax());
		resourceLimitInfo.setPipeSizeCur(sigarResourceLimit.getPipeSizeCur());
		resourceLimitInfo.setPipeSizeMax(sigarResourceLimit.getPipeSizeMax());
		resourceLimitInfo.setProcessesCur(sigarResourceLimit.getProcessesCur());
		resourceLimitInfo.setProcessesMax(sigarResourceLimit.getProcessesMax());
		resourceLimitInfo.setStackCur(sigarResourceLimit.getStackCur());
		resourceLimitInfo.setStackMax(sigarResourceLimit.getStackMax());
		resourceLimitInfo.setVirtualMemoryCur(sigarResourceLimit
				.getVirtualMemoryCur());
		resourceLimitInfo.setVirtualMemoryMax(sigarResourceLimit
				.getVirtualMemoryMax());
		return resourceLimitInfo;
	}

	private ProcTimeInfo transform(ProcTime sigarProcTime) {
		ProcTimeInfo procTimeInfo = new ProcTimeInfo();
		procTimeInfo.setStartTime(sigarProcTime.getStartTime());
		procTimeInfo.setSys(sigarProcTime.getSys());
		procTimeInfo.setTotal(sigarProcTime.getTotal());
		procTimeInfo.setUser(sigarProcTime.getUser());
		return procTimeInfo;
	}

	private ProcStateInfo transform(ProcState sigarProcState) {
		ProcStateInfo procStateInfo = new ProcStateInfo();
		return procStateInfo;
	}

	private ProcFdInfo transform(ProcFd sigarProcFd) {
		ProcFdInfo procFdInfo = new ProcFdInfo();
		return procFdInfo;
	}

	private ProcExeInfo transform(ProcExe sigarProcExe) {
		ProcExeInfo procExeInfo = new ProcExeInfo();
		procExeInfo.setCwd(sigarProcExe.getCwd());
		procExeInfo.setName(sigarProcExe.getName());
		return procExeInfo;
	}

	private ProcCredNameInfo transform(ProcCredName sigarProcCredName) {
		ProcCredNameInfo procCredNameInfo = new ProcCredNameInfo();
		procCredNameInfo.setGroup(sigarProcCredName.getGroup());
		procCredNameInfo.setUser(sigarProcCredName.getUser());
		return procCredNameInfo;
	}

	/*
	 * private FileInfo transform(org.hyperic.sigar.FileInfo sigarFileInfo) {
	 * FileInfo fileInfo = new FileInfo(); return fileInfo; }
	 */

	private DiskUsageInfo transform(DiskUsage diskUsage) {
		DiskUsageInfo diskUsageInfo = new DiskUsageInfo();
		diskUsageInfo.setQueue(diskUsage.getQueue());
		diskUsageInfo.setReadBytes(diskUsage.getReadBytes());
		diskUsageInfo.setReads(diskUsage.getReads());
		diskUsageInfo.setServiceTime(diskUsage.getServiceTime());
		diskUsageInfo.setWriteBytes(diskUsage.getWriteBytes());
		diskUsageInfo.setWrites(diskUsage.getWrites());
		return diskUsageInfo;
	}

	private UptimeInfo transform(Uptime uptime) {
		UptimeInfo uptimeInfo = new UptimeInfo();
		return uptimeInfo;
	}

	private WhoInfo transform(Who who) {
		WhoInfo whoInfo = new WhoInfo();
		return whoInfo;
	}
}
