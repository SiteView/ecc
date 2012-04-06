package com.siteview.agent;

public interface RequestCommand {
	//Common
	String COMMAND_COMMON_TEST = "Test";
	String COMMAND_COMMON_COUNTERS = "Counters";
	
	//Memory
	String COMMAND_USED_MEMORY = "UsedMemory(KB)";
	String COMMAND_FREE_MEMORY = "FreeMemory(KB)";
	String COMMAND_USED_SWAP = "UsedSwap(KB)";
	String COMMAND_FREE_SWAP = "FreeSwap(KB)";
	String COMMAND_MEMORY_SWAP = "MemorySwap(KB)";
	String COMMAND_MEMORY_RAM = "RAM(MB)";
	String COMMAND_MEMORY = "Memory(KB)";
	
	//Cpu
	String COMMAND_CPU = "Cpu";	
	String COMMAND_CPU_VENDOR = "Vendor";
	String COMMAND_CPU_MODEL = "Model";
	String COMMAND_CPU_MHZ = "Mhz";
	String COMMAND_CPU_CORES = "Cores";
	String COMMAND_CPU_USERTIME = "UserTime";
	String COMMAND_CPU_SYSTIME = "SysTime";
	String COMMAND_CPU_IDLETIME = "IdleTime";
	String COMMAND_CPU_WAITTIME = "WaitTime";
	String COMMAND_CPU_NICETIME = "NiceTime";
	String COMMAND_CPU_IRQTIME = "IrqTime";
	String COMMAND_CPU_COMBINED = "Combined";
	
	//Disk DiskPartition
	String COMMAND_DISK = "DiskPartition";
	String COMMAND_DISK_SIZE = "DiskPartitionSize";
	String COMMAND_DISK_USED = "DiskPartitionUsed";
	String COMMAND_DISK_AVAIL = "DiskPartitionFree";
	String COMMAND_DISK_USED_PERCENT = "DiskPartitionUsedPercent";
	String COMMAND_DISK_MOUNTED = "DiskPartitionMounted";
	String COMMAND_DISK_TYPE = "DiskPartitionType";
}
