package com.siteview.agent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

//import org.json.simple.JSONArray;
//import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

import com.siteview.agent.info.FileSystemInfo;

public class CountersResponse extends Response {
	private Collector collector = null;
	private static Map<String, String> cmdTextMap;
	static {
		cmdTextMap = new HashMap<String, String>();
		cmdTextMap.put(RequestCommand.COMMAND_DISK_SIZE, "Size");
		cmdTextMap.put(RequestCommand.COMMAND_DISK_USED_PERCENT, "UsedPercent");
		cmdTextMap.put(RequestCommand.COMMAND_DISK_USED, "Used");
		cmdTextMap.put(RequestCommand.COMMAND_DISK_TYPE, "Type");
		cmdTextMap.put(RequestCommand.COMMAND_DISK_MOUNTED, "Mounted");
		cmdTextMap.put(RequestCommand.COMMAND_DISK_AVAIL, "Free");

		cmdTextMap.put(RequestCommand.COMMAND_CPU_COMBINED, "Combined");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_CORES, "Cores");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_IDLETIME, "IdleTime");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_IRQTIME, "IrqTime");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_MHZ, "Mhz");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_MODEL, "Model");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_NICETIME, "NiceTime");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_SYSTIME, "SysTime");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_USERTIME, "UserTime");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_VENDOR, "Vendor");
		cmdTextMap.put(RequestCommand.COMMAND_CPU_WAITTIME, "WaitTime");
	}

	public CountersResponse(Collector collector) {
		this.collector = collector;
	}

	public String toJson() {

		/*
		 * JSONObject counters = new JSONObject();
		 * 
		 * JSONArray memChildList = new JSONArray();
		 * memChildList.add(RequestCommand.COMMAND_USED_MEMORY);
		 * memChildList.add(RequestCommand.COMMAND_FREE_MEMORY); JSONObject
		 * swapCounters = new JSONObject(); JSONArray swapChildList = new
		 * JSONArray(); swapChildList.add(RequestCommand.COMMAND_USED_SWAP);
		 * swapChildList.add(RequestCommand.COMMAND_FREE_SWAP);
		 * swapCounters.put(RequestCommand.COMMAND_MEMORY_SWAP, swapChildList);
		 * memChildList.add(swapCounters);
		 * memChildList.add(RequestCommand.COMMAND_MEMORY_RAM);
		 * counters.put(RequestCommand.COMMAND_MEMORY, memChildList);
		 * 
		 * JSONArray cpuChildList = new JSONArray(); int cpuCount =
		 * collector.getCpuInfoList().length; for (int i = 1; i <= cpuCount;
		 * i++) { JSONObject cpuCounters = new JSONObject();
		 * cpuCounters.put(RequestCommand.COMMAND_CPU + "#" + i,
		 * buildIndexCmd(new String[] { RequestCommand.COMMAND_CPU_VENDOR,
		 * RequestCommand.COMMAND_CPU_COMBINED,
		 * RequestCommand.COMMAND_CPU_CORES,
		 * RequestCommand.COMMAND_CPU_IDLETIME,
		 * RequestCommand.COMMAND_CPU_IRQTIME, RequestCommand.COMMAND_CPU_MHZ,
		 * RequestCommand.COMMAND_CPU_MODEL,
		 * RequestCommand.COMMAND_CPU_NICETIME,
		 * RequestCommand.COMMAND_CPU_SYSTIME,
		 * RequestCommand.COMMAND_CPU_USERTIME,
		 * RequestCommand.COMMAND_CPU_WAITTIME }, i));
		 * cpuChildList.add(cpuCounters); } if (cpuCount > 0) { //
		 * cpuChildList.add("CpuPerc"); }
		 * counters.put(RequestCommand.COMMAND_CPU, cpuChildList);
		 * 
		 * // Disk JSONArray diskChildList = new JSONArray(); int diskCount =
		 * collector.getFileSystemList().length; for (int i = 1; i <= diskCount;
		 * i++) { JSONObject diskCounters = new JSONObject();
		 * diskCounters.put(RequestCommand.COMMAND_DISK + "#" + i,
		 * buildIndexCmd(new String[] { RequestCommand.COMMAND_DISK_SIZE,
		 * RequestCommand.COMMAND_DISK_USED, RequestCommand.COMMAND_DISK_AVAIL,
		 * RequestCommand.COMMAND_DISK_USED_PERCENT,
		 * RequestCommand.COMMAND_DISK_MOUNTED, RequestCommand.COMMAND_DISK_TYPE
		 * }, i)); diskChildList.add(diskCounters); }
		 * counters.put(RequestCommand.COMMAND_DISK, diskChildList);
		 */

		// Memory
		Counter memRamCounter = new Counter(RequestCommand.COMMAND_MEMORY_RAM,
				RequestCommand.COMMAND_MEMORY_RAM, null);
		Counter memSwapFreeCounter = new Counter(
				RequestCommand.COMMAND_FREE_SWAP,
				RequestCommand.COMMAND_FREE_SWAP, null);
		Counter memSwapUsedCounters = new Counter(
				RequestCommand.COMMAND_USED_SWAP,
				RequestCommand.COMMAND_USED_SWAP, null);
		Counter memSwapCounter = new Counter(
				RequestCommand.COMMAND_MEMORY_SWAP,
				RequestCommand.COMMAND_MEMORY_SWAP, addToList(new Counter[] {
						memSwapUsedCounters, memSwapFreeCounter }));
		Counter memFreeCounter = new Counter(
				RequestCommand.COMMAND_FREE_MEMORY,
				RequestCommand.COMMAND_FREE_MEMORY, null);
		Counter memUsedCounters = new Counter(
				RequestCommand.COMMAND_USED_MEMORY,
				RequestCommand.COMMAND_USED_MEMORY, null);
		Counter memCounters = new Counter(RequestCommand.COMMAND_MEMORY,
				RequestCommand.COMMAND_MEMORY, addToList(new Counter[] {
						memUsedCounters, memFreeCounter, memSwapCounter,
						memRamCounter }));

		// Disk
		FileSystemInfo[] diskInfo = collector.getFileSystemList();
		int diskCount = diskInfo.length;
		ArrayList<Counter> diskChildCounters = new ArrayList<Counter>();
		for (int i = 0; i < diskCount; i++) {
			ArrayList<Counter> diskIndexChildCounters = buildIndexCmd(
					new String[] { RequestCommand.COMMAND_DISK_SIZE,
							RequestCommand.COMMAND_DISK_USED_PERCENT,
							RequestCommand.COMMAND_DISK_USED,
							RequestCommand.COMMAND_DISK_TYPE,
							RequestCommand.COMMAND_DISK_MOUNTED,
							RequestCommand.COMMAND_DISK_AVAIL }, i);
			Counter diskIndexCounter = buildCounter(RequestCommand.COMMAND_DISK
					+ "#" + i, diskInfo[i].getDevName(), diskIndexChildCounters);

			diskChildCounters.add(diskIndexCounter);
		}
		Counter diskCounters = new Counter(RequestCommand.COMMAND_DISK,
				RequestCommand.COMMAND_DISK, diskChildCounters);

		// Cpu
		int cpuCount = collector.getCpuInfoList().length;
		ArrayList<Counter> cpuChildCounters = new ArrayList<Counter>();
		for (int i = 0; i < cpuCount; i++) {
			ArrayList<Counter> cpuIndexChildCounters = buildIndexCmd(
					new String[] { RequestCommand.COMMAND_CPU_COMBINED,
							RequestCommand.COMMAND_CPU_CORES,
							RequestCommand.COMMAND_CPU_IDLETIME,
							RequestCommand.COMMAND_CPU_IRQTIME,
							RequestCommand.COMMAND_CPU_MHZ,
							RequestCommand.COMMAND_CPU_MODEL,
							RequestCommand.COMMAND_CPU_NICETIME,
							RequestCommand.COMMAND_CPU_SYSTIME,
							RequestCommand.COMMAND_CPU_USERTIME,
							RequestCommand.COMMAND_CPU_VENDOR,
							RequestCommand.COMMAND_CPU_WAITTIME }, i);
			Counter cpuIndexCounter = buildCounter(RequestCommand.COMMAND_CPU
					+ "#" + i, RequestCommand.COMMAND_CPU + "#" + (i + 1),
					cpuIndexChildCounters);
			cpuChildCounters.add(cpuIndexCounter);
		}
		Counter cpuCounters = new Counter(RequestCommand.COMMAND_CPU,
				RequestCommand.COMMAND_CPU, cpuChildCounters);

		ArrayList<Counter> counters = new ArrayList<Counter>();
		counters.add(memCounters);
		counters.add(diskCounters);
		counters.add(cpuCounters);
		return JSONValue.toJSONString(counters);

	}

	private ArrayList<Counter> buildIndexCmd(String[] cmds, int index) {
		ArrayList<Counter> counters = new ArrayList<Counter>();
		for(String cmd : cmds) {
			if (cmdTextMap.containsKey(cmd))
				counters.add(new Counter(cmd + "#" + index,
						cmdTextMap.get(cmd), null));
		}
		return counters;
	}

	private Counter buildCounter(String cmd, String text,
			ArrayList<Counter> child) {
		return new Counter(cmd, text, child);
	}

	private ArrayList<Counter> addToList(Counter[] counters) {
		ArrayList<Counter> counterList = new ArrayList<Counter>();
		for (Counter c : counters) {
			counterList.add(c);
		}
		return counterList;
	}

	/*
	 * private JSONArray buildIndexCmd(String[] cmds, int index) { JSONArray
	 * cmdList = new JSONArray(); for (int i = 0, count = cmds.length; i <
	 * count; i++) { cmdList.add(cmds[i] + "#" + index); } return cmdList; }
	 */

	public String toString() {
		return toJson();
	}
}
