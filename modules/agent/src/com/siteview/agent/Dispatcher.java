package com.siteview.agent;

import java.util.Map;

import com.siteview.agent.info.LiteralInfo;

public class Dispatcher {

	private Log logger = Log.getInstance(Dispatcher.class);
	private Collector collector;
	private MemoryCollector memoryCollector = new MemoryCollector();
	private CpuCollector cpuCollector = new CpuCollector();
	private DiskCollector diskCollector = new DiskCollector();

	/*
	 * private static final Map<String, String> commandMap; static { commandMap
	 * = new HashMap<String, String>(); commandMap.put("CpuInfoList",
	 * "getCpuInfoList"); }
	 */

	public Dispatcher(Collector collector) {
		this.collector = collector;
	}

	public Response ProcessRequest(RequestBase request) {
		logger.info("Received Request", request.toString());
		if (request.valid()) {
			try {
				return processRequest(request);
			} catch (CollectorException e) {
				logger.error("ProcessRequest", e);
				return Response.Invalid;
			}
		}
		return Response.Invalid;
	}

	private LiteralInfo buildLiteralInfo(String key, Object value) {
		return new LiteralInfo(key, value);
	}

	private Response processRequest(RequestBase request)
			throws CollectorException {
		logger.info("process the request", request.toString());
		String cmd = request.getCommand();
		Map<String, String> params = request.getCommandParam();

		String cmdText = cmd;
		
		// Memory
		if (RequestCommand.COMMAND_MEMORY.equalsIgnoreCase(cmd))
			return new Response(buildLiteralInfo(cmdText,memoryCollector.getMemory()));

		if (RequestCommand.COMMAND_USED_MEMORY.equalsIgnoreCase(cmd))
			return new Response(buildLiteralInfo(cmdText,memoryCollector.getUsedMemory()));

		if (RequestCommand.COMMAND_FREE_MEMORY.equalsIgnoreCase(cmd))
			return new Response(buildLiteralInfo(cmdText,memoryCollector.getFreeMemory()));

		if (RequestCommand.COMMAND_MEMORY_SWAP.equalsIgnoreCase(cmd))
			return new Response(buildLiteralInfo(cmdText,memoryCollector.getMemorySwap()));

		if (RequestCommand.COMMAND_USED_SWAP.equalsIgnoreCase(cmd))
			return new Response(buildLiteralInfo(cmdText,memoryCollector.getUsedSwap()));

		if (RequestCommand.COMMAND_FREE_SWAP.equalsIgnoreCase(cmd))
			return new Response(buildLiteralInfo(cmdText,memoryCollector.getFreeSwap()));

		if (RequestCommand.COMMAND_MEMORY_RAM.equalsIgnoreCase(cmd))
			return new Response(buildLiteralInfo(cmdText,memoryCollector.getRAM()));

		// Cpu
		if (RequestCommand.COMMAND_CPU.equalsIgnoreCase(cmd))
			return new Response(buildLiteralInfo(cmdText, cpuCollector.getCount()));

		// Disk
		if (RequestCommand.COMMAND_DISK.equalsIgnoreCase(cmd))
			return new Response(buildLiteralInfo(cmdText, diskCollector.getCount()));

		// 针对多实例计数器
		CommandMatch match = CommandMatch.matchMultiFormat(cmd);
		if (match.success()) {
			String theCmd = match.getCommand();
			int index = match.getIndex();

			// Cpu Multi
			if (RequestCommand.COMMAND_CPU.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, index + 1));

			if (RequestCommand.COMMAND_CPU_VENDOR.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, cpuCollector
						.getVendor(index)));

			if (RequestCommand.COMMAND_CPU_COMBINED.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, CpuCollector
						.format(cpuCollector.getCombined(index))));

			if (RequestCommand.COMMAND_CPU_CORES.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, cpuCollector
						.getCores(index)));

			if (RequestCommand.COMMAND_CPU_IDLETIME.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, CpuCollector
						.format(cpuCollector.getIdelTime(index))));

			if (RequestCommand.COMMAND_CPU_IRQTIME.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, CpuCollector
						.format(cpuCollector.getIrqTime(index))));

			if (RequestCommand.COMMAND_CPU_MHZ.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, cpuCollector
						.getMhz(index)));

			if (RequestCommand.COMMAND_CPU_MODEL.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, cpuCollector
						.getModel(index)));

			if (RequestCommand.COMMAND_CPU_NICETIME.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, CpuCollector
						.format(cpuCollector.getNiceTime(index))));

			if (RequestCommand.COMMAND_CPU_SYSTIME.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, CpuCollector
						.format(cpuCollector.getUserTime(index))));

			if (RequestCommand.COMMAND_CPU_USERTIME.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, CpuCollector
						.format(cpuCollector.getUserTime(index))));

			if (RequestCommand.COMMAND_CPU_WAITTIME.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, CpuCollector
						.format(cpuCollector.getWaitTime(index))));

			if (RequestCommand.COMMAND_DISK.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, diskCollector
						.getLable(index)));

			if (RequestCommand.COMMAND_DISK_SIZE.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, diskCollector
						.getSize(index)));

			if (RequestCommand.COMMAND_DISK_USED.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, diskCollector
						.getUsed(index)));

			if (RequestCommand.COMMAND_DISK_AVAIL.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, diskCollector
						.getAvail(index)));

			if (RequestCommand.COMMAND_DISK_USED_PERCENT
					.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, diskCollector
						.getUsedPercent(index)));

			if (RequestCommand.COMMAND_DISK_MOUNTED.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, diskCollector
						.getMounted(index)));

			if (RequestCommand.COMMAND_DISK_TYPE.equalsIgnoreCase(theCmd))
				return new Response(buildLiteralInfo(cmdText, diskCollector
						.getType(index)));
		}

		if ("CpuInfoList".equalsIgnoreCase(cmd))
			return new Response(collector.getCpuInfoList());

		if ("CpuPerc".equalsIgnoreCase(cmd))
			return new Response(collector.getCpuPerc());

		if ("CpuPercList".equalsIgnoreCase(cmd))
			return new Response(collector.getCpuPercList());

		if ("DirStat".equalsIgnoreCase(cmd))
			return new Response(collector.getDirStat(params.get("Name")));

		if ("DirUsage".equalsIgnoreCase(cmd))
			return new Response(collector.getDirUsage(params.get("Name")));

		if ("DiskUsage".equalsIgnoreCase(cmd))
			return new Response(collector.getDiskUsage(params.get("Name")));

		if ("FQDN".equalsIgnoreCase(cmd))
			return new Response(new LiteralInfo("FQDN", collector.getFQDN()));

		if ("FileSystemList".equalsIgnoreCase(cmd))
			return new Response(collector.getFileSystemList());

		if ("FileSystemUsage".equalsIgnoreCase(cmd))
			return new Response(collector
					.getFileSystemUsage(params.get("Name")));

		if ("LoadAverage".equalsIgnoreCase(cmd))
			return new Response(new LiteralInfo("LoadAverage", collector
					.getLoadAverage()));

		if ("Mem".equalsIgnoreCase(cmd))
			return new Response(collector.getMem());

		if ("MountedFileSystemUsage".equalsIgnoreCase(cmd))
			return new Response(collector.getMountedFileSystemUsage(params
					.get("Name")));

		if ("MultiProcCpu".equalsIgnoreCase(cmd))
			return new Response(collector.getMultiProcCpu(params.get("Query")));

		if ("MultiProcMem".equalsIgnoreCase(cmd))
			return new Response(collector.getMultiProcMem(params.get("Query")));

		if ("NetConnectionList".equalsIgnoreCase(cmd))
			return new Response(collector.getNetConnectionList(Integer
					.parseInt(params.get("Query"))));

		if ("NetInfo".equalsIgnoreCase(cmd))
			return new Response(collector.getNetInfo());

		if ("NetInterfaceConfigList".equalsIgnoreCase(cmd))
			return new Response(collector.getNetInterfaceConfig());

		if ("NetInterfaceConfig".equalsIgnoreCase(cmd))
			return new Response(collector
					.getFileSystemUsage(params.get("Name")));

		/*
		 * if("NetInterfaceList".equalsIgnoreCase(cmd)) return new
		 * Response(collector.getNetInterfaceList());
		 */

		if ("NetInterfaceStat".equalsIgnoreCase(cmd))
			return new Response(collector.getNetInterfaceStat(params
					.get("Name")));

		/*
		 * if("NetListenAddress".equalsIgnoreCase(cmd)) return new
		 * Response(collector.getNetListenAddress());
		 */

		if ("NetRouteList".equalsIgnoreCase(cmd))
			return new Response(collector.getNetRouteList());

		/*
		 * if("NetServicesName".equalsIgnoreCase(cmd)) return new
		 * Response(collector
		 * .getNetServicesName(Integer.parseInt(params.get("Protocol"
		 * )),Long.parseLong(params.get("Port"))));
		 */

		if (RequestCommand.COMMAND_COMMON_TEST.equalsIgnoreCase(cmd))
			return new Response(new LiteralInfo("State", "Running"));

		if (RequestCommand.COMMAND_COMMON_COUNTERS.equalsIgnoreCase(cmd))
			return new CountersResponse(this.collector);

		return Response.Invalid;

		/*
		 * if (commandMap.containsKey(cmd)) { try { Object result =
		 * ReflectUtil.invokeMethod(collector, commandMap.get(cmd),null);
		 * if(result instanceof Info) return new Response((Info)result);
		 * if(result instanceof Info[]) return new Response((Info[])result); }
		 * catch (Exception e) { return Response.Invalid; } }
		 */
	}

}
