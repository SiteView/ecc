package com.siteview.agent;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

//Request格式定义：["diskspace",{"name":"C","size":123}]

public class Request extends RequestBase {
	private String originRequest;
	private String command;
	private Map<String, String> params = null;

	private static Set<String> permissionCmds = null;

	static {
		permissionCmds = new HashSet<String>();
		permissionCmds.add(RequestCommand.COMMAND_COMMON_TEST);
		permissionCmds.add(RequestCommand.COMMAND_COMMON_COUNTERS);

		// Memory
		permissionCmds.add(RequestCommand.COMMAND_MEMORY);
		permissionCmds.add(RequestCommand.COMMAND_MEMORY_RAM);
		permissionCmds.add(RequestCommand.COMMAND_MEMORY_SWAP);
		permissionCmds.add(RequestCommand.COMMAND_FREE_SWAP);
		permissionCmds.add(RequestCommand.COMMAND_USED_SWAP);
		permissionCmds.add(RequestCommand.COMMAND_FREE_MEMORY);
		permissionCmds.add(RequestCommand.COMMAND_USED_MEMORY);

		// Cpu
		permissionCmds.add(RequestCommand.COMMAND_CPU);
		permissionCmds.add(RequestCommand.COMMAND_CPU_VENDOR);
		permissionCmds.add(RequestCommand.COMMAND_CPU_MODEL);
		permissionCmds.add(RequestCommand.COMMAND_CPU_MHZ);
		permissionCmds.add(RequestCommand.COMMAND_CPU_CORES);
		permissionCmds.add(RequestCommand.COMMAND_CPU_USERTIME);
		permissionCmds.add(RequestCommand.COMMAND_CPU_SYSTIME);
		permissionCmds.add(RequestCommand.COMMAND_CPU_IDLETIME);
		permissionCmds.add(RequestCommand.COMMAND_CPU_WAITTIME);
		permissionCmds.add(RequestCommand.COMMAND_CPU_NICETIME);
		permissionCmds.add(RequestCommand.COMMAND_CPU_IRQTIME);
		permissionCmds.add(RequestCommand.COMMAND_CPU_COMBINED);
		
		// Disk
		permissionCmds.add(RequestCommand.COMMAND_DISK);
		permissionCmds.add(RequestCommand.COMMAND_DISK_SIZE);
		permissionCmds.add(RequestCommand.COMMAND_DISK_USED);
		permissionCmds.add(RequestCommand.COMMAND_DISK_AVAIL);
		permissionCmds.add(RequestCommand.COMMAND_DISK_USED_PERCENT);
		permissionCmds.add(RequestCommand.COMMAND_DISK_MOUNTED);
		permissionCmds.add(RequestCommand.COMMAND_DISK_TYPE);
	}

	public Request(String originRequest) {
		this.originRequest = originRequest;
		build(originRequest);
	}

	/*
	 * protected int getParamLimitCount() { return 2; }
	 */

	private void build(String originRequest) {
		this.params = new HashMap<String, String>();
		Object obj = JSONValue.parse(originRequest);
		JSONArray cmd = (JSONArray) obj;
		// int commandParamCount = cmd.size();
		// if (commandParamCount != getParamLimitCount())
		// throw new Exception("不符合请求格式");
		this.command = cmd.get(0).toString();
		String param = cmd.get(1).toString();
		JSONObject paramObj = (JSONObject) JSONValue.parse(param);
		for (Object key : paramObj.keySet()) {
			this.params.put(key.toString(), paramObj.get(key).toString());
		}
	}

	public String getOriginRequest() {
		return originRequest;
	}

	public boolean valid() {
		boolean single = permissionCmds.contains(command);
		boolean multi = false;
		CommandMatch match = CommandMatch.matchMultiFormat(this.command);
		if (match.success()) {
			String multiCmd = match.getCommand();
			multi = permissionCmds.contains(multiCmd);
		}
		
		return single || multi;
	}

	public String getCommand() {
		return this.command;
	}

	public String toString() {
		return this.originRequest;
	}

	@Override
	public Map<String, String> getCommandParam() {
		return this.params;
	}
}
