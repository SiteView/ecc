package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class ResourceLimitInfo extends Info {
	long cpuCur = 0L;

	long cpuMax = 0L;

	long fileSizeCur = 0L;

	long fileSizeMax = 0L;

	long pipeSizeMax = 0L;

	long pipeSizeCur = 0L;

	long dataCur = 0L;

	long dataMax = 0L;

	long stackCur = 0L;

	long stackMax = 0L;

	long coreCur = 0L;

	long coreMax = 0L;

	long memoryCur = 0L;

	long memoryMax = 0L;

	long processesCur = 0L;

	long processesMax = 0L;

	long openFilesCur = 0L;

	long openFilesMax = 0L;

	long virtualMemoryCur = 0L;

	long virtualMemoryMax = 0L;

	public long getCpuCur() {
		return cpuCur;
	}

	public void setCpuCur(long cpuCur) {
		this.cpuCur = cpuCur;
	}

	public long getCpuMax() {
		return cpuMax;
	}

	public void setCpuMax(long cpuMax) {
		this.cpuMax = cpuMax;
	}

	public long getFileSizeCur() {
		return fileSizeCur;
	}

	public void setFileSizeCur(long fileSizeCur) {
		this.fileSizeCur = fileSizeCur;
	}

	public long getFileSizeMax() {
		return fileSizeMax;
	}

	public void setFileSizeMax(long fileSizeMax) {
		this.fileSizeMax = fileSizeMax;
	}

	public long getPipeSizeMax() {
		return pipeSizeMax;
	}

	public void setPipeSizeMax(long pipeSizeMax) {
		this.pipeSizeMax = pipeSizeMax;
	}

	public long getPipeSizeCur() {
		return pipeSizeCur;
	}

	public void setPipeSizeCur(long pipeSizeCur) {
		this.pipeSizeCur = pipeSizeCur;
	}

	public long getDataCur() {
		return dataCur;
	}

	public void setDataCur(long dataCur) {
		this.dataCur = dataCur;
	}

	public long getDataMax() {
		return dataMax;
	}

	public void setDataMax(long dataMax) {
		this.dataMax = dataMax;
	}

	public long getStackCur() {
		return stackCur;
	}

	public void setStackCur(long stackCur) {
		this.stackCur = stackCur;
	}

	public long getStackMax() {
		return stackMax;
	}

	public void setStackMax(long stackMax) {
		this.stackMax = stackMax;
	}

	public long getCoreCur() {
		return coreCur;
	}

	public void setCoreCur(long coreCur) {
		this.coreCur = coreCur;
	}

	public long getCoreMax() {
		return coreMax;
	}

	public void setCoreMax(long coreMax) {
		this.coreMax = coreMax;
	}

	public long getMemoryCur() {
		return memoryCur;
	}

	public void setMemoryCur(long memoryCur) {
		this.memoryCur = memoryCur;
	}

	public long getMemoryMax() {
		return memoryMax;
	}

	public void setMemoryMax(long memoryMax) {
		this.memoryMax = memoryMax;
	}

	public long getProcessesCur() {
		return processesCur;
	}

	public void setProcessesCur(long processesCur) {
		this.processesCur = processesCur;
	}

	public long getProcessesMax() {
		return processesMax;
	}

	public void setProcessesMax(long processesMax) {
		this.processesMax = processesMax;
	}

	public long getOpenFilesCur() {
		return openFilesCur;
	}

	public void setOpenFilesCur(long openFilesCur) {
		this.openFilesCur = openFilesCur;
	}

	public long getOpenFilesMax() {
		return openFilesMax;
	}

	public void setOpenFilesMax(long openFilesMax) {
		this.openFilesMax = openFilesMax;
	}

	public long getVirtualMemoryCur() {
		return virtualMemoryCur;
	}

	public void setVirtualMemoryCur(long virtualMemoryCur) {
		this.virtualMemoryCur = virtualMemoryCur;
	}

	public long getVirtualMemoryMax() {
		return virtualMemoryMax;
	}

	public void setVirtualMemoryMax(long virtualMemoryMax) {
		this.virtualMemoryMax = virtualMemoryMax;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("CpuCur", cpuCur);
		map.put("CpuMax", cpuMax);
		map.put("FileSizeCur", fileSizeCur);
		map.put("FileSizeMax", fileSizeMax);
		map.put("PipeSizeMax", pipeSizeMax);
		map.put("PipeSizeCur", pipeSizeCur);
		map.put("DataCur", dataCur);
		map.put("DataMax", dataMax);
		map.put("StackCur", stackCur);
		map.put("StackMax", stackMax);
		map.put("CoreCur", coreCur);
		map.put("CoreMax", coreMax);
		map.put("MemoryCur", memoryCur);
		map.put("MemoryMax", memoryMax);
		map.put("ProcessesCur", processesCur);
		map.put("ProcessesMax", processesMax);
		map.put("OpenFilesCur", openFilesCur);
		map.put("OpenFilesMax", openFilesMax);
		map.put("VirtualMemoryCur", virtualMemoryCur);
		map.put("VirtualMemoryMax", virtualMemoryMax);
		return map;
	}
}
