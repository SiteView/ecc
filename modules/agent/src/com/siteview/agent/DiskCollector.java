package com.siteview.agent;

import org.hyperic.sigar.FileSystem;
import org.hyperic.sigar.FileSystemUsage;
import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;

public class DiskCollector {
	private Sigar sigar = new Sigar();

	public int getCount() throws CollectorException {
		try {
			return sigar.getFileSystemList().length;
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_DISK);
		}
	}

	public String getType(int index) throws CollectorException {
		try {
			FileSystem fs = sigar.getFileSystemList()[index];
			return fs.getSysTypeName() + "/" + fs.getTypeName();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_DISK_TYPE);
		}
	}

	public String getMounted(int index) throws CollectorException {
		try {
			return sigar.getFileSystemList()[index].getDirName();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_DISK_MOUNTED);
		}
	}

	public String getUsedPercent(int index) throws CollectorException {
		try {
			return getUsage(index).getUsePercent() * 100.0D + "%";
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_DISK_USED_PERCENT);
		}
	}

	public long getAvail(int index) throws CollectorException {
		try {
			return getUsage(index).getAvail();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_DISK_AVAIL);
		}
	}

	public long getUsed(int index) throws CollectorException {
		try {
			return getUsage(index).getUsed();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_DISK_USED);
		}
	}

	public long getSize(int index) throws CollectorException {
		try {
			return getUsage(index).getTotal();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_DISK_SIZE);
		}
	}

	public String getLable(int index) throws CollectorException {
		try {
			return sigar.getFileSystemList()[index].getDevName();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_DISK);
		}
	}

	private FileSystemUsage getUsage(int index) throws SigarException {
		FileSystem fs = sigar.getFileSystemList()[index];
		return this.sigar.getFileSystemUsage(fs.getDirName());
	}
}
