package com.siteview.agent;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;

public class CpuCollector {
	private Sigar sigar = new Sigar();

	public int getCount() throws CollectorException {
		try {
			return sigar.getCpuInfoList().length;
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU);
		}
	}

	public String getVendor(int index) throws CollectorException {
		try {
			return sigar.getCpuInfoList()[index].getVendor();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_VENDOR);
		}
	}

	public double getWaitTime(int index) throws CollectorException {
		try {
			return sigar.getCpuPercList()[index].getWait();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_WAITTIME);
		}
	}

	public double getUserTime(int index) throws CollectorException {
		try {
			return sigar.getCpuPercList()[index].getUser();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_USERTIME);
		}
	}

	public double getNiceTime(int index) throws CollectorException {
		try {
			return sigar.getCpuPercList()[index].getNice();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_NICETIME);
		}
	}

	public String getModel(int index) throws CollectorException {
		try {
			return sigar.getCpuInfoList()[index].getModel();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_MODEL);
		}
	}

	public int getMhz(int index) throws CollectorException {
		try {
			return sigar.getCpuInfoList()[index].getMhz();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_MHZ);
		}
	}

	public double getIrqTime(int index) throws CollectorException {
		try {
			return sigar.getCpuPercList()[index ].getIrq();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_IRQTIME);
		}
	}

	public double getIdelTime(int index) throws CollectorException {
		try {
			return sigar.getCpuPercList()[index].getIdle();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_IDLETIME);
		}
	}

	public int getCores(int index) throws CollectorException {
		try {
			return sigar.getCpuInfoList()[index].getTotalCores();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_CORES);
		}
	}

	public double getCombined(int index) throws CollectorException {
		try {
			return sigar.getCpuPercList()[index].getCombined();
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_CPU_COMBINED);
		}
	}

	public static String format(double val) {
		String p = String.valueOf(val * 100.0D);

		int ix = p.indexOf(".") + 1;
		String percent = p.substring(0, ix) + p.substring(ix, ix + 1);

		return percent + "%";
	}
}
