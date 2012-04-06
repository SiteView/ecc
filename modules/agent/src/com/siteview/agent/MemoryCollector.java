package com.siteview.agent;

import org.hyperic.sigar.Sigar;
import org.hyperic.sigar.SigarException;


public class MemoryCollector {
	private Sigar sigar = new Sigar();

	long getMemory() throws CollectorException {
		try {
			return format(sigar.getMem().getTotal());
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_MEMORY);
		}
	}

	long getUsedMemory() throws CollectorException {
		try {
			return format(sigar.getMem().getUsed());
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_USED_MEMORY);
		}
	}

	long getFreeMemory() throws CollectorException {
		try {
			return format(sigar.getMem().getFree());
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_FREE_MEMORY);
		}
	}

	long getMemorySwap() throws CollectorException {
		try {
			return format(sigar.getSwap().getTotal());
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_MEMORY_SWAP);
		}
	}

	long getUsedSwap() throws CollectorException {
		try {
			return format(sigar.getSwap().getUsed());
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_USED_SWAP);
		}
	}

	long getFreeSwap() throws CollectorException {
		try {
			return format(sigar.getSwap().getFree());
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_FREE_SWAP);
		}
	}

	long getRAM() throws CollectorException {
		try {
			return format(sigar.getMem().getRam());
		} catch (SigarException e) {
			throw new CollectorException(RequestCommand.COMMAND_MEMORY_RAM);
		}
	}

	private static long format(long value) {
		return value / 1024L;
	}
}
