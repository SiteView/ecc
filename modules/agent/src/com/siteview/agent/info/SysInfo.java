package com.siteview.agent.info;

import java.util.HashMap;
import java.util.Map;

public class SysInfo extends Info {
	String name = null;

	String version = null;

	String arch = null;

	String machine = null;

	String description = null;

	String patchLevel = null;

	String vendor = null;

	String vendorVersion = null;

	String vendorName = null;

	String vendorCodeName = null;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public String getArch() {
		return arch;
	}

	public void setArch(String arch) {
		this.arch = arch;
	}

	public String getMachine() {
		return machine;
	}

	public void setMachine(String machine) {
		this.machine = machine;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getPatchLevel() {
		return patchLevel;
	}

	public void setPatchLevel(String patchLevel) {
		this.patchLevel = patchLevel;
	}

	public String getVendor() {
		return vendor;
	}

	public void setVendor(String vendor) {
		this.vendor = vendor;
	}

	public String getVendorVersion() {
		return vendorVersion;
	}

	public void setVendorVersion(String vendorVersion) {
		this.vendorVersion = vendorVersion;
	}

	public String getVendorName() {
		return vendorName;
	}

	public void setVendorName(String vendorName) {
		this.vendorName = vendorName;
	}

	public String getVendorCodeName() {
		return vendorCodeName;
	}

	public void setVendorCodeName(String vendorCodeName) {
		this.vendorCodeName = vendorCodeName;
	}

	@Override
	public Map<String, Object> toMap() {
		Map<String, Object> map = new HashMap<String, Object>();
		map.put("Name", name);
		map.put("Version", version);
		map.put("Arch", arch);
		map.put("Machine", machine);
		map.put("Description", description);
		map.put("PatchLevel", patchLevel);
		map.put("Vendor", vendor);
		map.put("VendorVersion", vendorVersion);
		map.put("VendorName", vendorName);
		map.put("VendorCodeName", vendorCodeName);
		return map;
	}
}
