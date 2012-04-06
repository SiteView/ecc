package com.siteview.agent;

public class ShellValidator {

	public static boolean Validate(String cmd) {
		return ("start".equalsIgnoreCase(cmd)
				|| "stop".equalsIgnoreCase(cmd)
				|| "restart".equalsIgnoreCase(cmd)
				|| "ping".equalsIgnoreCase(cmd)
				|| "status".equalsIgnoreCase(cmd));
	}

}
