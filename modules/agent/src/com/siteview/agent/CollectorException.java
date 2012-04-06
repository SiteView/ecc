package com.siteview.agent;

public class CollectorException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4590327254081365792L;
	
	private String command;
	
	public CollectorException(String command) {
		this.command = command;
	}

	public String getCommand() {
		return this.command;
	}
}
