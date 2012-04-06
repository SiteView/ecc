package com.siteview.agent;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CommandMatch {
	private CommandMatch() {
	}

	public static CommandMatch matchMultiFormat(String cmd) {
		Pattern pattern = Pattern.compile("([^#]+)#(\\d+)");
		final Matcher m = pattern.matcher(cmd);
		if (m.find()) {
			return new CommandMatch() {
				public boolean success() {
					return true;
				}
				
				public String getCommand() {
					return m.group(1);
				}

				public int getIndex() {
					return Integer.parseInt(m.group(2));
				}
			};
		}
		
		return new CommandMatch() {
			public boolean success() {
				return false;
			}
		};
	}

	public boolean success() {
		return false;
	}

	public String getCommand() {
		return null;
	}

	public int getIndex() {
		return -1;
	}

}
