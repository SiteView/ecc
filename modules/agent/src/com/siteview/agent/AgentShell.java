package com.siteview.agent;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Properties;

public class AgentShell {
	private static Agent agent;

	public static void main(String[] arstring) {
		int argsCount = arstring.length;

		if (argsCount > 1) {
			print("命令选项过多", true);
			return;
		}

		Properties props;
		try {
			props = ConfigLoader.Load(AgentShell.class,"/conf/agent.conf");
		} catch (FileNotFoundException e) {
			print("加载配置文件出错,文件不存在",true);
			return;
		} catch (IOException e) {
			print("读取配置文件出错",true);
			return;
		}
		
		agent = new Agent(props);
		if (argsCount < 1)
			doInteractiveMode();
		else
			doCommandMode(arstring[0]);
	}

	private static void doCommandMode(String cmd) {
		if (ShellValidator.Validate(cmd)) {
			boolean result = agent.Run(cmd);

			if (result)
				doInteractiveMode();
			else {
				return;
			}
		} else {
			print("命令模式命令验证不通过", true);
		}
	}

	private static void doInteractiveMode() {
		InputStreamReader inputstreamreader = new InputStreamReader(System.in);
		BufferedReader reader = new BufferedReader(inputstreamreader);

		print("agent>", false);
		String cmd = null;
		try {
			while ((cmd = reader.readLine()) != null) {
				if (cmd.equalsIgnoreCase("exit"))
					break;
				// 验证命令有效性
				if (ShellValidator.Validate(cmd)) {
					agent.Run(cmd);
				} else {
					print("交互模式命令验证不通过", true);
				}
				print("agent>", false);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static void print(String cmd, boolean isBreak) {
		if (isBreak)
			System.out.print(cmd + "\n");
		else
			System.out.print("\b\b\b\b\b\b" + cmd);
	}
}
