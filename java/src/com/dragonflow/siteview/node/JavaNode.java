package com.dragonflow.siteview.node;

import java.io.File;
import java.io.IOException;

import com.dragonflow.erlangecc.monitor.BaseMonitor;
import com.dragonflow.erlangecc.util.SvMessage;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpMsg;
import com.ericsson.otp.erlang.OtpNode;

public class JavaNode {
	public static final String ERLANG_NODE = "debug@test-52";
	public static final int TIME_OUT = 2000;
	public static final String DEFAULT_MAIL_BOX = "java_mail_box";
	public static final String JAVA_NODE = "java_monitor";
	public static final String COOKIE = "3ren";
	public static final long RECEIVE_TIME_OUT = 1000 * 60 * 60;
	public static final String UNKOWN_REASONS = "unkown reasons. ";
	public static final String ERROR = "java_node_error";
	public static final String DEFAULT_STATE_KEY = "ok";
	public static final String DEFAULT_STATE_VALUE = "ok";
	public static final String CONTINUE_STATE_KEY = "ok";
	public static final String CONTINE_STATE = "continue";
	
	public static String getLibPath(){
		String strCur = System.getProperty("user.dir");
		if (strCur.endsWith(";"))
			strCur.replaceAll(";", "");
		return strCur + File.separator + "lib";
	}
	
	public static String getCurrentDir(){
		String strCur = System.getProperty("user.dir");
		if (strCur.endsWith(";"))
			strCur.replaceAll(";", "");
		return strCur;
	}
	

	public static void main(String[] args) throws IOException {

		String v = System.getProperties().getProperty("OtpConnection.trace");
		System.out.println("OtpConnection.trace=" + v);
		OtpNode server = null;
		java.io.File f=new java.io.File("longname");
		if(f.exists())
		{
			String ipaddr = java.net.Inet4Address.getLocalHost().getHostAddress();
			System.out.println("server ip is :  " + ipaddr);
			server = new OtpNode(JAVA_NODE + "@" + ipaddr, COOKIE);
		}else
		{
			server = new OtpNode(JAVA_NODE, COOKIE);
		}

		System.out.println("server node is :  " + server.node());
		System.out.println("cookie is :  " + server.cookie());

		OtpMbox serverMbox = server.createMbox(DEFAULT_MAIL_BOX);

		String[] nodeNames = server.getNames(); // equivalent to nodes() in
		// Erlang.
		for (int i = 0; i < nodeNames.length; i++) {
			System.out.println("name:" + nodeNames[i]);
		}
		//System.out.println("ping erlang node: " + server.ping(ERLANG_NODE, TIME_OUT));
		int try_count = 1;

//		OtpErlangPid from = null;
		while (true) {
//			OtpErlangObject[] response = new OtpErlangObject[3];
//			Map<String, String> results = new HashMap<String, String>();
//			OtpErlangList list = new OtpErlangList();
			
			String[] states = { DEFAULT_STATE_KEY, DEFAULT_STATE_VALUE };
			try {
				System.out.println("the " + try_count + "th receive messages : ----- " + try_count++);
				OtpMsg msg = serverMbox.receiveMsg(RECEIVE_TIME_OUT);
				
				//System.out.println("raw message from erlang: " + msg.getMsg());
				SvMessage message = new SvMessage(serverMbox);
				
				message.fromOtpMsg(msg);
				
				Class cls = Class.forName(message.getRequestType());
				
				BaseMonitor monitor = (BaseMonitor)cls.newInstance();
				//System.out.println("Erlang To Java:" +message.getParams().toString());
				monitor.setMessage(message);
				monitor.start();
				
				/*
				
				OtpErlangObject request = msg.getMsg();
				from = msg.getSenderPid();
				System.out.println("from pid: " + from);
				System.out.println("receive[raw]:  " + request);

				if (request instanceof OtpErlangTuple) {
					OtpErlangTuple t = (OtpErlangTuple) request;
					Map<String, String> map = ErlangUtils.getRequestFromTuple(t);

					// OtpErlangPid send_pid =
					// (OtpErlangPid)map.get("from_pid");
					// System.out.println("from pid " + send_pid);

					System.out.println("after parse received request to map: \n" + map);

					String request_type = map.get("monitor_type");
					if (request_type != null && request_type.equals(Monitors.MONITOR_STOP)) {
						System.out.println("java node closed becase of receiving command: " + request_type);
						server.close();
						break;
					} else if (request_type != null && request_type.equals(Monitors.MONITOR_CONTINUE)) {
						Thread.sleep(1000);
						System.out.println("continue command,  sleepppppppppppppppppppppppppppppp");
						states[0] = DEFAULT_STATE_KEY;
						states[1] = "wait continue...";
						list = ErlangUtils.erlangListAdd(list, ErlangUtils.createTuple(new OtpErlangAtom(CONTINE_STATE), new OtpErlangString(states[1])));
						continue;
					}
					
					Class clazz = Class.forName(map.get("monitor_type"));
					MonitorUpdate action = Action.createAction(clazz);
					action.initArgs(map);
					
					results = action.update();
					list = ErlangUtils.map2ErlangTupleList(results);
					// System.out.println(list);

					response[2] = list;
					// System.out.println("test--------------------" +
					// request_type);

				}
				*/
			} catch (OtpErlangExit e) {
				// receiveMsg error
				states[0] = ERROR;
				states[1] = "java node receive message error: " + e.getMessage();
				e.printStackTrace();
				//list = ErlangUtils.erlangListAdd(list, ErlangUtils.createTuple(new OtpErlangAtom(states[0]), new OtpErlangString(states[1])));
/*			} catch (OtpErlangDecodeException e) {
				// getMsg---get request args error
				states[0] = ERROR;
				states[1] = "request arguments error: " + e.getMessage();
				list = ErlangUtils.erlangListAdd(list, ErlangUtils.createTuple(new OtpErlangAtom(states[0]), new OtpErlangString(states[1])));
*/			} catch (ClassNotFoundException e) {
				// monitor_type can not be loaded.
				states[0] = ERROR;
				states[1] = "monitor type can not be loaded. " + e.getMessage();
				e.printStackTrace();
				//list = ErlangUtils.erlangListAdd(list, ErlangUtils.createTuple(new OtpErlangAtom(states[0]), new OtpErlangString(states[1])));
			} catch (InstantiationException e) {
				states[0] = ERROR;
				states[1] = "instantiate the monitor update object error. " + e.getMessage();
				e.printStackTrace();
				//list = ErlangUtils.erlangListAdd(list, ErlangUtils.createTuple(new OtpErlangAtom(states[0]), new OtpErlangString(states[1])));
			} catch (Exception e) {
				// other eroors
				states[0] = ERROR;
				states[1] = UNKOWN_REASONS + e.getMessage();
				e.printStackTrace();
				//list = ErlangUtils.erlangListAdd(list, ErlangUtils.createTuple(new OtpErlangAtom(states[0]), new OtpErlangString(states[1])));
/*			} finally {
				response[0] = new OtpErlangAtom(states[0]);
				response[1] = serverMbox.self();				
				response[2] = list;				
				System.out.println("--------sending to erlang node------------");
				System.out.println("list after add: -------" + list);
				OtpErlangTuple message = new OtpErlangTuple(response);
				System.out.println("---------------message: \n" + message);
				serverMbox.send(from, message);
*/
			}

		}

	}

}
