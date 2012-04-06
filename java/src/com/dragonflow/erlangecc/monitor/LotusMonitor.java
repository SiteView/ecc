package com.dragonflow.erlangecc.monitor;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import lotus.domino.*;

public class LotusMonitor extends BaseMonitor {

	String dominoURL = "192.168.6.197"; // DOMINO服务器的IP地址
	String username = "li li/siteview"; // 用户ID
	String password = "siteview"; // 用户的internet口令
	String iorpath = "F:\\diiop_ior.txt";
	StringBuffer error = new StringBuffer();
	OtpErlangList EMPTY_ERLANG_LIST_STRING = new OtpErlangList();
	Map<String, Object> Counters = new HashMap<String, Object>();
	HashMap<String, String> namevalues = new HashMap<String, String>();

	public void initArgs(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("dominoURL");
			if (s != null && s.length() > 0) {
				this.dominoURL = s.trim();
			}
			s = (String) map.get("username");
			if (s != null && s.length() > 0) {
				this.username = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.password = s.trim();
			}
			s = (String) map.get("iorpath");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.iorpath = s.trim();
			}

		} catch (Exception e) {
			error.append(e.getMessage());
			e.printStackTrace();
		}

	}

	public void initArgs1(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("dominoURL");
			if (s != null && s.length() > 0) {
				this.dominoURL = s.trim();
			}
			s = (String) map.get("username");
			if (s != null && s.length() > 0) {
				this.username = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.password = s.trim();
			}
			s = (String) map.get("iorpath");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.iorpath = s.trim();
			}
			Object o = map.get("counters");
			if (o != null && (o instanceof OtpErlangList)) {
				OtpErlangList counters = (OtpErlangList) o;
				if (counters.arity() > 0) {
					OtpErlangObject[] objs = counters.elements();
					for (OtpErlangObject e : objs) {
						if (e instanceof OtpErlangTuple) {
							OtpErlangTuple t = (OtpErlangTuple) e;
							String id = ((OtpErlangString) t.elementAt(0))
									.stringValue();
							String value = ((OtpErlangString) t.elementAt(1))
									.stringValue();
							this.Counters.put(id, value);
						}
					}
				}
			} else {
				this.Counters.put("MALFORMED COUNTER ID", null);
			}
			// this.Counters = ErlangUtils.erlangList2arrayList(counters);

		} catch (Exception e) {
			error.append(e.getMessage());
			e.printStackTrace();
		}

	}

	/*
	 * 获取session 对象
	 */
	public Session getsession(String dominoURL, String username,
			String password, String iorpath) {
		Session session = null;
		String ior = null;
		try {
			// 如果已将ior文件拷到本地,则直接读ior文件
			try {
				FileInputStream fin = new FileInputStream(iorpath);
				InputStreamReader fisr = new InputStreamReader(fin);
				BufferedReader br = new BufferedReader(fisr);
				ior = br.readLine();
				fin.close();
			} catch (IOException e) {
				ior = null;
			}
			// 如果读不到diiop_ior.txt文件，则需要通过网络从服务器下载这个文件
			// 这需要domino服务器开放http服务
			if (ior == null)
				ior = NotesFactory.getIOR(dominoURL);// 下载IOR
			session = NotesFactory
					.createSessionWithIOR(ior, username, password);
		} catch (NotesException ex) {
			error.append(ex.getMessage());
			System.out.println(ex.getClass().getName() + ":" + ex.id);
		}
		return session;

	}

	/*
	 * 获取domino 的相关信息
	 */
	public String getMem(String dominoURL, String username, String password,
			String iorpath) {
		String result = "";
		Session session = this.getsession(dominoURL, username, password,
				iorpath);
		if (session != null) {
			try {
				result = session.sendConsoleCommand(this.dominoURL,
						"show stat mem");
			} catch (NotesException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

		return result;

	}

	/*
	 * 获取domino 的内存信息
	 */
	public String getDisk(String dominoURL, String username, String password,
			String iorpath) {
		String result = "";
		Session session = this.getsession(dominoURL, username, password,
				iorpath);
		if (session != null) {
			try {
				result = session.sendConsoleCommand(this.dominoURL,
						"show stat disk");
			} catch (NotesException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

		return result;

	}

	/*
	 * 获取domino 服务器信息
	 */
	public String getTask(String dominoURL, String username, String password,
			String iorpath) {
		String result = "";
		Session session = this.getsession(dominoURL, username, password,
				iorpath);
		if (session != null) {
			try {
				result = session.sendConsoleCommand(this.dominoURL,
						"show stat server");
			} catch (NotesException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

		return result;

	}

	/*
	 * 获取domino 邮件系统信息
	 */
	public String getMail(String dominoURL, String username, String password,
			String iorpath) {
		String result = "";
		Session session = this.getsession(dominoURL, username, password,
				iorpath);
		if (session != null) {
			try {
				result = session.sendConsoleCommand(this.dominoURL,
						"show stat mail");
			} catch (NotesException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

		return result;

	}

	/*
	 * 获取domino 数据库信息
	 */
	public String getDatabase(String dominoURL, String username,
			String password, String iorpath) {
		String result = "";
		Session session = this.getsession(dominoURL, username, password,
				iorpath);
		if (session != null) {
			try {
				result = session.sendConsoleCommand(this.dominoURL,
						"show stat database");
			} catch (NotesException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

		return result;

	}

	/*
	 * 得到domino的所有信息
	 */
	public Boolean getAllDomino(String dominoURL, String username,
			String password, String iorpath) {
		String result = "";
		Boolean R = false;
		Session session = this.getsession(dominoURL, username, password,
				iorpath);
		if (session != null) {
			try {
				this.namevalues.clear();
				result = session.sendConsoleCommand(this.dominoURL,
						"show stat");
				String[] fields = result.split("\r\n");
			
				for (String s : fields) {
					String[] keyvalue = s.split("=");
					if (keyvalue.length < 2)
						continue;
					String key = keyvalue[0].trim();
					String value = keyvalue[1].trim();
					this.namevalues.put(key, value);
				}
				R = true;
			} catch (NotesException e) {
				// TODO Auto-generated catch block
				error.append(e.getMessage());
				e.printStackTrace();
			}
		}

		return R;
	}

	public static void main(String[] args) {
//		Session session;
//		String ior;
//		String dominoURL = "192.168.6.197"; // DOMINO服务器的IP地址
//		String username = "li li/siteview"; // 用户ID
//		String password = "siteview"; // 用户的internet口令
//		try {
//			// 如果已将ior文件拷到本地,则直接读ior文件
//			try {
//				FileInputStream fin = new FileInputStream("F:\\diiop_ior.txt");
//				InputStreamReader fisr = new InputStreamReader(fin);
//				BufferedReader br = new BufferedReader(fisr);
//				ior = br.readLine();
//				fin.close();
//			} catch (IOException e) {
//				ior = null;
//			}
//			// 如果读不到diiop_ior.txt文件，则需要通过网络从服务器下载这个文件
//			// 这需要domino服务器开放http服务
//			if (ior == null)
//				ior = NotesFactory.getIOR(dominoURL);// 下载IOR
//			session = NotesFactory
//					.createSessionWithIOR(ior, username, password);
//			String result = session.sendConsoleCommand(dominoURL, "show stat");
//			String[] dd = result.split("\r\n");
//			String[] aa = "dddd.jj.kk".split("\\.");
//			System.out.println(result); // 如果这一行打印出来了,表示连接成功
//		} catch (NotesException ex) {
//			System.out.println(ex.getClass().getName() + ":" + ex.id);
//		}
		// try {
		// FileInputStream fin = new FileInputStream("F:\\diiop_ior.txt");
		// InputStreamReader fisr = new InputStreamReader(fin);
		// BufferedReader br = new BufferedReader(fisr);
		// ior = br.readLine();
		// fin.close();
		// Session s = NotesFactory.createSession(ior, "li li/siteview",
		// "siteview");
		// s.sendConsoleCommand("192.168.6.197", "show stat server");
		//
		// } catch (Exception e) {
		// e.printStackTrace();
		// }
	}

	public Map<String, Object> update() {
		Map<String, Object> result = new HashMap<String, Object>();
		this.initArgs1(this.getMessage().getParams());
		Boolean success=this.getAllDomino(dominoURL, username, password, iorpath);
		int len=this.Counters.size();
		for (String id : this.Counters.keySet()) {
			result.put(id, "n/a");
		}
		if (success)
		{
			int errorCount = 0;
			StringBuffer statestring = new StringBuffer();
			int j=0;
			for (String s : this.Counters.keySet()) {
				String name = this.Counters.get(s).toString();// .replace("-",
				// "/");
                String value="";
				if (namevalues.containsKey(s)) {
					String v = namevalues.get(s);
					value=v.replace(",", "");
					try {
						int V1 = Integer.parseInt(v);
						result.put(s, V1);
						result.put(name, V1);
					} catch (Exception ex) {
						result.put(s, v);
						result.put(name, v);
					}

				} else {
					++errorCount;
					value="n/a";
					result.put(s, "n/a");
					result.put(name, "n/a");
				}
				if (value.equals(""))
					statestring.append(name).append(" = \"\"");
				else {
					statestring.append(name).append(" = ").append(value);
				}

				if (j != len - 1)
					statestring.append(", ");
				++j;
			}
			result.put("stateString", statestring.toString());
			result.put("countersInError", errorCount);

		
		 	
		}else
		{
			result.put("countersInError", len);
			result.put("stateString", "connect to server fail!");
			this.error.append("connect to server fail!");
			return result;
		}
		
		
		return result;

	}

	public Map<String, Object> getBrowseData() {
		Map<String, Object> result = new HashMap<String, Object>();
		this.initArgs(this.getMessage().getParams());
		Boolean succ = this.getAllDomino(this.dominoURL, this.username,
				this.password, this.iorpath);
		if (succ) {
			int i;
			for (String name : this.namevalues.keySet()) {
				String[] ss = name.split("\\.");
				for (i = 0; i < ss.length; i++) {
					String v = "";
					String id = "";
					for (int j = 0; j <= i; j++) {
						if (j == 0) {
							id = ss[j];
							v = ss[j];
						} else {
							id = id + "." + ss[j];
							v = v + "/" + ss[j];
						}

					}
					if (id!=null&&id!=""&&!result.containsKey(id))
					{
						result.put(id, "Browsedata"+"/"+v);
					}
				}

			}

		}
		result.put("111111", "Browsedata");
		return result;

	}

	@Override
	public int handleMessage() {

		// TODO Auto-generated method stub
		String action = this.getMessage().getAction();
		String error = null;

		Map<String, Object> resp = null;

		if (action != null && action.equals("update")) {
			resp = this.update();
			error = this.error.toString();
			ArrayList<String> R = new ArrayList<String>();
			if (error.length() > 0) {
				R.add(error);
				this.sendResponse3("error", R);
			} else {
				System.out.println("update result: " + resp);
				this.sendResponse2("ok", resp);
			}

		} else if (action != null && action.equals("getBrowseData")) {
			resp = this.getBrowseData();
			error = this.error.toString();
			if (error != null && error.length() > 0) {
				resp = new HashMap<String, Object>();
				resp.put("error", ServicePlatform.replaceUnicodeChar(error));
				this.sendResponse("error", resp);
			} else {
				System.out.println("getBrowseData data result: \n" + resp);
				this.sendResponse2("ok", resp);
			}
		}
		return 0;

	}
}
