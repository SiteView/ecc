package com.dragonflow.erlangecc.monitor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import com.dragonflow.siteview.informix.*;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.dragonflow.siteview.websphere.util.WebSphereCounter;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import COM.datachannel.xml.om.Document; //import org.w3c.dom.Document;

public class InformixMonitor extends BaseMonitor {

	String ServerName;//
	String Host;
	String Port = "1526";
	String UserName;
	String Password;
	String DriverName = "com.informix.jdbc.IfxDriver";
	String Database = "sysmaster";
	OtpErlangList EMPTY_ERLANG_LIST_STRING = new OtpErlangList();
	String lastError = "";
	Map<String, Object> Counters = new HashMap<String, Object>();

	public void initArgs(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("dnamicserver");
			if (s != null && s.length() > 0) {
				this.ServerName = s.trim();
			}
			s = (String) map.get("host");
			if (s != null && s.length() > 0) {
				this.Host = s.trim();
			}
			s = (String) map.get("port");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Port = s.trim();
			}
			s = (String) map.get("username");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.UserName = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Password = s.trim();
			}

		} catch (Exception e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}

	}

	public void initArgs1(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("dnamicserver");
			if (s != null && s.length() > 0) {
				this.ServerName = s.trim();
			}
			s = (String) map.get("host");
			if (s != null && s.length() > 0) {
				this.Host = s.trim();
			}
			s = (String) map.get("port");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Port = s.trim();
			}
			s = (String) map.get("username");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.UserName = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Password = s.trim();
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
			lastError = e.getMessage();
			e.printStackTrace();
		}

	}

	public void buildTreemap(Map<String, Object> res_map, Element node,
			String parentName) {
		if (node == null || !node.hasChildNodes()) {
			return;
		}
		NodeList tnodelist = node.getChildNodes();
		try {
			for (int j = 0; j < tnodelist.getLength(); j++) {
				Element tempNode = (Element) tnodelist.item(j);
				String type = tempNode.getTagName().toLowerCase();
				String id = "";
				id = tempNode.getAttribute("id");
				String name = tempNode.getAttribute("name").replace("/", "-");
				if (parentName == "") {
					if (!id.isEmpty()) {
						if (!res_map.containsKey(id)) {
							if (type.equals("counter"))
								res_map.put(id, name);
							// res_map.put(name1, name);
							else
								res_map.put(id, name);
						}

					}
				} else {

					if (!id.isEmpty()) {
						if (!res_map.containsKey(id)) {
							if (type.equals("counter"))
								// res_map.put(parentName1+name1,parentName+name);
								res_map.put(id, parentName + name);
							else
								res_map.put(id, parentName + name);
						}
					}
				}
				// ÌØÊâ´¦Àí

				buildTreemap(res_map, tempNode, parentName + name + "/");

			}
		} catch (Exception r) {
			System.out.println(r);
		}

	}

	public Map<String, Object> getBrowseData() {
		Map<String, Object> res_map = new HashMap<String, Object>();
		Document document = new Document();
		
		this.initArgs(this.getMessage().getParams());
		String url = "jdbc:informix-sqli://" + this.Host + ":" + this.Port
		+ "/" + this.Database + ":INFORMIXSERVER=" + this.ServerName;
DbInformix db = new DbInformix(url, this.UserName, this.Password,
		this.DriverName);
		StringBuffer s = db.xmlBrowseData(this.ServerName);
		document.loadXML(s.toString());
		Element root = (Element) document.getFirstChild();
		buildTreemap(res_map, root, "");
		return res_map;
	}

	public Map<String, Object> update() {

		Map<String, Object> result = new HashMap<String, Object>();
		this.initArgs1(this.getMessage().getParams());
		String url = "jdbc:informix-sqli://" + this.Host + ":" + this.Port
				+ "/" + this.Database + ":INFORMIXSERVER=" + this.ServerName;
		DbInformix inf = new DbInformix(url, this.UserName, this.Password,
				this.DriverName);
		HashMap<String, String> cmap = inf.getUpdate();
		
		int len = this.Counters.size();
		if (cmap.containsKey("error")) {
			this.lastError = cmap.get("error");
			result.put("pStatus","error");
			result.put("stateString", "update fail");
			result.put("countersInError", len);
		} else {
			result.put("pStatus","ok");
			int errorCount = 0;
			StringBuffer statestring = new StringBuffer();
			int j=0;
			for (String s : this.Counters.keySet()) {
				String s1 = this.Counters.get(s).toString();// .replace("-",
				// "/");
                String value="";
				if (cmap.containsKey(s)) {
					String v = cmap.get(s);
					value=v;
					try {
						int V1 = Integer.parseInt(v);
						result.put(s1, V1);
						result.put(s, V1);
					} catch (Exception ex) {
						try
						{
						Float Vf=Float.parseFloat(v);
						result.put(s1, Vf);
						result.put(s, Vf);
						}catch(Exception ex1)
						{
						 if(v instanceof String)
						 {
							if(v.equals("false"))
							{
								result.put(s1, false);	
								result.put(s, false);	
							}
							else if(v.equals("true"))
							{ result.put(s1, true);
							result.put(s, true);}
							else
							{ result.put(s1, v);result.put(s, v);}
						 }else
						 {
						 result.put(s1, v);
						 result.put(s, v);
						 }
						}
					}

				} else {
					++errorCount;
					value="n/a";
					result.put(s1, "n/a");
				}
				if (value.equals(""))
					statestring.append(s1).append(" = \"\"");
				else {
					statestring.append(s1).append(" = ").append(value);
				}

				if (j != len - 1)
					statestring.append(", ");
				++j;
			}
			result.put("stateString", statestring.toString());
			result.put("countersInError", errorCount);

		}
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
			error = this.lastError.toString();
			ArrayList<String> R = new ArrayList<String>();
			if (lastError.length() > 0) {
				R.add(error);
				this.sendResponse3("error", R);
			} else {
				System.out.println("update result: " + resp);
				this.sendResponse2("ok", resp);
			}

		} else if (action != null && action.equals("getBrowseData")) {
			resp = this.getBrowseData();
			error = this.lastError.toString();
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
