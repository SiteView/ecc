package com.dragonflow.erlangecc.monitor;

import java.io.CharArrayWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import COM.datachannel.xml.om.Document;

import com.dragonflow.erlangecc.websphereservlet.FileUtils;
import com.dragonflow.erlangecc.websphereservlet.XSLUtils;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class Oracle10gMonitor extends BaseMonitor  {

	OtpErlangList EMPTY_ERLANG_LIST_STRING = new OtpErlangList();
	// List<String> Counters = new ArrayList<String>();
	Map<String, Object> Counters = new HashMap<String, Object>();
	String lastError = "";
	static String metricListURL = ServicePlatform.getRoot()
			+ "/templates.applications/oracle10g.xsl";
	static long xslTime = 0L;
	String xml = "";

	long networkTime;

	protected static boolean transform(String xml, String xslFilename,
			StringBuffer result) {
		long l = System.currentTimeMillis();

		try {
			if (xslFilename == null) {
				result.append(xml);
				return true;
			}
			CharArrayWriter writer = new CharArrayWriter();
			StringBuffer xslBuffer = FileUtils.readFile(xslFilename);
			XSLUtils
					.convert(xml, xslBuffer.toString(), new PrintWriter(writer));
			result.append(writer.toCharArray());
			return true;
		} catch (IOException e) {
			e.printStackTrace();
			result.append("Failed to read XSL file: " + xslFilename);
			return false;
		} catch (Exception e) {
			e.printStackTrace();
			result.append("Exception: " + e);
			return false;
		} finally {
			l = System.currentTimeMillis() - l;
			System.out.println("Transform: " + l);
			xslTime += l;
		}
	}

	public void buildTreemap(Map<String, Object> res_map, Element node,
			String parentName, int pid) {
		if (node == null || !node.hasChildNodes()) {
			return;
		}
		NodeList tnodelist = node.getChildNodes();
		try {
			for (int j = 0; j < tnodelist.getLength(); j++) {
				Element tempNode = (Element) tnodelist.item(j);
				// String type = tempNode.getTagName().toLowerCase();
				String name = tempNode.getAttribute("name").replace("/", "-");
				String id = System.currentTimeMillis() + "."
						+ Integer.toString(j) + Math.random();
				if (parentName == "") {
					res_map.put(id, name);
				} else {
					if (!res_map.containsKey(id)) {
						res_map.put(id, parentName + name);
					} else
						System.out.println(id);
				}
				// 特殊处理

				buildTreemap(res_map, tempNode, parentName + name + "/", pid);

			}
		} catch (Exception r) {
			System.out.println(r);
		}

	}

	public void initArgs(Map<String, Object> map) {
		try {
			OtpErlangList lists = (OtpErlangList) map.get("xml");
			OtpErlangObject[] objs = lists.elements();

			byte[] buffer = new byte[objs.length];
			int i = 0;
			for (OtpErlangObject obj : objs) {
				if (obj instanceof OtpErlangLong) {
					OtpErlangLong temlong = (OtpErlangLong) obj;
					buffer[i] = (byte) temlong.longValue();
					++i;
				}
			}

			String sxml = new String(buffer, "utf-8");
			this.xml = sxml;

		} catch (Exception e) {
			try {
				String s = (String) map.get("xml");
				if (s != null && s.length() > 0
						&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
					this.xml =s;
					System.out.println(this.xml);
				}
			} catch (Exception e1) {
				lastError = e1.getMessage();
				e1.printStackTrace();

			}

		}

	}

	public void initArgs1(Map<String, Object> map) {
		this.initArgs(map);
		try {

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

	public Map<String, Object> getBrowseData() {
		Map<String, Object> res_map = new HashMap<String, Object>();
		Document document = new Document();
		this.initArgs(this.getMessage().getParams());
		StringBuffer result = new StringBuffer();
		boolean bOK = transform(this.xml, this.metricListURL, result);
		if (bOK) {
			document.loadXML(result.toString());
			Element root = (Element) document.getFirstChild();
			buildTreemap(res_map, root, "", 1);
		}

		return res_map;
	}
	public void getmapbyxml(Element node,HashMap<String, String> result,String parentName)
	{

		if (node == null || !node.hasChildNodes()) {
			return;
		}
		NodeList tnodelist = node.getChildNodes();
		try {
			for (int j = 0; j < tnodelist.getLength(); j++) {
				Element tempNode = (Element) tnodelist.item(j);
				String type = tempNode.getTagName().toLowerCase();
				String name;
				if(type.equals("counter"))
				{
				name = tempNode.getAttribute("name").replace("/", "-");
				String value=tempNode.getAttribute("value");
				result.put(parentName + name, value);
				}else
				{
					 name = tempNode.getAttribute("name").replace("/", "-");
				}
				// 特殊处理d
				getmapbyxml( tempNode, result, parentName + name + "/");

			}
		} catch (Exception r) {
			System.out.println(r);
		}

	
	}
	public HashMap<String, String> getUpdate(String xml)
	{
		HashMap<String, String> result=new HashMap<String, String>();
		Document document = new Document();
		document.loadXML(xml);
		Element root = (Element) document.getFirstChild();
		getmapbyxml(root,result,"");
		return result;
		
	}

	public Map<String, Object> update() {
		Map<String, Object> result = new HashMap<String, Object>();
		this.initArgs1(this.getMessage().getParams());
		StringBuffer result1 = new StringBuffer();
		Document document = new Document();
		Map<String, Object> res_map = new HashMap<String, Object>();
		int len = this.Counters.size();
		boolean bOK = transform(this.xml, this.metricListURL, result1);
		if (bOK) {
		
			int errorCount = 0;
			StringBuffer statestring = new StringBuffer();
			int j = 0;
			HashMap<String, String> cmap = getUpdate(result1.toString());
			for (String s : this.Counters.keySet()) {
				String s1 = this.Counters.get(s).toString();// .replace("-",
				// "/");
				String value = "";
				if (cmap.containsKey(s1)) {
					String v;
					try {
						v = new String(cmap.get(s1).getBytes("GBK"), "UTF-8").split("]")[0];
					} catch (UnsupportedEncodingException e) {
						// TODO Auto-generated catch block
						v=s1;
					}
					value = v;
					try {
						int V1 = Integer.parseInt(v);
						result.put(s1, V1);
					} catch (Exception ex) {
						result.put(s1, v);
					}

				} else {
					++errorCount;
					value = "n/a";
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
			result.put("pStatus", "ok");
			if(errorCount>0)
			{
				result.put("pStatus", "error");
			}

		} else {
			this.lastError = result1.toString();
			result.put("stateString", "update fail");
			result.put("countersInError", len);
			result.put("pStatus", "error");
		}

		// if (logger.isDebugEnabled())
		// logger.debug("Entering WebSphereMonitor.update().");
		// int len = getMaxCounters();
		//	 
		// if (len == 0) {
		// if (logger.isDebugEnabled())
		// logger.debug("Leaving WebSphereMonitor.update() because there were no counters to retrieve.");
		// return true;
		// }

		return result;
	}

	@Override
	public int handleMessage() {
		String action = this.getMessage().getAction();
		String error = null;

		Map<String, Object> resp = null;

		if (action != null && action.equals("update")) {
			resp = this.update();
			error = this.lastError.toString();
			ArrayList<String> R = new ArrayList<String>();
			if ((lastError).length() > 0) {
				R.add(lastError);
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
