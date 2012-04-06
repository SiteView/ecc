package com.dragonflow.erlangecc.monitor;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeMap;

import javax.management.Attribute;
import javax.management.AttributeList;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.TabularData;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import javax.naming.NamingException;

import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class JMXMonitor extends BaseMonitor {

	String server = "192.168.6.195";
	String port = "6888";
	String applicationType = "apusic";
	String userName = "admin";
	String password = "admin";
	StringBuffer lastError = new StringBuffer();
	private JMXConnector mConnection;
	OtpErlangList EMPTY_ERLANG_LIST_STRING = new OtpErlangList();
	Map<String, Object> Counters = new HashMap<String, Object>();

	public JMXMonitor() {
		this.mConnection = null;
	}

	public void initArgs(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("server");
			if (s != null && s.length() > 0) {
				this.server = s.trim();
			}
			s = (String) map.get("port");
			if (s != null && s.length() > 0) {
				this.port = s.trim();
			}
			s = (String) map.get("applicationType");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.applicationType = s.trim();
			}
			s = (String) map.get("userName");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.userName = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.password = s.trim();
			}

		} catch (Exception e) {
			lastError.append(e.getMessage());
			e.printStackTrace();
		}

	}

	public void initArgs1(Map<String, Object> map) {
		String s = null;
		try {
			s = (String) map.get("server");
			if (s != null && s.length() > 0) {
				this.server = s.trim();
			}
			s = (String) map.get("port");
			if (s != null && s.length() > 0) {
				this.port = s.trim();
			}
			s = (String) map.get("applicationType");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.applicationType = s.trim();
			}
			s = (String) map.get("userName");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.userName = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.password = s.trim();
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

		} catch (Exception e) {
			lastError.append(e.getMessage());
			e.printStackTrace();
		}

	}

	private MBeanServerConnection getMBeanServerConnection()
			throws IOException, NamingException {
		MBeanServerConnection ret = null;
		if (this.mConnection != null) {
			try {
				ret = this.mConnection.getMBeanServerConnection();
				if (ret != null)
					return ret;
			} catch (IOException e) {
				this.lastError.append(e.getMessage());
			}

		}

		String jmxUrl = "";
		if (this.applicationType.equals("tomcat")) {
			jmxUrl = "service:jmx:rmi:///jndi/rmi://" + this.server + ":"
					+ this.port + "/jmxrmi";
		} else if (this.applicationType.equals("apusic")) {
			jmxUrl = "service:jmx:iiop:///jndi/corbaname::1.2@" + this.server
					+ ":" + this.port + "#jmx/rmi/RMIConnectorServer";
		} else {

		}
		// if (jmxUrl.startsWith("jnp://")) {
		// return getJnpMBeanServerConnection();
		// }
		Map connectionEnv = null;

		if ((this.userName != null) && (!(this.userName.equals("")))) {
			connectionEnv = new HashMap();
			String[] creds = { this.userName, this.password };
			connectionEnv.put("jmx.remote.credentials", creds);
		}

		JMXServiceURL url = new JMXServiceURL(jmxUrl);
		this.mConnection = JMXConnectorFactory.connect(url, connectionEnv);
		return this.mConnection.getMBeanServerConnection();
	}

	public Map<String, Object> getBrowseData() {
		Map<String, Object> result = new HashMap<String, Object>();
		MBeanServerConnection mbsc;
		this.initArgs(this.getMessage().getParams());
		try {
			mbsc = getMBeanServerConnection();
			Set MBeanset = mbsc.queryMBeans(null, null);
			Iterator MBeansetIterator = MBeanset.iterator();
			result = this.buildTreemap();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			this.lastError.append(e.getMessage());
			e.printStackTrace();
		} catch (NamingException e) {
			// TODO Auto-generated catch block
			this.lastError.append(e.getMessage());
			e.printStackTrace();
		}

		return result;
	}

	public Map<String, Object> update() {
		Map<String, Object> result = new HashMap<String, Object>();
		int errorCount = 0;
		MBeanServerConnection mbsc = null;
		this.initArgs1(this.getMessage().getParams());
		int len = this.Counters.size();
		List counterList = new ArrayList();
		Map attributesToQuery = new HashMap();
		Map hm = new HashMap();
		String onameStr;
		String counterStr;
		for (String s : this.Counters.keySet()) {
			String compent[] = s.split("\\$\\$");
			if (compent.length > 1) {
				hm.put("oname", compent[0]);
				hm.put("attr", compent[1]);
			}
			if (compent.length == 3) {
				hm.put("composite", compent[2]);
			}
			if (compent.length == 4) {
				hm.put("tab", compent[2]);
				hm.put("composite", compent[3]);
			}
			onameStr = (String) hm.get("oname");
			counterStr = (String) hm.get("attr");
			counterList.add(hm);
			List ll = (List) attributesToQuery.get(onameStr);
			if (ll == null) {
				ll = new LinkedList();
				attributesToQuery.put(onameStr, ll);
			}
			ll.add(counterStr);
		}
		Map dataMap = new HashMap();
		try {
			mbsc = getMBeanServerConnection();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			this.lastError.append(e.getMessage());
			e.printStackTrace();
		} catch (NamingException e) {
			// TODO Auto-generated catch block
			this.lastError.append(e.getMessage());
			e.printStackTrace();
		}
		if (mbsc == null) {
			result.put("stateString", this.lastError.toString()
					+ "get counters fail!");
			result.put("countersInError", len);
		} else {
			Set<Map.Entry<String, String>> set = attributesToQuery.entrySet();
			for (Map.Entry me : set) {
				try {
					onameStr = (String) me.getKey();
					String[] attrArr = new String[((List) me.getValue()).size()];
					((List) me.getValue()).toArray(attrArr);

					ObjectName objectName = new ObjectName(onameStr);
					AttributeList alist;
					try {
						alist = mbsc.getAttributes(objectName, attrArr);
					} catch (Exception e) {
						mbsc.getMBeanInfo(objectName);
						alist = mbsc.getAttributes(objectName, attrArr);
					}

					for (int i = 0; i < alist.size(); ++i) {
						Attribute attr = (Attribute) alist.get(i);
						String attrStr = attr.getName();
						dataMap
								.put(onameStr + " - " + attrStr, attr
										.getValue());
					}
				} catch (Exception e) {
				}

			}
			StringBuffer statestring = new StringBuffer();

			for (int i = 1; i <= counterList.size(); ++i) {
				String valStr = null;

				hm = (Map) counterList.get(i - 1);
				onameStr = (String) hm.get("oname");
				counterStr = (String) hm.get("attr");
				String compositeKey = (String) hm.get("composite");
				String tabKey = (String) hm.get("tab");
				try {
					Object val = dataMap.get(onameStr + " - " + counterStr);

					if (CompositeData.class.isInstance(val)) {
						Object o = ((CompositeData) val).get(compositeKey);
						if (o != null) {
							valStr = o.toString();
						}

					} else if (TabularData.class.isInstance(val)) {
						String[] keys = tabKey.split(",");
						CompositeData cd = ((TabularData) val).get(keys);
						if (cd != null) {
							Object o = cd.get(compositeKey);
							if (o != null)
								valStr = o.toString();
						}
					} else if ((val != null) && (val.getClass().isArray())) {
						Object[] arr = (Object[]) (Object[]) val;
						valStr = "";
						for (int k = 0; k < arr.length; ++k) {
							if (k > 0) {
								valStr = valStr + "; ";
							}
							valStr = valStr + arr[k].toString();
						}
					} else if (val != null) {
						valStr = val.toString();
					}
				} catch (Exception e) {
					++errorCount;
					valStr = "n/a";
				}

				if (valStr == null) {
					++errorCount;
					valStr = "n/a";
				}

				if (valStr.equals("")) {
					valStr = "(empty)";
				}
				String k1 = onameStr + "$$" + counterStr;
				if (tabKey != null && !tabKey.isEmpty()) {
					k1 = k1 + "$$" + tabKey;
				}
				if (compositeKey != null && !compositeKey.isEmpty()) {
					k1 = k1 + "$$" + compositeKey;
				}
				String s = (String) this.Counters.get(k1);
				try {
					int V1 = Integer.parseInt(valStr);
					result.put(s, V1);
				} catch (Exception ex) {
					result.put(s, valStr);
				}
				if (i > 1) {
					statestring.append(", ");
				}
				statestring.append(s + "=" + valStr);
			}
			result.put("stateString", statestring.toString());
			result.put("countersInError", errorCount);
		}
		return result;
	}

	public Map<String, Object> buildTreemap() {
		Map<String, Object> treemap = new HashMap<String, Object>();
		MBeanServerConnection mbsc = null;
		MBeanTreeModel beanModel = new MBeanTreeModel();
		try {
			mbsc = this.getMBeanServerConnection();
			if (mbsc != null) {
				Set MBeanset = mbsc.queryMBeans(null, null);
				Iterator MBeansetIterator;
				// while (MBeansetIterator.hasNext()) {
				// ObjectName objectName = (ObjectName) MBeansetIterator
				// .next();
				// beanModel.addMBean(objectName, mbsc
				// .getMBeanInfo(objectName));
				// }
				for (MBeansetIterator = MBeanset.iterator(); MBeansetIterator
						.hasNext();)
					try {
						ObjectName objectName = ((ObjectInstance) MBeansetIterator
								.next()).getObjectName();
						beanModel.addMBean(objectName, mbsc
								.getMBeanInfo(objectName));
					} catch (Exception e) {
						System.out.println("1" + e.getMessage());
					} catch (NoClassDefFoundError e) {
						System.out.println("2" + e.getMessage());
					}
				treemap.put("111111111111111", "browse_data");
				for (String domain : beanModel.getDomains()) {
					JMXMonitor.MBeanTreeModel.MBeanNode node = beanModel
							.getDomain(domain);
					convertMBeanNode(node, mbsc, "browse_data/", treemap);
				}
			}

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NamingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return treemap;
	}

	void convertMBeanNode(JMXMonitor.MBeanTreeModel.MBeanNode node,
			MBeanServerConnection mbsc, String parentname,
			Map<String, Object> treemap) {
		String name = node.getName().replace("/", "-");
		String key1 = name;
		String value1 = parentname + name;
		if (!treemap.containsKey(key1)) {
			treemap.put(key1, value1);
		}

		if (node.isLeaf()) {
			MBeanInfo beanInfo = node.getMBeanInfo();
			ObjectName objectName = node.getObjectName();
			createItems(beanInfo, objectName, mbsc, parentname + name + "/",
					treemap);
		} else {
			for (JMXMonitor.MBeanTreeModel.MBeanNode child : node.getChildren())
				convertMBeanNode(child, mbsc, parentname + name + "/", treemap);
		}
	}

	// 1 sid=oname.getCanonicalName()+m.put("attr", inf.getName())+composite
	// 2oname+attr+tab+composite
	// 3oname+attr
	void createItems(MBeanInfo mbi, ObjectName oname,
			MBeanServerConnection mbsc, String parentname,
			Map<String, Object> treemap) {
		MBeanAttributeInfo[] attrs = mbi.getAttributes();
		String key1 = "";
		String value1 = "";
		String tempid = "";
		for (MBeanAttributeInfo inf : attrs) {
			if (!(inf.isReadable())) {
				continue;
			}
			try {
				String attrType = inf.getType();
				Class attrClass = getClassFor(attrType);
				if (attrClass != null) {
					String description = (inf.getDescription() == null) ? ""
							: inf.getDescription();

					if (CompositeData.class.isAssignableFrom(attrClass)) {
						CompositeData comp = (CompositeData) mbsc.getAttribute(
								oname, inf.getName());

						if (comp != null) {
							CompositeType ct = comp.getCompositeType();
							Set keySet = ct.keySet();
							String name = inf.getName().replace("/", "-");
							key1 = name;
							value1 = parentname + name;
							if (!treemap.containsKey(key1)) {
								treemap.put(key1, value1);
							}
							// Map m = new HashMap();
							// m.put("oname", oname.getCanonicalName());
							// m.put("attr", inf.getName());
							tempid = oname.getCanonicalName() + "$$"
									+ inf.getName();
							Iterator ki = keySet.iterator();
							while (ki.hasNext()) {
								String k = (String) ki.next();
								String sid = tempid + "$$" + k;
								if (!treemap.containsKey(sid)) {
									treemap.put(sid, value1 + "/" + k);
								}
							}
						}
					} else if (TabularData.class.isAssignableFrom(attrClass)) {
						TabularData td = (TabularData) mbsc.getAttribute(oname,
								inf.getName());

						if (td != null) {
							// Element tabNode =
							// this.doc.createElement("object");
							// tabNode.setAttribute("name", inf.getName());
							// newNode.appendChild(tabNode);
							String name = inf.getName().replace("/", "-");
							key1 = name;
							value1 = parentname + name;
							if (!treemap.containsKey(key1)) {
								treemap.put(key1, value1);
							}
							Set s = td.keySet();
							// Map m = new HashMap();
							// m.put("oname", oname.getCanonicalName());
							// m.put("attr", inf.getName());
							// tempid=oname.getCanonicalName()+inf.getName();
							Iterator it = s.iterator();
							while (it.hasNext()) {
								List ll = (List) it.next();
								StringBuffer buf = new StringBuffer();
								for (Iterator i$ = ll.iterator(); i$.hasNext();) {
									Object v = i$.next();
									if (buf.length() > 0)
										buf.append(",");
									buf.append(v);
								}
								tempid = oname.getCanonicalName() + "$$"
										+ inf.getName() + "$$" + buf.toString();
								// m.put("tab", buf.toString());
								CompositeData comp = td.get(ll.toArray());

								if (comp != null) {
									CompositeType ct = comp.getCompositeType();
									key1 = buf.toString();
									value1 = parentname + name + "/"
											+ buf.toString();
									if (!treemap.containsKey(key1)) {
										treemap.put(key1, value1);
									}
									// Element compNode = this.doc
									// .createElement("object");
									//
									// compNode.setAttribute("name", buf
									// .toString());
									//
									// tabNode.appendChild(compNode);
									Set keySet = ct.keySet();
									Iterator ki = keySet.iterator();
									while (ki.hasNext()) {
										String k = (String) ki.next();
										String sid = tempid + "$$" + k;
										value1 = parentname + name + "/"
												+ buf.toString() + "/" + k;
										if (!treemap.containsKey(sid)) {
											treemap.put(sid, value1);
										}
										// m.put("composite", k);
										// Element counterNode = this.doc
										// .createElement("counter");
										//
										// counterNode.setAttribute("name", k);
										// counterNode.setAttribute("desc",
										// description);
										//
										// counterNode.setAttribute("id",
										// TextUtilsBase.mapToString(m));
										//
										// compNode.appendChild(counterNode);
									}
								}
							}
						}
					} else {
						// Map m;
						// Element counterNode;
						if (attrClass.isArray()) {
							// m = new HashMap();
							// m.put("oname", oname.getCanonicalName());
							// m.put("attr", inf.getName());
							// counterNode = this.doc.createElement("counter");
							// counterNode.setAttribute("name", inf.getName());
							// counterNode.setAttribute("desc", description);
							// counterNode.setAttribute("id", TextUtilsBase
							// .mapToString(m));
							//
							// newNode.appendChild(counterNode);
							tempid = oname.getCanonicalName() + "$$"
									+ inf.getName();
							value1 = parentname + inf.getName();
							if (!treemap.containsKey(tempid)) {
								treemap.put(tempid, value1);
							}
						} else {
							tempid = oname.getCanonicalName() + "$$"
									+ inf.getName();
							value1 = parentname + inf.getName();
							if (!treemap.containsKey(tempid)) {
								treemap.put(tempid, value1);
							}
							// m = new HashMap();
							// m.put("oname", oname.getCanonicalName());
							// m.put("attr", inf.getName());
							// counterNode = this.doc.createElement("counter");
							// counterNode.setAttribute("name", inf.getName());
							// counterNode.setAttribute("desc", description);
							// counterNode.setAttribute("id", TextUtilsBase
							// .mapToString(m));
							//
							// newNode.appendChild(counterNode);
						}
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
				System.out.println(e.getMessage());
			}

		}
	}

	private static Map<String, Class<?>> classLookups = new HashMap();
	static {
		classLookups.put("boolean", Boolean.class);
		classLookups.put("int", Integer.class);
		classLookups.put("long", Long.class);
		classLookups.put("short", Short.class);
		classLookups.put("float", Float.class);
		classLookups.put("double", Double.class);
		classLookups.put("byte", Byte.class);
		classLookups.put("char", Character.class);
	}

	private static Class<?> getClassFor(String className) {
		try {
			synchronized (classLookups) {
				Class c = (Class) classLookups.get(className);
				if (c == null) {
					c = Class.forName(className);
					classLookups.put(className, c);
				}
				return c;
			}
		} catch (Exception e) {
		}
		return null;
	}

	public static class MBeanTreeModel {
		private TreeMap<String, MBeanNode> data;
		private static final List<String> orderedKeyPropertyList = new ArrayList();
		private static final String PATH_PREFIX_STRING = "|||";

		public MBeanTreeModel() {
			this.data = new TreeMap();
		}

		public void addMBean(ObjectName oname, MBeanInfo mbInfo) {
			if ((oname == null) || (mbInfo == null)) {
				return;
			}
			String domain = oname.getDomain().trim();
			if (!(this.data.containsKey(domain))) {
				MBeanNode node = new MBeanNode(domain, false);
				this.data.put(domain, node);
			}
			String path = createPath(getKeyPropertyListString(oname));

			((MBeanNode) this.data.get(domain)).addChild(oname, mbInfo, path);
		}

		public void clear() {
			this.data.clear();
		}

		public MBeanNode getDomain(String domainName) {
			return ((MBeanNode) this.data.get(domainName));
		}

		public Collection<String> getDomains() {
			return this.data.keySet();
		}

		public boolean isEmpty() {
			return (this.data.size() == 0);
		}

		private String createPath(String keyPropertyListString) {
			String[] keyArray = keyPropertyListString.split(",");
			StringBuffer pathBuffer = new StringBuffer();
			for (int index = 0; index < keyArray.length; ++index) {
				String[] split = keyArray[index].split("=");
				pathBuffer.append(split[1].trim());
				if (index + 1 < keyArray.length) {
					pathBuffer.append("|||");
				}
			}
			return pathBuffer.toString();
		}

		private Map<String, String> extractKeyValuePairs(String properties,
				ObjectName mbean) {
			String props = properties;
			Map map = new LinkedHashMap();
			int eq = props.indexOf("=");
			while (eq != -1) {
				String key = props.substring(0, eq);
				String value = mbean.getKeyProperty(key);
				map.put(key, value);
				props = props.substring(key.length() + 1 + value.length());
				if (props.startsWith(",")) {
					props = props.substring(1);
				}
				eq = props.indexOf("=");
			}
			return map;
		}

		private String getKeyPropertyListString(ObjectName mbean) {
			String props = mbean.getKeyPropertyListString();
			Map map = extractKeyValuePairs(props, mbean);
			StringBuilder sb = new StringBuilder();

			for (String key : orderedKeyPropertyList) {
				if (map.containsKey(key)) {
					sb.append(key + "=" + ((String) map.get(key)) + ",");
					map.remove(key);
				}
			}
			Set<Map.Entry<String, String>> set = map.entrySet();
			for (Map.Entry<String, String> entry : set) {
				sb.append(((String) entry.getKey()) + "="
						+ ((String) entry.getValue()) + ",");
			}
			String orderedKeyPropertyListString = sb.toString();
			orderedKeyPropertyListString = orderedKeyPropertyListString
					.substring(0, orderedKeyPropertyListString.length() - 1);

			return orderedKeyPropertyListString;
		}

		static {
			String keyPropertyList = System
					.getProperty("com.sun.tools.jconsole.mbeans.keyPropertyList");

			if (keyPropertyList == null) {
				orderedKeyPropertyList.add("type");
				orderedKeyPropertyList.add("j2eeType");
			} else {
				StringTokenizer st = new StringTokenizer(keyPropertyList, ",");
				while (st.hasMoreTokens())
					orderedKeyPropertyList.add(st.nextToken());
			}
		}

		public static class MBeanNode {
			private TreeMap<String, MBeanNode> children = new TreeMap();
			private boolean leaf;
			private MBeanInfo mBeanInfo;
			private String name;
			private ObjectName objectName;

			public MBeanNode() {
			}

			public MBeanNode(String name, boolean leaf) {
				this.name = name;
				this.leaf = leaf;
			}

			public void addChild(ObjectName oname, MBeanInfo mbInfo, String path) {
				int indexOf = path.indexOf("|||");
				if (indexOf == -1) {
					MBeanNode beanNode = new MBeanNode(path, true);
					beanNode.setObjectName(oname);
					beanNode.setMBeanInfo(mbInfo);
					this.children.put(path, beanNode);
				} else {
					String chName = path.substring(0, indexOf);
					String subPath = path.substring(indexOf + "|||".length());

					if (!(this.children.containsKey(chName))) {
						this.children.put(chName, new MBeanNode(chName, false));
					}
					((MBeanNode) this.children.get(chName)).addChild(oname,
							mbInfo, subPath);
				}
			}

			public void clear() {
				this.children.clear();
			}

			public Collection<MBeanNode> getChildren() {
				return this.children.values();
			}

			public MBeanInfo getMBeanInfo() {
				return this.mBeanInfo;
			}

			public String getName() {
				return this.name;
			}

			public ObjectName getObjectName() {
				return this.objectName;
			}

			public boolean isLeaf() {
				return this.leaf;
			}

			public void setLeaf(boolean leaf) {
				this.leaf = leaf;
			}

			public void setMBeanInfo(MBeanInfo beanInfo) {
				this.mBeanInfo = beanInfo;
			}

			public void setName(String name) {
				this.name = name;
			}

			public void setObjectName(ObjectName objectName) {
				this.objectName = objectName;
			}
		}
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

//	public static void main(String[] args) {
//		JMXMonitor jmxm = new JMXMonitor();
//		try {
//			MBeanServerConnection mbsc = jmxm.getMBeanServerConnection();
//			Set MBeanset = mbsc.queryMBeans(null, null);
//			Iterator MBeansetIterator = MBeanset.iterator();
//			jmxm.buildTreemap();
//			// while (MBeansetIterator.hasNext()) {
//			// ObjectInstance objectInstance = (ObjectInstance) MBeansetIterator
//			// .next();
//			// ObjectName objectName = objectInstance.getObjectName();
//			// System.out.println("objectName : " + objectName);
//			// String canonicalName = objectName.getCanonicalName();
//			// // System.out.println("canonicalName : " + canonicalName);
//			// }
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} catch (NamingException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//	}

}
