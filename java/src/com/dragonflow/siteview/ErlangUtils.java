package com.dragonflow.siteview;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangChar;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangFloat;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangBoolean;
public class ErlangUtils {
	static String EMPTY_ERLANG_STRING = (new OtpErlangList()).toString();

	public static OtpErlangList map2ErlangTupleList(Map<String, String> map) {
		// OtpErlangList list = null;
		ArrayList<OtpErlangTuple> array = new ArrayList<OtpErlangTuple>();
		Set<String> set = map.keySet();
		Iterator<String> it = set.iterator();

		while (it.hasNext()) {
			String key = it.next();
			String value = map.get(key);
			OtpErlangObject[] objs = new OtpErlangObject[2];
			objs[0] = new OtpErlangAtom(key);
			objs[1] = new OtpErlangString(value);

			array.add(new OtpErlangTuple(objs));
		}
		OtpErlangTuple[] tuples = new OtpErlangTuple[array.size()];
		array.toArray(tuples);
		return new OtpErlangList(tuples);
	}

	public static String erlangList2String(OtpErlangList list)
			throws OtpErlangRangeException {
		OtpErlangObject[] ss = list.elements();
		StringBuffer sb = new StringBuffer("");
		if (ss == null)
			return "";

		for (OtpErlangObject s : ss) {
			System.out.print(s + " ");
			if (s instanceof OtpErlangChar) {
				OtpErlangChar ch = (OtpErlangChar) s;
				sb.append(ch.charValue());
			}
		}
		System.out.println(sb);
		return sb.toString();
	}

	public static List<String> erlangList2arrayList(OtpErlangList list)
			throws OtpErlangRangeException {
		List<String> result = new ArrayList<String>();
		OtpErlangObject[] ss = list.elements();
		for (OtpErlangObject s : ss) {
			if (s instanceof OtpErlangString) {
				String aa = s.toString() == "[]" ? "" : ((OtpErlangString)s).stringValue();
				result.add(aa);
			}
		}
		return result;
	}
   public static OtpErlangList arrayList2ErlangList(ArrayList<String> list)
   {
	  ArrayList<OtpErlangObject> erlist=new ArrayList<OtpErlangObject>();
	   for(String s:list)
	   {   
		   OtpErlangObject aa=new OtpErlangList(s);
		   erlist.add(aa);
	   }
	   OtpErlangObject[] ret = new OtpErlangObject[erlist.size()];
	   erlist.toArray(ret);
	   return new OtpErlangList(ret);
	   
   }
	public static String getRequestMonitorType(OtpErlangObject request) {
		String monitorType = "";
		if (request instanceof OtpErlangTuple) {
			OtpErlangTuple tuple = (OtpErlangTuple) request;
			monitorType = tuple.elementAt(0).toString();
		}

		return monitorType;
	}

	public static OtpErlangList erlangListAdd(OtpErlangList erlangList,
			OtpErlangObject obj) {
		OtpErlangObject[] objs = erlangList.elements();
		ArrayList<OtpErlangObject> list = new ArrayList<OtpErlangObject>();
		for (OtpErlangObject e : objs) {
			list.add(e);
		}
		list.add(obj);
		OtpErlangObject[] ret = new OtpErlangObject[list.size()];
		list.toArray(ret);
		return new OtpErlangList(ret);

	}

	public static OtpErlangTuple createTuple(OtpErlangObject... erlangObjects) {
		OtpErlangObject[] objs = new OtpErlangObject[erlangObjects.length];
		for (int i = 0; i < objs.length; i++) {
			objs[i] = erlangObjects[i];
		}
		return new OtpErlangTuple(objs);

	}

	public static void main(String[] args) {
		OtpErlangAtom atom = new OtpErlangAtom("abc");
		System.out.println(atom);
	}

	public static Map<String, Object> erlangListToMap(OtpErlangList list) {
		Map<String, Object> map = new HashMap<String, Object>();

		OtpErlangObject[] objs = list.elements();
		for (OtpErlangObject obj : objs) {

			if (obj instanceof OtpErlangTuple) {
				OtpErlangTuple t = (OtpErlangTuple) obj;
				String key = t.elementAt(0).toString();
				Object e = t.elementAt(1);

				Object value = null;
				if (e instanceof OtpErlangString) {
					OtpErlangString erstr = (OtpErlangString) e;
					value = erstr.stringValue();
				} else if ((e instanceof OtpErlangAtom)
						|| (e instanceof OtpErlangLong)
						|| (e instanceof OtpErlangDouble)) {
					value = e.toString();
				} else if (e instanceof OtpErlangList) {
					OtpErlangList erList = (OtpErlangList) e;
					if (erList.toString().equals(EMPTY_ERLANG_STRING)) {
						value = "";
					} else {
						value = erList;
					}
				} else {
					value = e.toString();
				}
				map.put(key, value);
			}
		}
		return map;
	}

	public static OtpErlangList map2ErlangList(Map<String, Object> map) {

		ArrayList<OtpErlangTuple> array = new ArrayList<OtpErlangTuple>();
		Set<String> set = map.keySet();
		Iterator<String> it = set.iterator();

		while (it.hasNext()) {
			String key = it.next();
			Object value = map.get(key);
			OtpErlangObject[] objs = new OtpErlangObject[2];
			objs[0] = new OtpErlangAtom(key);
			if (value == null)
				continue;
			else if (value instanceof OtpErlangObject)
				objs[1] = (OtpErlangObject) value;
			else if (value instanceof String)
				objs[1] = new OtpErlangString((String) value);
			else if (value instanceof Float)
				objs[1] = new OtpErlangFloat(((Float) value).floatValue());
			else if (value instanceof Integer)
				objs[1] = new OtpErlangInt(((Integer) value).intValue());
			else if (value instanceof Long)
				objs[1] = new OtpErlangLong(((Long) value).longValue());
			else if (value instanceof Double)
				objs[1] = new OtpErlangDouble(((Double) value).doubleValue());
			else
				objs[1] = new OtpErlangString(value.toString());

			array.add(new OtpErlangTuple(objs));
		}
		OtpErlangTuple[] tuples = new OtpErlangTuple[array.size()];
		array.toArray(tuples);
		if (tuples.length == 0) {
			return new OtpErlangList();
		}
		return new OtpErlangList(tuples);
	}

	public static OtpErlangList map2ErlangList2(Map<String, Object> map) {

		ArrayList<OtpErlangTuple> array = new ArrayList<OtpErlangTuple>();
		Set<String> set = map.keySet();
		Iterator<String> it = set.iterator();

		while (it.hasNext()) {
			String key = it.next();
			Object value = map.get(key);
			OtpErlangObject[] objs = new OtpErlangObject[2];
			objs[0] = new OtpErlangString(key);
			if (value == null)
				continue;
			else if (value instanceof OtpErlangObject)
				objs[1] = (OtpErlangObject) value;
			else if (value instanceof String)
				objs[1] = new OtpErlangString((String) value);
			else if (value instanceof Float)
				objs[1] = new OtpErlangFloat(((Float) value).floatValue());
			else if (value instanceof Integer)
				objs[1] = new OtpErlangInt(((Integer) value).intValue());
			else if (value instanceof Long)
				objs[1] = new OtpErlangLong(((Long) value).longValue());
			else if (value instanceof Boolean)
				objs[1] = new OtpErlangBoolean(((Boolean) value).booleanValue());
			else if (value instanceof Double)
				objs[1] = new OtpErlangDouble(((Double) value).doubleValue());
			else
				objs[1] = new OtpErlangString(value.toString());

			array.add(new OtpErlangTuple(objs));
		}
		OtpErlangTuple[] tuples = new OtpErlangTuple[array.size()];
		array.toArray(tuples);
		if (tuples.length == 0) {
			return new OtpErlangList();
		}
		return new OtpErlangList(tuples);
	}
}
