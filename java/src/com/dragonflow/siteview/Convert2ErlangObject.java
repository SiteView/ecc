package com.dragonflow.siteview;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class Convert2ErlangObject {
	public static List<OtpErlangObject> convertMap2ErlangTupleList(Map<String, Object> map) {

		Set<String> set = map.keySet();
		// int size = set.size();
		List<OtpErlangObject> list = new ArrayList<OtpErlangObject>();

		Iterator<String> it = set.iterator();
		while (it.hasNext()) {
			String ss = it.next();
			// if (ss == null) {
			// ss = "";
			// }
			OtpErlangObject obj = new OtpErlangBinary(map.get(ss));
			OtpErlangString key = new OtpErlangString(ss);
			OtpErlangObject[] erlangObjs = new OtpErlangObject[2];
			erlangObjs[0] = key;
			erlangObjs[1] = obj;
			OtpErlangTuple tuple = new OtpErlangTuple(erlangObjs);

			list.add(tuple);

		}

		return list;

	}

}
