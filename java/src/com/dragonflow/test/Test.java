package com.dragonflow.test;

import java.io.UnsupportedEncodingException;
import org.apache.commons.lang.CharSet;
import com.ericsson.otp.erlang.OtpErlangRangeException;

public class Test {
	public static void main(String[] args) throws OtpErlangRangeException {
//		System.out.println((int)' ');
//		OtpErlangList list = new OtpErlangList("abcdd ddd");
//		OtpErlangString erstr = new OtpErlangString("aaabbbbbbb");
//		System.out.println(erstr);
//		System.out.println(erstr.stringValue());
//		System.out.println(list.toString());
////		OtpOutputStream oos = new OtpOutputStream();
////		list.encode(oos);
////		byte[] bytes = new byte[oos.size()];
////		oos.write(bytes);
////		System.out.println(new String(bytes));
//		OtpErlangObject[] ss = list.elements();
//		StringBuffer sb = new StringBuffer("");
//		for (OtpErlangObject s : ss) {
//			System.out.print(s + " ");
//			if(s instanceof OtpErlangChar){
//				OtpErlangChar ch = (OtpErlangChar)s;
//				System.out.println(s.getClass().getName());
//				char c = ch.charValue();
//				sb.append(c);
//				
//			}
//		}
//		System.out.println(sb);
		
//		OtpErlangTuple tuple = ErlangUtils.createTuple(new OtpErlangAtom("ok"), new OtpErlangLong(500));
//		System.out.println(tuple);
//		
//		System.out.println(ErlangUtils.createTuple(new OtpErlangAtom("state_string"), new OtpErlangLong(500)));
//		
		String s = "����jdjsdk";
		CharSet chset = CharSet.getInstance(s);
		
		
		try {
			System.out.println(new String(s.getBytes("UTF-8"), "utf-8"));
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
	}

}
