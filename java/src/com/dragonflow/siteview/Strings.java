package com.dragonflow.siteview;

import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;

import org.apache.commons.lang.CharEncoding;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.SystemUtils;

import com.ericsson.otp.erlang.OtpErlangList;

public class Strings {
	public static final String CRLF = "\r\n";
	public static final String FS = SystemUtils.FILE_SEPARATOR;
	public static final String HTTP_LF = "<br>";
	public static final String EMPTY_ERLANG_LIST_STRING = (new OtpErlangList()).toString();
	
	
	

	public static String defaultCharsetToUTF8(String str) {
		if (str == null) {
			return null;
		}
		byte[] bytes = str.getBytes(Charset.defaultCharset());
		String utf = null;
		try {
			utf = new String(bytes, CharEncoding.UTF_8);
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		return utf;
	}

	public static String defaultCharsetToUTF8(StringBuffer buffer) {
		if (buffer == null) {
			return null;
		}
		return defaultCharsetToUTF8(buffer.toString());
	}

	public static String ToUTF8(String str, String nowCharset) {
		if (str == null) {
			return null;
		}
		String utf = null;
		try {
			byte[] bytes = str.getBytes(nowCharset);
			utf = new String(bytes, CharEncoding.UTF_8);
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}

		return utf;
	}

	public static String htmlEscape(String str) {
		if (str == null) {
			return null;
		}
		String escaped = StringEscapeUtils.escapeHtml(str);
		return escaped;
	}

	public static String htmlEscape(StringBuffer buffer) {
		if (buffer == null) {
			return null;
		}
		return htmlEscape(buffer.toString());
	}

	public static void main(String[] args) {
		// System.out.println(HTTP_LF);
	}

}
