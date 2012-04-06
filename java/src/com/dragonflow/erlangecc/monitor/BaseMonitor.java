package com.dragonflow.erlangecc.monitor;

import java.util.ArrayList;
import java.util.Map;

import com.dragonflow.erlangecc.util.SvMessage;
import com.dragonflow.siteview.ErlangUtils;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public abstract class BaseMonitor extends Thread {
	private SvMessage message;
	
	public abstract int handleMessage();
	
	public BaseMonitor(){
		
	}

	public BaseMonitor(SvMessage message) {
		super();
		this.setMessage(message);
	}

	@Override
	public void run() {
		this.handleMessage();
	}

	public void setMessage(SvMessage message) {
		this.message = message;
	}

	public SvMessage getMessage() {
		return message;
	}

	public int sendResponse(String status,Map<String,Object> Result){
		OtpErlangObject[] response = new OtpErlangObject[3];
		OtpErlangList list = ErlangUtils.map2ErlangList(Result);
		response[0] = new OtpErlangAtom(status);
		response[1] = message.getMailbox().self();	
		response[2] = list;				

		OtpErlangTuple msg = new OtpErlangTuple(response);

		message.getMailbox().send(message.getRemotePid(), msg);
		
		return 0;
	}
	
	public int sendResponse2(String status,Map<String,Object> Result){
		OtpErlangObject[] response = new OtpErlangObject[3];
		OtpErlangList list = ErlangUtils.map2ErlangList2(Result);
		response[0] = new OtpErlangAtom(status);
		response[1] = message.getMailbox().self();	
		response[2] = list;				

		OtpErlangTuple msg = new OtpErlangTuple(response);

		message.getMailbox().send(message.getRemotePid(), msg);
		
		return 0;
	}
	public int sendResponse3(String status,ArrayList<String> Result){
		OtpErlangObject[] response = new OtpErlangObject[3];
		OtpErlangList list = ErlangUtils.arrayList2ErlangList(Result);
		response[0] = new OtpErlangAtom(status);
		response[1] = message.getMailbox().self();	
		response[2] = list;				

		OtpErlangTuple msg = new OtpErlangTuple(response);

		message.getMailbox().send(message.getRemotePid(), msg);
		
		return 0;
	}
	
	public int sendResponse(String status,String Desc){
		OtpErlangObject[] response = new OtpErlangObject[3];
		response[0] = new OtpErlangAtom(status);
		response[1] = message.getMailbox().self();	
		response[2] = new OtpErlangList(Desc);				

		OtpErlangTuple msg = new OtpErlangTuple(response);

		message.getMailbox().send(message.getRemotePid(), msg);
		
		return 0;
	}
}
