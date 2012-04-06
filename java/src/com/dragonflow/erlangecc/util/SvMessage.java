package com.dragonflow.erlangecc.util;

import java.util.Map;

import com.dragonflow.siteview.ErlangUtils;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpMsg;

public class SvMessage {

	private String remoteNode;
	private OtpErlangPid remotePid;
	private String	action;
	private long	messageId;
	private Map<String,Object> params;
	private OtpMbox		mailbox;
	private String	requestType;
	
	public SvMessage(OtpMbox mailbox) {
		super();
		this.setMailbox(mailbox);
	}
	
	public int fromOtpMsg(OtpMsg otpmsg){
		remotePid = otpmsg.getSenderPid();

		OtpErlangObject erlobject;
		try {
			erlobject = otpmsg.getMsg();
		

			if (erlobject instanceof OtpErlangTuple) {
				OtpErlangTuple t = (OtpErlangTuple) erlobject;
				OtpErlangString strType =  (OtpErlangString)t.elementAt(0);
				requestType = strType.stringValue();
				OtpErlangString strAction =  (OtpErlangString)t.elementAt(1);
				action = strAction.stringValue();
				OtpErlangObject Obj = t.elementAt(t.arity()-1);
				if (Obj instanceof OtpErlangList){
					params = ErlangUtils.erlangListToMap((OtpErlangList)Obj);
				}
	
			}
		} catch (OtpErlangDecodeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return 0;
	}

	/**
	 * @return the remoteNode
	 */
	public String getRemoteNode() {
		return remoteNode;
	}
	/**
	 * @param remoteNode the remoteNode to set
	 */
	public void setRemoteNode(String remoteNode) {
		this.remoteNode = remoteNode;
	}
	/**
	 * @return the remotePid
	 */
	public OtpErlangPid getRemotePid() {
		return remotePid;
	}
	/**
	 * @param remotePid the remotePid to set
	 */
	public void setRemotePid(OtpErlangPid remotePid) {
		this.remotePid = remotePid;
	}
	/**
	 * @return the action
	 */
	public String getAction() {
		return action;
	}
	/**
	 * @param action the action to set
	 */
	public void setAction(String action) {
		this.action = action;
	}
	/**
	 * @return the messageId
	 */
	public long getMessageId() {
		return messageId;
	}
	/**
	 * @param messageId the messageId to set
	 */
	public void setMessageId(long messageId) {
		this.messageId = messageId;
	}
	/**
	 * @return the params
	 */
	public Map<String, Object> getParams() {
		return params;
	}
	/**
	 * @param params the params to set
	 */
	public void setParams(Map<String, Object> params) {
		this.params = params;
	}

	public void setRequestType(String requestType) {
		this.requestType = requestType;
	}

	public String getRequestType() {
		return requestType;
	}

	public void setMailbox(OtpMbox mailbox) {
		this.mailbox = mailbox;
	}

	public OtpMbox getMailbox() {
		return mailbox;
	}
	
	
}
