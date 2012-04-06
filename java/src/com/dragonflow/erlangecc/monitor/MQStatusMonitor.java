package com.dragonflow.erlangecc.monitor;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.dragonflow.siteview.ErlangUtils;
import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ericsson.otp.erlang.OtpErlangList;

public class MQStatusMonitor extends BaseMonitor {

	static final String EMPTY_ERLANG_LIST_STRING = (new OtpErlangList())
			.toString();
	String CCSID = "";
	String HostName = "";
	String PortNumber = "1414";
	String ChannelName = "";
	String QueueMgr = "";
	String AltQueueMgr = "";
	String ReturnMqStatCodes = "ibmCodes";
	String CipherSuite = "";
	String Username = "";
	String Password = "";
	String LatestEventTime = "";
	String LastMsgDate = "";
	String LastMsgTime = "";
	String obj = "";
	List<String> Measurements = new ArrayList<String>();
	private MQConnector mqConn;
	String lastError = "";
	static String exit_path = ServicePlatform.getRoot()
			+ "/templates.applications/MQ_config.cg";

	// this function must run befor update()
	public void initArgs(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("hostName");
			if (s != null && s.length() > 0) {
				this.HostName = s.trim();
			}
			s = (String) map.get("portNumber");
			if (s != null && s.length() > 0) {
				this.PortNumber = s.trim();
			}
			s = (String) map.get("channelName");
			if (s != null && s.length() > 0) {
				this.ChannelName = s.trim();
			}
			s = (String) map.get("queueMgr");
			if (s != null && s.length() > 0) {
				this.QueueMgr = s.trim();
			}
			s = (String) map.get("altQueueMgr");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.AltQueueMgr = s.trim();
			}
			s = (String) map.get("returnMqStatCodes");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.ReturnMqStatCodes = s.trim();
			}
			s = (String) map.get("cipherSuite");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.CipherSuite = s.trim();
			}
			s = (String) map.get("username");
			if (s != null && (s.length() > 0)
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Username = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Password = s.trim();
			}
			s = (String) map.get("latestEventTime");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.LatestEventTime = s.trim();
			}
			s = (String) map.get("lastMsgDate");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.LastMsgDate = s.trim();
			}
			s = (String) map.get("lastMsgTime");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.LastMsgTime = s.trim();
			}
			
			OtpErlangList measurements = (OtpErlangList) map
					.get("measurements");
			this.Measurements = ErlangUtils.erlangList2arrayList(measurements);
             
             /*
			 // test data 
			this.Measurements = new ArrayList<String>();
			 this.Measurements.add("Queues/t222/tChannel Time Between Sends");
			 this.Measurements.add(
			 "Channels/tSSS/tNo. of Channel Messages Transferred");
			*/
			HashMap<String, String> mqParms = new HashMap<String, String>();
			String s1 = null;
			try {
				File file = new File(exit_path);
				BufferedReader bufRead = new BufferedReader(
						new InputStreamReader(new FileInputStream(file)));
				while ((s1 = bufRead.readLine()) != null) {
					String[] Args = s1.split("=");
					if (Args.length > 1) {
						mqParms.put(Args[0], Args[1]);
					} else {
						mqParms.put(Args[0], "");
					}
				}

				if (mqParms.containsKey("_mqServerCCSID")) {
					CCSID = mqParms.get("_mqServerCCSID");
				}

			} catch (IOException ioe) {
			}

		} catch (Exception e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}

	}

	public void initArgs1(Map<String, Object> map) {
		String s = null;

		try {
			s = (String) map.get("hostName");
			if (s != null && s.length() > 0) {
				this.HostName = s.trim();
			}
			s = (String) map.get("portNumber");
			if (s != null && s.length() > 0) {
				this.PortNumber = s.trim();
			}
			s = (String) map.get("channelName");
			if (s != null && s.length() > 0) {
				this.ChannelName = s.trim();
			}
			s = (String) map.get("queueMgr");
			if (s != null && s.length() > 0) {
				this.QueueMgr = s.trim();
			}
			s = (String) map.get("cipherSuite");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.CipherSuite = s.trim();
			}
			s = (String) map.get("username");
			if (s != null && (s.length() > 0)
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Username = s.trim();
			}
			s = (String) map.get("password");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.Password = s.trim();
			}
			s = (String) map.get("obj");
			if (s != null && s.length() > 0
					&& !s.equals(EMPTY_ERLANG_LIST_STRING)) {
				this.obj = s;
			}
			HashMap<String, String> mqParms = new HashMap<String, String>();
			String s1 = null;
			try {
				File file = new File(exit_path);
				BufferedReader bufRead = new BufferedReader(
						new InputStreamReader(new FileInputStream(file)));
				while ((s1 = bufRead.readLine()) != null) {
					String[] Args = s1.split("=");
					if (Args.length > 1) {
						mqParms.put(Args[0], Args[1]);
					} else {
						mqParms.put(Args[0], "");
					}
				}

				if (mqParms.containsKey("_mqServerCCSID")) {
					CCSID = mqParms.get("_mqServerCCSID");
				}

			} catch (IOException ioe) {
			}

		} catch (Exception e) {
			lastError = e.getMessage();
			e.printStackTrace();
		}

	}

	public ArrayList<String> getInstances() {
		ArrayList<String> result = new ArrayList<String>();
		this.initArgs1(this.getMessage().getParams());

		StringBuffer error = new StringBuffer();
		try {
			Object queueManager = getMQConnector().connectToQueueMgr(
					this.QueueMgr, this.HostName, this.PortNumber,
					this.ChannelName, this.Username, this.Password, error,
					this.CipherSuite, this.CCSID);
			String[] instances = null;
			if (this.obj.equals("Channels"))
				instances = getMQConnector().getChannelNamesUsingQueueManager(
						queueManager, error, this.CCSID);
			else {
				instances = getMQConnector().getQueueNamesUsingQueueManager(
						queueManager, error, this.CCSID);
			}
			if (instances != null) {
				for (String s : instances) {
					result.add(s);
				}
			}
		} catch (Exception e) {
			error.append("error:" + e.getMessage());
			lastError = error.toString();
			result.add(error.toString());
		}

		return result;
	}

	private MQConnector getMQConnector() {
		if (this.mqConn == null) {
			this.mqConn = new MQConnector(this.ChannelName);
		}
		return this.mqConn;
	}

	public static String getCounterName(String tag) {
		String[] ss = tag.split("/t");
		return ss[2];
	}

	public static String getObjectName(String tag) {
		String[] ss = tag.split("/t");
		return ss[0];
	}

	public static String getInstanceName(String tag) {
		String[] ss = tag.split("/t");
		return ss[1];
	}

	public static boolean isQueueEventMeasure(String measure) {
		return (measure.indexOf("Event: Queue") >= 0);
	}

	private boolean isChannelEventMeasure(String measure) {
		return (measure.indexOf("Event: Channel") >= 0);
	}

	private boolean isQueueMeasure(String measure) {
		String token = getObjectName(measure);
		boolean result = token.equalsIgnoreCase("Queues");
		return result;
	}

	private void transferEventValues(TreeMap eventCounters, String[] values) {
		for (Iterator i$ = eventCounters.keySet().iterator(); i$.hasNext();) {
			Object eventCounter = i$.next();
			String counterName = (String) eventCounter;
			int delim = counterName.indexOf(" == ");
			int index = Integer.parseInt(counterName.substring(delim + 4)
					.trim());
			values[index] = ((String) eventCounters.get(counterName));
		}
	}

	private String[] getPrevMsgSendTimeStamp(String channel) {
		String[] result = new String[2];

		String prevLastMsgDate = this.LastMsgDate;
		String prevLastMsgTime = this.LastMsgTime;

		String resultDate = findTimeStampForChannel(channel, prevLastMsgDate);
		String resultTime = findTimeStampForChannel(channel, prevLastMsgTime);

		result[0] = ((resultDate == null) ? "" : resultDate);
		result[1] = ((resultTime == null) ? "" : resultTime);

		return result;
	}

	private void setPrevMsgSendTimeStamp(String channel, String newMsgDate,
			String newMsgTime, Map<String, Object> result) {
		String prevLastMsgDate = this.LastMsgDate;
		String prevLastMsgTime = this.LastMsgTime;

		prevLastMsgDate = (prevLastMsgDate == null) ? "" : prevLastMsgDate;
		prevLastMsgTime = (prevLastMsgTime == null) ? "" : prevLastMsgTime;

		String newMsgDateString = UpdateTimeStampForChannel(channel,
				prevLastMsgDate, newMsgDate);
		String newMsgTimeString = UpdateTimeStampForChannel(channel,
				prevLastMsgTime, newMsgTime);

		result.put("lastMsgDate", newMsgDateString);
		result.put("lastMsgTime", newMsgTimeString);
	}

	private String UpdateTimeStampForChannel(String channel,
			String propertyValue, String replacementTimeStamp) {
		if ((channel == null) || (propertyValue == null)) {
			return null;
		}

		int chIndex = propertyValue.indexOf(channel + ":");
		if (chIndex < 0) {
			if (replacementTimeStamp == null) {
				return null;
			}
			return ((propertyValue.length() == 0) ? "" : new StringBuilder()
					.append(propertyValue).append(",").toString())
					+ channel + ":" + replacementTimeStamp;
		}

		int valueIndex = chIndex + channel.length() + 1;
		int valueEnd = propertyValue.indexOf(",", valueIndex);

		if (replacementTimeStamp == null) {
			if (valueEnd < 0) {
				return propertyValue.substring(valueIndex);
			}
			return propertyValue.substring(valueIndex, valueEnd);
		}

		if (valueEnd < 0) {
			return propertyValue.substring(0, valueIndex)
					+ replacementTimeStamp;
		}
		return propertyValue.substring(0, valueIndex) + replacementTimeStamp
				+ propertyValue.substring(valueEnd);
	}

	private String findTimeStampForChannel(String channel, String propertyValue) {
		return UpdateTimeStampForChannel(channel, propertyValue, null);
	}

	public Map<String, Object> update() {
		Map<String, Object> result = new HashMap<String, Object>();
		this.initArgs(this.getMessage().getParams());
		String[] values = new String[Measurements.size()];
		StringBuffer errorStr = new StringBuffer();
		MQConnector conn = getMQConnector();
		int errorCount = 0;
		if (this.AltQueueMgr == null) {
			AltQueueMgr = "";
		}
		boolean useOrigMqCodes = this.ReturnMqStatCodes.equals("ibmCodes");
		Object queueMgrObj = null;
		try {
			queueMgrObj = conn.connectToQueueMgr(this.QueueMgr, this.HostName,
					this.PortNumber, this.ChannelName, this.Username,
					this.Password, errorStr, this.CipherSuite, this.CCSID);
		} catch (Exception ex) {
			lastError = ex.getMessage();
		}

		if (queueMgrObj == null) {
			for (String measurement : Measurements) {
				result.put(measurement, "n/a");
			}
			result.put("NoData", "n/a");
			result.put("countersInError", Measurements.size());
			result.put("stateString", "connect to MQ fail!");
			return result;
		}

		try {
			TreeMap qEventCounters = new TreeMap();
			TreeMap chEventCounters = new TreeMap();

			for (int i = 0; i < Measurements.size(); ++i) {
				String counter = Measurements.get(i);
				String instance = getInstanceName(counter);
				String attrName = getCounterName(counter);
				if (isQueueEventMeasure(counter)) {
					qEventCounters.put(counter + " == " + i, "");
				} else if (isChannelEventMeasure(counter)) {
					chEventCounters.put(counter + " == " + i, "");
				} else {
					if (isQueueMeasure(counter)) {
						Object queueObj = conn.openQueue(queueMgrObj, instance,
								errorStr);
						try {
							if (queueObj != null)
								values[i] = conn.getValueForQueueAttr(queueObj,
										attrName, errorStr);
						} finally {
							if (queueObj != null) {
								conn.closeQueue(queueObj);
							}
						}
					} else if (attrName.equals("Channel Time Between Sends")) {
						String[] timeStamp = getPrevMsgSendTimeStamp(instance);
						String prevLastMsgDate = timeStamp[0];
						String prevLastMsgTime = timeStamp[1];
						String[] returns = conn
								.getValueForChannelTimeBetweenSends(
										queueMgrObj, instance, prevLastMsgDate,
										prevLastMsgTime, errorStr);
						if ((errorStr.length() > 0)
								&& (((returns[2] == null) || (returns[2]
										.length() == 0)))) {
							values[i] = "n/a";
						} else
							values[i] = returns[2];

						if ((returns[0] != null) && (returns[1] != null))
							setPrevMsgSendTimeStamp(instance, returns[0],
									returns[1], result);
					} else {
						values[i] = conn.getValueForOneChannelAttr(queueMgrObj,
								instance, attrName, useOrigMqCodes, errorStr);
					}

					if (values[i] == null) {
						values[i] = "n/a";
					}

					if (values[i].equals("n/a")) {
						++errorCount;
					}
				}
			}
			long latestEventTime = 0L;
			long recordedLatestEventTime = 0L;

			String eventTime = this.LatestEventTime;
			if ((eventTime != null) && (eventTime.length() > 0)) {
				recordedLatestEventTime = Long.parseLong(eventTime);
			}

			if (qEventCounters.size() > 0) {
				latestEventTime = conn.getMQEventValues(queueMgrObj, QueueMgr,
						AltQueueMgr, qEventCounters, true,
						recordedLatestEventTime, errorStr);
				transferEventValues(qEventCounters, values);
			}

			if (chEventCounters.size() > 0) {
				long anotherLatestEventTime = conn.getMQEventValues(
						queueMgrObj, QueueMgr, AltQueueMgr, chEventCounters,
						false, recordedLatestEventTime, errorStr);
				transferEventValues(chEventCounters, values);
				if (anotherLatestEventTime > latestEventTime) {
					latestEventTime = anotherLatestEventTime;
				}
			}

			if (latestEventTime > 0L) {
				result.put("latestEventTime", latestEventTime);
			}

			String stateString = "";
			for (int i = 0; i < Measurements.size(); ++i) {
				// measurements[i].setValue(values[i]);
				String cter = Measurements.get(i);

				stateString = stateString + this.Measurements.get(i) + "="
						+ values[i];
				if (i < Measurements.size() - 1) {
					stateString = stateString + ",";
				}
				try
				{
				int V=Integer.parseInt(values[i]) ;
				result.put(cter, V);
				}catch(Exception ex)
				{
					result.put(cter, values[i]);
				}
				result.put("stateString", stateString);
			}
			result.put("countersInError", errorCount);
		} finally {
			conn.disconnectFromQueueMgr(queueMgrObj);
		}
		return result;
	}

	@Override
	public int handleMessage() {
		// TODO Auto-generated method stub
		String action = this.getMessage().getAction();
		Map<String, Object> result = null;
		if (action != null && action.equals("update")) {
			result = this.update();
			ArrayList<String> R = new ArrayList<String>();
			if (lastError.length() > 0) {
				R.add(lastError);
				this.sendResponse3("error", R);
			} else {
				System.out.println("update result: " + result);
				this.sendResponse2("ok", result);
			}

		} else if (action != null && action.equals("getInstances")) {
			ArrayList<String> R = new ArrayList<String>();
			R = this.getInstances();
			if (lastError.length() > 0) {
				this.sendResponse3("error", R);
			} else {
				this.sendResponse3("ok", R);
			}

		} else {
			ArrayList<String> R = new ArrayList<String>();
			R.add("no such action. \n");
			this.sendResponse3("error", R);
		}
		return 0;
	}

}
