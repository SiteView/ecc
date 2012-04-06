package com.dragonflow.erlangecc.monitor;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeMap;

import com.dragonflow.siteview.infra.util.ServicePlatform;
import com.ibm.mq.MQC;
import com.ibm.mq.MQException;
import com.ibm.mq.MQGetMessageOptions;
import com.ibm.mq.MQMessage;
import com.ibm.mq.MQQueue;
import com.ibm.mq.MQQueueManager;
import com.ibm.mq.MQSecurityExit;
import com.ibm.mq.pcf.CMQCFC;
import com.ibm.mq.pcf.PCFException;
import com.ibm.mq.pcf.PCFMessage;
import com.ibm.mq.pcf.PCFMessageAgent;

public class MQConnector {

	private static MQSecurityExit _mqSecurityExit = null;
	public static final String QUEUES = "Queues";
	public static final String CHANNELS = "Channels";
	private static final String QUEUE_DEPTH = "Current Queue Depth";
	private static final String QUEUE_INPUT_COUNT = "Queue Open Input Count";
	private static final String QUEUE_OUTPUT_COUNT = "Queue Open Output Count";
	private static final String QUEUE_DEPTH_HIGH = "Event: Queue Depth High";
	private static final String QUEUE_DEPTH_LOW = "Event: Queue Depth Low";
	private static final String QUEUE_FULL = "Event: Queue Full";
	private static final String QUEUE_SERVICE_INTERVAL_HIGH = "Event: Queue Service Interval High";
	private static final String QUEUE_SERVICE_INTERVAL_OK = "Event: Queue Service Interval Ok";
	private static final String CHANNEL_BYTES_RECEIVED = "Channel Bytes Received";
	private static final String CHANNEL_BYTES_SENT = "Channel Bytes Sent";
	private static final String CHANNEL_STATUS = "Channel Status";
	public static final String CHANNEL_TIME_SENDS = "Channel Time Between Sends";
	private static final String CHANNEL_BUFFERS_SENT = "No. of Channel Buffers Received";
	private static final String CHANNEL_BUFFERS_RECEIVED = "No. of Channel Buffers Sent";
	private static final String CHANNEL_MESSAGES_TRANSFERRED = "No. of Channel Messages Transferred";
	private static final String CHANNEL_ACTIVATED = "Event: Channel Activated";
	private static final String CHANNEL_NOT_ACTIVATED = "Event: Channel Not Activated";
	private static final String CHANNEL_STARTED = "Event: Channel Started";
	private static final String CHANNEL_STOPPED = "Event: Channel Stopped";
	private static final String CHANNEL_STOPPED_BY_USER = "Event: Channel Stopped by User";
	private static final HashMap eventMap = new HashMap();
	private static final String SECURITY_EXIT_KEY = "_mqMonitorSecurityExit";
	private static final String SSL_CIPHER_SUITE_PROPERTY = "SSL Cipher Suite";
	private PCFMessageAgent messageAgent;
	public static final int iMDate = 0;
	public static final int iMTime = 1;
	public static final int iMDiffValue = 2;
	private final int iEventName = 0;
	private final int iQMgrName = 1;
	private final int iQName = 2;
	static String exit_path = ServicePlatform.getRoot()
			+ "/templates.applications/MQ_config.cg";
	String ccsid = "";
	private final int iEventReason = 0;
	private final int iNumParms = 1;

	static {
		eventMap.put("Event: Queue Depth High", String.valueOf(2224));
		eventMap.put("Event: Queue Depth Low", String.valueOf(2225));
		eventMap.put("Event: Queue Full", String.valueOf(2053));
		eventMap
				.put("Event: Queue Service Interval High", String.valueOf(2226));
		eventMap.put("Event: Queue Service Interval Ok", String.valueOf(2227));
		eventMap.put("Event: Channel Activated", String.valueOf(2295));
		eventMap.put("Event: Channel Not Activated", String.valueOf(2296));
		eventMap.put("Event: Channel Started", String.valueOf(2282));
		eventMap.put("Event: Channel Stopped", String.valueOf(2283));
		eventMap.put("Event: Channel Stopped by User", String.valueOf(2279));
	}

	public MQConnector(String systemChannel) {
		init(systemChannel);
	}

	public void init(String systemChannel) {
		com.ibm.mq.MQEnvironment.port = 1414;
		if ((systemChannel != null) && (systemChannel.length() > 0)) {
			com.ibm.mq.MQEnvironment.channel = systemChannel;
		}
		setSecurityExit();
	}

	private void setSecurityExit() {
		String exitName = "";
		HashMap<String, String> mqParms = new HashMap<String, String>();
		String s = null;
		try {
			File file = new File(exit_path);
			BufferedReader bufRead = new BufferedReader(new InputStreamReader(
					new FileInputStream(file)));
			while ((s = bufRead.readLine()) != null) {
				String[] Args = s.split("=");
				if (Args.length > 1) {
					mqParms.put(Args[0], Args[1]);
				} else {
					mqParms.put(Args[0], "");
				}
			}
			if (mqParms.containsKey("_mqMonitorSecurityExit")) {
				exitName = mqParms.get("_mqMonitorSecurityExit");
			}
			if (mqParms.containsKey("_mqServerCCSID")) {
				ccsid = mqParms.get("_mqServerCCSID");
			}

		} catch (IOException ioe) {
		}

		if ((exitName == null) || (exitName.length() == 0))
			return;
		try {
			if (_mqSecurityExit == null) {
				Class exitClass = Class.forName(exitName);
				_mqSecurityExit = (MQSecurityExit) exitClass.newInstance();
				// status("setSecurityExit() - exit class loaded.\n");
			}
			com.ibm.mq.MQEnvironment.securityExit = _mqSecurityExit;
		} catch (Exception ex) {
			// error(ex.toString());
		}
	}

	public Object connectToQueueMgr(String qMgrName, String host, String port,
			String channel, String username, String password,
			StringBuffer errorStr, String cipherSuite, String ccsid) {
		MQQueueManager qMgr = null;

		// status("Hitting " + host + ":" + port + " (" + channel + ") for QM("
		// + qMgrName + ")");

		Hashtable properties = new Hashtable();
		properties.put("hostname", host);
		properties.put("channel", channel);
		properties.put("port", new Integer(port));

		int ccsidValue = getCCSIDIntValue(ccsid);
		if (ccsidValue != 0) {
			properties.put("CCSID", Integer.valueOf(ccsidValue));
		}

		if (!username.isEmpty()) {
			properties.put("userID", username);
		}
		if (!password.isEmpty()) {
			properties.put("password", password);
		}

		if ((cipherSuite != null) && (cipherSuite.length() > 0)) {
			// s_Logger.debug("Found a CIPHER_SUITE_PARAMTER in sslProperties");
			properties.put("SSL Cipher Suite", cipherSuite);
		} else {
			// s_Logger
			// .debug("Didn't find a CIPHER_SUITE_PARAMTER in sslProperties");
		}

		try {
			qMgr = new MQQueueManager(qMgrName, properties);

			initPCFMessageAgent(qMgr, 0);
		} catch (MQException ex) {
			// error(getMqErrorMessage(ex), errorStr);
			// s_Logger.debug("", ex);
			errorStr.append(ex.getMessage());
		}
		return qMgr;
	}

	private MQException initPCFMessageAgent(MQQueueManager mgr, int timeout) {
		try {
			if (timeout > 0) {
				try {
					Thread.sleep(timeout);
				} catch (InterruptedException e) {
					// s_Logger.debug("Exception: ", e);
				}
			}
			this.messageAgent = new PCFMessageAgent(mgr);
			this.messageAgent.setCharacterSet(Integer.valueOf(this.ccsid));
		} catch (MQException e) {
			return e;
		}
		return null;
	}

	public Object openQueue(Object queueMgrObj, String qName,
			StringBuffer errorStr) {
		MQQueue queue = null;
		MQQueueManager queueMgr = (MQQueueManager) queueMgrObj;
		try {
			int openOptions = 40;
			queue = queueMgr.accessQueue(qName, openOptions, null, null, null);
			// status("Opened queue for Browse and Inquiry");
		} catch (Exception ex) {
			// error(ex.toString(), errorStr);
		}

		return queue;
	}

	public void closeQueue(Object queueObj) {
		MQQueue queue = (MQQueue) queueObj;
		try {
			queue.close();
		} catch (Exception ex) {
			// s_Logger.debug("Exception: " + ex, ex);
		}
	}

	public void disconnectFromQueueMgr(Object qMgrObj) {
		MQQueueManager qMgr = (MQQueueManager) qMgrObj;
		try {
			this.messageAgent.disconnect();
			this.messageAgent = null;
		} catch (Exception ex) {
			// s_Logger.debug("Exception: " + ex, ex);
		}
		try {
			qMgr.disconnect();
		} catch (Exception ex) {
			// s_Logger.debug("Exception: " + ex, ex);
		}
	}

	public String[] getMQObjects() {
		String[] result = { "Channels", "Queues" };
		return result;
	}

	public String[] getQueueNames(String host, String port, String channel,
			StringBuffer errorStr) {
		String[] names = null;
		PCFMessageAgent agent = null;
		PCFMessage[] responses;
		try {
			// status("Connecting to queue manager at " + host + ":" + port
			// + " over channel " + channel + " for getting queues...");

			agent = new PCFMessageAgent(host, Integer.parseInt(port), channel);

			// status("Connected.");

			PCFMessage request = new PCFMessage(18);
			request.addParameter(2016, "*");
			request.addParameter(20, 1001);

			// status("Sending PCF request... ");

			responses = agent.send(request);
			// status("Received reply.");

			names = (String[]) (String[]) responses[0].getParameterValue(3011);
		} catch (PCFException pcfe) {
			// error("Error in response: ");
			// error(getMqErrorMessage(pcfe), errorStr);

			responses = (PCFMessage[]) (PCFMessage[]) pcfe.exceptionSource;

			for (int i = 0; i < responses.length; ++i) {
				// error(responses[i].toString());
			}

		} catch (MQException mqe) {
			// error(getMqErrorMessage(mqe), errorStr);
		} catch (IOException ioe) {
			// error(ioe.toString(), errorStr);
		} finally {
			if (agent != null) {
				try {
					agent.disconnect();
				} catch (MQException e) {
					// s_Logger.error("Error closing the PCF agent", e);
				}
			}
		}

		return names;
	}

	public String[] getQueueNamesUsingQueueManager(Object queueManager,
			StringBuffer errorStr, String ccsid) {
		String[] names = null;
		String[] tempnames = null;
		PCFMessageAgent agent = null;
		PCFMessage[] responses;
		try {
			// status("Connecting to queue manager " + queueManager
			// + " for getting queues...");

			agent = new PCFMessageAgent((MQQueueManager) queueManager);
			initPCFMessageAgentCharacterSet(agent, ccsid);

			// status("Connected.");

			PCFMessage request = new PCFMessage(18);
			request.addParameter(2016, "*");
			request.addParameter(20, 1001);

			// status("Sending PCF request... ");

			responses = agent.send(request);
			// status("Received reply.");

			tempnames = (String[]) (String[]) responses[0]
					.getParameterValue(3011);
			int i = 0;
			if (tempnames != null) {
				names=new String[tempnames.length];
				for (String s : tempnames) {
					if (!s.isEmpty()) {
						names[i] = s.trim();
						++i;
					}
				}
			}
		} catch (PCFException pcfe) {
			// error("Error in response: ");
			// error(getMqErrorMessage(pcfe), errorStr);

			responses = (PCFMessage[]) (PCFMessage[]) pcfe.exceptionSource;

			for (int i = 0; i < responses.length; ++i) {
				// error(responses[i].toString());
			}

		} catch (MQException mqe) {
			// error(getMqErrorMessage(mqe), errorStr);
		} catch (IOException ioe) {
			// error(ioe.toString(), errorStr);
		} finally {
			if (agent != null) {
				try {
					agent.disconnect();
				} catch (MQException e) {
					// s_Logger.error("Error closing the PCF agent", e);
				}
			}
		}

		return names;
	}

	public String[] getChannelNames(String host, String port, String channel,
			StringBuffer errorStr) {
		String[] names = null;
		PCFMessageAgent agent = null;
		PCFMessage[] responses;
		try {
			// status("Connecting to queue manager at " + host + ":" + port
			// + " over channel " + channel + " for getting queues...");

			agent = new PCFMessageAgent(host, Integer.parseInt(port), channel);

			// status("Connected.");

			PCFMessage request = new PCFMessage(20);
			request.addParameter(3501, "*");
			request.addParameter(1511, 5);

			// status("Sending PCF request... ");

			responses = agent.send(request);
			// status("Received reply.");

			if ((responses == null) || (responses.length == 0)) {
				String[] arrayOfString1 = new String[0];
				return arrayOfString1;
			}
			Object channels = new ArrayList();
			for (int index = 0; index < responses.length; ++index) {
				names = (String[]) (String[]) responses[index]
						.getParameterValue(3512);
				if ((names != null) && (names.length > 0)) {
					for (String chName : names) {
						if (chName != null) {
							((List) channels).add(chName);
						}
					}
				}
			}
			names = (String[]) ((List) channels)
					.toArray(new String[((List) channels).size()]);
		} catch (PCFException pcfe) {
			// error("Error in response: ");
			// error(getMqErrorMessage(pcfe), errorStr);

			responses = (PCFMessage[]) (PCFMessage[]) pcfe.exceptionSource;
			for (int i = 0; i < responses.length; ++i) {
				// error(responses[i].toString());
			}

		} catch (MQException mqe) {
			// error(getMqErrorMessage(mqe), errorStr);
		} catch (IOException ioe) {
			// error(ioe.toString(), errorStr);
		} finally {
			if (agent != null) {
				try {
					agent.disconnect();
				} catch (MQException e) {
					// s_Logger.error("Error closing the PCF agent", e);
				}
			}
		}

		return ((String[]) names);
	}

	public String[] getChannelNamesUsingQueueManager(Object queueManager,
			StringBuffer errorStr, String ccsid) {
		PCFMessageAgent agent = null;
		PCFMessage[] responses;
		try {
			// status("Connecting to queue manager " + queueManager
			// + " for getting queues...");

			agent = new PCFMessageAgent((MQQueueManager) queueManager);
			initPCFMessageAgentCharacterSet(agent, ccsid);

			// status("Connected.");

			PCFMessage request = new PCFMessage(20);
			request.addParameter(3501, "*");
			request.addParameter(1511, 5);

			// status("Sending PCF request... ");

			responses = agent.send(request);
			// status("Received reply.");

			List channels = new ArrayList();
			for (int index = 0; index < responses.length; ++index) {
				String[] names = (String[]) (String[]) responses[index]
						.getParameterValue(3512);
				if ((names != null) && (names.length > 0)) {
					for (String chName : names) {
						if (!chName.isEmpty()) {
							channels.add(chName.trim());
						}
					}
				}
			}
			String[] index = (String[]) channels.toArray(new String[channels
					.size()]);

			return index;
		} catch (PCFException e) {
			// error("Error in response: ");
			// error(getMqErrorMessage(pcfe), errorStr);

			responses = (PCFMessage[]) (PCFMessage[]) e.exceptionSource;

			for (int i = 0; i < responses.length; ++i) {
				// error(responses[i].toString());
			}

		} catch (MQException mqe) {
			// error(getMqErrorMessage(mqe), errorStr);
		} catch (IOException ioe) {
			// error(ioe.toString(), errorStr);
		} finally {
			if (agent != null) {
				try {
					agent.disconnect();
				} catch (MQException e) {
					// s_Logger.error("Error closing the PCF agent", e);
				}
			}
		}

		return null;
	}

	public String[] getAttributesForQueue(String qName) {
		String[] attrs = { "Current Queue Depth", "Queue Open Input Count",
				"Queue Open Output Count", "Event: Queue Depth High",
				"Event: Queue Depth Low", "Event: Queue Full",
				"Event: Queue Service Interval High",
				"Event: Queue Service Interval Ok" };

		return attrs;
	}

	public String getValueForQueueAttr(Object queueObj, String attrName,
			StringBuffer errorStr) {
		String result = null;
		MQQueue queue = (MQQueue) queueObj;
		try {
			int value = 0;
			if (attrName.equalsIgnoreCase("Current Queue Depth")) {
				value = queue.getCurrentDepth();
			} else if (attrName.equalsIgnoreCase("Queue Open Input Count")) {
				value = queue.getOpenInputCount();
			} else if (attrName.equalsIgnoreCase("Queue Open Output Count")) {
				value = queue.getOpenOutputCount();
			} else {
				// error("Queue attribute not recognized.", errorStr);
				return "";
			}

			result = String.valueOf(value);
		} catch (Exception ex) {
			// error(ex.toString(), errorStr);
		}

		return result;
	}

	public String getValueForOneChannelAttr(Object qMgrObj, String channelName,
			String attrName, boolean useOrigMQchannelStatusCodes,
			StringBuffer errorStr) {
		String result = "";
		MQQueueManager qMgr = (MQQueueManager) qMgrObj;
		Integer value = null;
		try {
			// PCFMessage request = new PCFMessage(42);
			PCFMessage request = new PCFMessage(
					CMQCFC.MQCMD_INQUIRE_CHANNEL_STATUS);
			// request.addParameter(3501, channelName);
			request.addParameter(CMQCFC.MQCACH_CHANNEL_NAME, channelName);
			int[] attrs = { mapChannelAttrNameToAttrConstant(attrName) };

			if (!(attrName.equalsIgnoreCase("Channel Status"))) {
				request.addParameter(1524, attrs);
			}

			PCFMessage[] responses = getPCFMessageAget(qMgr).send(request);
			value = (Integer) responses[0].getParameterValue(attrs[0]);

			if ((attrName.equalsIgnoreCase("Channel Status"))
					&& (!(useOrigMQchannelStatusCodes)))
				result = String
						.valueOf(mapChannelStatusCodes(value.intValue()));
			else {
				result = value.toString();
			}

		} catch (PCFException pcfe) {
			if (pcfe.reasonCode == 3065)
				if (attrName.equals("Channel Status"))
					if (useOrigMQchannelStatusCodes)
						result = "-1";
					else
						result = String.valueOf(mapChannelStatusCodes(6));
				else
					result = "0";
			else {
				// error(getMqErrorMessage(pcfe), errorStr);
			}
		} catch (MQException mqe) {
			// error(getMqErrorMessage(mqe), errorStr);
		} catch (IOException ioe) {
			// error(ioe.toString(), errorStr);
		}

		return result;
	}

	private PCFMessageAgent getPCFMessageAget(MQQueueManager mgr)
			throws MQException {
		if (this.messageAgent == null) {
			MQException me = null;
			int delay = (int) ((System.currentTimeMillis() % 3L + 3L) * 100L);
			if (initPCFMessageAgent(mgr, delay) != null) {
				delay = (int) ((System.currentTimeMillis() % 3L + 3L) * 200L);
				if ((me = initPCFMessageAgent(mgr, delay)) != null) {
					throw me;
				}
			}
		}
		return this.messageAgent;
	}

	public String[] getValueForChannelTimeBetweenSends(Object qMgrObj,
			String channelName, String prevLastMsgDate, String prevLastMsgTime,
			StringBuffer errorStr) {
		String[] result = new String[3];
		MQQueueManager qMgr = (MQQueueManager) qMgrObj;
		String diffStr = "";

		PCFMessageAgent agent = null;
		try {
			agent = new PCFMessageAgent(qMgr);
			PCFMessage request = new PCFMessage(42);

			request.addParameter(3501, channelName);
			int[] attrs = { 3525, 3524 };

			request.addParameter(1524, attrs);

			PCFMessage[] responses = agent.send(request);

			String lastMsgDate = (String) responses[0]
					.getParameterValue(attrs[0]);
			String lastMsgTime = (String) responses[0]
					.getParameterValue(attrs[1]);

			if (isValidDateTime(lastMsgDate, lastMsgTime)) {
				int diff = calcTimeDiff(prevLastMsgDate, prevLastMsgTime,
						lastMsgDate, lastMsgTime);
				if (diff >= 0)
					diffStr = String.valueOf(diff);
			} else {
				// Time is null for " + channelName + ".");
				lastMsgDate = lastMsgTime = null;
			}

			result[0] = lastMsgDate;
			result[1] = lastMsgTime;
			result[2] = diffStr;
		} catch (PCFException pcfe) {
			// error("Error retrieving ChannelTimeBetweenSends for " +
			// channelName + ": " + getMqErrorMessage(pcfe), errorStr);
		} catch (MQException mqe) {
			// error("Error retrieving ChannelTimeBetweenSends for " +
			// channelName + ": " + getMqErrorMessage(mqe), errorStr);
		} catch (IOException ioe) {
			// error("Error retrieving ChannelTimeBetweenSends for " +
			// channelName + ": " + ioe.toString(), errorStr);
		} finally {
			if (agent != null) {
				try {
					agent.disconnect();
				} catch (MQException e) {
					// s_Logger.error("Error closing the PCF agent", e);
				}
			}
		}

		return result;
	}

	private boolean isValidDateTime(String date, String time) {
		if ((date == null) || (time == null)) {
			return false;
		}
		if (date.trim().length() < 8) {
			return false;
		}

		return (time.trim().length() >= 8);
	}

	private int calcTimeDiff(String prevLastMsgDate, String prevLastMsgTime,
			String lastMsgDate, String lastMsgTime) {
		SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd hh.mm.ss");
		Calendar calendar = new GregorianCalendar();
		int diff = -1;

		if ((prevLastMsgDate.length() == 0) || (prevLastMsgTime.length() == 0)) {
			return -1;
		}
		try {
			Date prevDate = formatter.parse(prevLastMsgDate + " "
					+ prevLastMsgTime);
			Date nowDate = formatter.parse(lastMsgDate + " " + lastMsgTime);

			calendar.setTime(prevDate);
			long prevMSeconds = calendar.getTimeInMillis();

			calendar.setTime(nowDate);
			long nowMSeconds = calendar.getTimeInMillis();

			// 1000L);
			// status("Now(" + nowMSeconds + ") - Prev(" + prevMSeconds + ") = "
			// + diff);
		} catch (Exception ex) {
			// error("Exception parsing LastMsgDate or LastMsgTime.");
		}

		return diff;
	}

	public String[] getAttributesForChannel(String channelName) {
		String[] attrs = { "Channel Bytes Received", "Channel Bytes Sent",
				"Channel Status", "Channel Time Between Sends",
				"No. of Channel Buffers Sent",
				"No. of Channel Buffers Received",
				"No. of Channel Messages Transferred",
				"Event: Channel Activated", "Event: Channel Not Activated",
				"Event: Channel Started", "Event: Channel Stopped",
				"Event: Channel Stopped by User" };

		return attrs;
	}

	private int mapChannelAttrNameToAttrConstant(String channelAttr) {
		int result = 0;

		if (channelAttr.equals("Channel Bytes Received"))
			result = 1536;
		else if (channelAttr.equals("Channel Bytes Sent"))
			result = 1535;
		else if (channelAttr.equals("No. of Channel Buffers Sent"))
			result = 1539;
		else if (channelAttr.equals("No. of Channel Buffers Received"))
			result = 1538;
		else if (channelAttr.equals("No. of Channel Messages Transferred"))
			result = 1534;
		else if (channelAttr.equals("Channel Status")) {
			result = 1527;
		}

		return result;
	}

	private int mapChannelStatusCodes(int mqCode) {
		int result = mqCode;

		switch (mqCode) {
		case 6:
		case 8:
			result = 0;
			break;
		case 4:
		case 13:
			result = 1;
			break;
		case 2:
			break;
		case 5:
			result = 3;
			break;
		case 7:
			result = 4;
			break;
		case 1:
			result = 5;
			break;
		case 3:
			result = 6;
		case 9:
		case 10:
		case 11:
		case 12:
		}
		return result;
	}

	public long getMQEventValues(Object qMgrObj, String qMgrName,
			String altQMgrName, TreeMap eventCounters, boolean isQueue,
			long recordedLatestEventTime, StringBuffer errorStr) {
		MQQueueManager qMgr = (MQQueueManager) qMgrObj;
		int openOptions = 8;
		long latestEventTime = 0L;

		MQQueue myQueue = null;
		Set countersSet = eventCounters.keySet();

		int[] eventValues = new int[eventCounters.size()];
		for (int i = 0; i < eventValues.length; ++i)
			eventValues[i] = 0;
		String qName;
		if (isQueue)
			qName = "SYSTEM.ADMIN.PERFM.EVENT";
		else {
			qName = "SYSTEM.ADMIN.CHANNEL.EVENT";
		}
		try {
			myQueue = qMgr.accessQueue(qName, openOptions, null, null, null);
		} catch (MQException mqe) {
			// error("Could not open queue " + qName + " (" +
			// getMqErrorMessage(mqe) + ")", errorStr);
			// s_Logger.debug("", mqe);
			return latestEventTime;
		}

		try {
			MQGetMessageOptions gmo = new MQGetMessageOptions();
			gmo.options = 17;
			MQMessage myMessage = new MQMessage();

			boolean done = false;
			label560: do {
				try {
					myMessage.clearMessage();
					myMessage.correlationId = MQC.MQCI_NONE;
					myMessage.messageId = MQC.MQMI_NONE;

					myQueue.get(myMessage, gmo);

					// status("Message format: " + myMessage.format);
					// status("Message putDateTime: " + myMessage.putDateTime);

					gmo.options = 33;

					if (!(myMessage.format.equalsIgnoreCase("MQEVENT "))) {
						// status("Skipping non event message.");

						break label560;
					}

					long eventTime = myMessage.putDateTime.getTimeInMillis();
					if (eventTime <= recordedLatestEventTime) {
						// status("Skipping old event (" + eventTime + " <= " +
						// recordedLatestEventTime + ")");
						break label560;
					}

					String[] eventInfo = getEventInfo(myMessage);
					if ((eventInfo == null) || (eventInfo[0] == null)
							|| (eventInfo[1] == null) || (eventInfo[2] == null)) {
						// status("Skipping event due to lack of event info.");
						break label560;
					}

					String eventQMgrName = eventInfo[1];
					if ((!(eventQMgrName.equalsIgnoreCase(qMgrName)))
							&& (!(eventQMgrName.equalsIgnoreCase(altQMgrName)))) {
						// c qmgrname does not match primary or alt qmgrname
						// (" + eventQMgrName + " != " + qMgrName + " or
						// " + altQMgrName + ")");
						break label560;
					}

					boolean isBeingMonitored = matchEventToCounter(countersSet,
							eventValues, eventInfo[0], eventInfo[2]);

					if ((isBeingMonitored) && (eventTime > latestEventTime)) {
						latestEventTime = eventTime;
					}
				} catch (MQException ex) {
					if (ex.reasonCode != 2033) {
						// error(getMqErrorMessage(ex), errorStr);
					}
					done = true;
				} catch (IOException ex) {
					// error("MQ Monitor exception: " + ex.toString(),
					// errorStr);
					done = true;
				}
			} while (!(done));

			Iterator it = countersSet.iterator();
			int i = 0;
			while (it.hasNext()) {
				String value = String.valueOf(eventValues[(i++)]);
				String counterName = (String) it.next();
				eventCounters.put(counterName, value);
			}
		} finally {
			try {
				myQueue.close();
			} catch (MQException mqe) {
				// error("MQ getEventInfo: queue close error - " + mqe);
			}

		}

		return latestEventTime;
	}

	private String[] getEventInfo(MQMessage msg) {
		String[] result = new String[3];

		int[] headerInfo = getInfoFromMQCFH(msg);
		int codeNumber = headerInfo[0];
		int numParms = headerInfo[1];

		String eventCode = (codeNumber == -1) ? null : String
				.valueOf(codeNumber);
		result[0] = eventCode;
		try {
			for (int i = 0; i < numParms; ++i) {
				int structType = msg.readInt();
				// status("Parm No. " + i);

				if (structType != 4) {
					int structSize = msg.readInt();
					msg.skipBytes(structSize - 8);
					// status("Skipped non-String Parm No. " + i);
				} else {
					int structLen = msg.readInt();

					int parmType = msg.readInt();
					// status("Parm Type: " + parmType);

					msg.skipBytes(4);

					int valueLen = msg.readInt();

					String parmValue = msg.readString(valueLen);
					// status("Parm value: " + parmValue);

					int toSkip = structLen - (20 + valueLen);
					if (toSkip > 0) {
						msg.skipBytes(toSkip);
					}

					switch (parmType) {
					case 2015:
						result[1] = parmValue.trim();
						break;
					case 2002:
					case 3501:
						result[2] = parmValue.trim();
						break;
					default:
						// status("Unknown parm type in MQCFST (" +
						// String.valueOf(parmType) + ")");
					}
				}
			}
		} catch (Exception ex) {
			// error("MQ getEventInfo exception - " + ex);
			// s_Logger.debug("", ex);
		}

		// status("Event name = " + eventCode);
		// status("Event qMgrName = " + result[1]);
		// Ch name = " + result[2]);

		return result;
	}

	private int[] getInfoFromMQCFH(MQMessage msg) {
		int[] result = { -1, -1 };
		try {
			int type = msg.readInt();
			if (type != 7) {
				return result;
			}

			int length = msg.readInt();

			int version = msg.readInt();
			if (version != 1) {
				// error("Warning: MQMonitor - MQCFH version (" + version
				// + ") is unknown - code may fail.");
			}

			int command = msg.readInt();

			msg.skipBytes(12);

			result[0] = msg.readInt();

			result[1] = msg.readInt();
		} catch (Exception ex) {
			// error("Exception reading MQCFH: " + ex);
		}

		return result;
	}

	private boolean matchEventToCounter(Set countersSet, int[] values,
			String eventCodeFromServer, String instanceNameFromServer) {
		int i = 0;

		Iterator it = countersSet.iterator();
		while (it.hasNext()) {
			String fullCounterName = (String) it.next();
			String instance = MQStatusMonitor.getInstanceName(fullCounterName);
			if ((instance.equalsIgnoreCase(instanceNameFromServer))
					&& (eventsAreEqual(fullCounterName, eventCodeFromServer))) {
				values[i] += 1;
				return true;
			}
			// Ch (" + instance + " != " + instanceNameFromServer + ") or
			// mismatched event");

			++i;
		}

		return false;
	}

	private boolean eventsAreEqual(String ssCounterName, String eventCode) {
		boolean result = false;

		String ssEvent = MQStatusMonitor.getCounterName(ssCounterName);

		int delim = ssEvent.indexOf(" == ");
		ssEvent = ssEvent.substring(0, delim);

		String mqEventCode = (String) eventMap.get(ssEvent);

		if ((mqEventCode != null) && (mqEventCode.equals(eventCode))) {
			result = true;
		} else if (mqEventCode == null)
			;
		// status("Event type mismatch due to null event code (counter="
		// + ssEvent + ")");
		else {
			// status("Event type mismatch (" + mqEventCode + " != " +
			// eventCode);
		}

		return result;
	}

	public static void main(String[] args) {
		if (args.length < 1) {
			System.out.println("Required parameter missing - queue name");
		} else {
			System.out.println("mqbrowse: browse and optionally get messages");
			MQConnector mySample = new MQConnector("SYSTEM.DEF.SVRCONN");

			mySample.start(args, "testwin2k8", "1414", "S_QM1");
			mySample.start(args, "testwin2k3", "1414", "S_QM1");
			mySample.start(args, "199.203.78.115", "1414", "S_QM1");
		}
		System.exit(0);
	}

	public void start(String[] args, String host, String port, String channel) {
		String name = "";
		StringBuffer errorStr = new StringBuffer();

		if (args.length > 1) {
			name = args[1];
		}

		MQQueueManager qMgr = null;
		MQQueue myQueue = null;
		try {
			qMgr = (MQQueueManager) connectToQueueMgr(name, host, port,
					channel, null, null, errorStr, "", null);
			myQueue = (MQQueue) openQueue(qMgr, args[0], errorStr);
			if (myQueue == null)
				return;
		} catch (Exception ex) {
			// s_Logger.error(ex.getMessage());
		}

		try {
			InputStreamReader isr = new InputStreamReader(System.in);
			BufferedReader br = new BufferedReader(isr);

			MQGetMessageOptions gmo = new MQGetMessageOptions();
			gmo.options = 17;
			MQMessage myMessage = new MQMessage();

			boolean done = false;
			do {
				try {
					myMessage.clearMessage();
					myMessage.correlationId = MQC.MQCI_NONE;
					myMessage.messageId = MQC.MQMI_NONE;

					myQueue.get(myMessage, gmo);
					String msg = myMessage.readString(myMessage
							.getMessageLength());

					// if (s_Logger.isDebugEnabled()) {
					// s_Logger.debug("Browsed message: " + msg);
					// }
					String runShow = br.readLine();
					if ((runShow.length() > 0)
							&& (((runShow.indexOf("Y") != -1) || (runShow
									.indexOf("y") != -1)))) {
						// if (s_Logger.isDebugEnabled())
						// s_Logger.debug("Actually getting the message");
						gmo.options = 256;
						myQueue.get(myMessage, gmo);
					}

					gmo.options = 33;
				} catch (MQException ex) {
					// if (s_Logger.isDebugEnabled())
					// s_Logger.debug("MQ exception: CC = "
					// + ex.completionCode + " RC = " + ex.reasonCode);
					done = true;
				} catch (IOException ex) {
					// s_Logger.debug("Java exception: " + ex);
					done = true;
				}
			} while (!(done));

			myQueue.close();
			qMgr.disconnect();
		} catch (MQException ex) {
			// s_Logger.debug(getMqErrorMessage(ex));
		}
	}

	// public static String extractInstanceNameFromMeasurement(String measure) {
	// int endingSlash = measure.lastIndexOf(92);
	// int prevSlash = measure.lastIndexOf(92, endingSlash - 1);
	// String result = measure.substring(prevSlash + 1, endingSlash);
	// return result;
	// }
	//
	// public static String extractCounterFromMeasurement(String measure) {
	// int endingSlash = measure.lastIndexOf(92);
	// return measure.substring(endingSlash + 1);
	// }

	private static String getMqErrorMessage(MQException mqEx) {
		String pcfType = "";

		if (mqEx instanceof PCFException) {
			pcfType = "PCF ";
		}
		String msg = "MQ " + pcfType + "exception: CC = " + mqEx.completionCode
				+ " RC = " + mqEx.reasonCode;

		if (mqEx.reasonCode == 2059)
			msg = msg + " (Queue Manager not available)";
		else if (mqEx.reasonCode == 2058) {
			msg = msg + " (Queue Manager name error)";
		}
		return msg;
	}

	private void initPCFMessageAgentCharacterSet(PCFMessageAgent agent,
			String ccsid) {
		if ((ccsid == null) || (agent == null))
			return;
		try {
			int ccsidValue = getCCSIDIntValue(ccsid);
			if (ccsidValue != 0)
				agent.setCharacterSet(ccsidValue);
		} catch (Exception e) {
			// s_Logger.error("Can't parse ccsid value to integer :"
			// + e.getMessage());
		}
	}

	private int getCCSIDIntValue(String ccsid) {
		int ccsidValue = 0;
		if (ccsid != null) {
			try {
				ccsidValue = Integer.valueOf(ccsid).intValue();
			} catch (NumberFormatException e) {
				// s_Logger
				// .debug("Can't parse ccsid value to integer (thus will not set CCSID): "
				// + e.getMessage());
			}
		}

		return ccsidValue;
	}

}
