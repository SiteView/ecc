package com.dragonflow.erlangecc.monitor;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.StringTokenizer;
import java.io.IOException;
import java.lang.System;
import java.net.SocketException;

import com.dragonflow.erlangecc.common.ErrorCode;
import com.dragonflow.erlangecc.util.SvMessage;
import com.dragonflow.siteview.ErlangUtils;
import com.ericsson.otp.erlang.OtpErlangList;
import edu.bucknell.net.JDHCP.DHCPMessage;
import edu.bucknell.net.JDHCP.DHCPSocket;

public class DHCPMonitor extends BaseMonitor{
	private String lastError;
	private String IP;
	private int Timeout;
    private boolean unavailable;
    long startTime;

    public DHCPMonitor()
    {
        startTime = 0L;
        unavailable = false;
    }
    
	int getValues(Map<String,Object> values, String ip, String timeout)
	{
        try
        {
            Class.forName("edu.bucknell.net.JDHCP.DHCPSocket");
        }
        catch(ClassNotFoundException cnfe)
        {
    		lastError = "JDHCP libraries unavailable (see Help document for DHCP Monitor)";
    		return 1;
        }
        
        IP = ip;
        Timeout = Integer.parseInt(timeout);
        
        int sleepTime = 1000;
        Random r = new Random();
        int xid = r.nextInt();
        byte hwaddr[] = new byte[16];
        

        startTime = System.currentTimeMillis();
        Object results[] = acquireAddress(xid, hwaddr, sleepTime);
        if(results[0].equals("n/a"))
        {
        	lastError = (String)results[4];
        	return 1;
        }
        else
        {
        	values.put("Roundtime", results[1]);
        	values.put("Address", results[4]);
        }
        
        return 0;
	}
	
    private byte[] stringToIP(String value)
    {
        byte ip[] = new byte[4];
        StringTokenizer st = new StringTokenizer(value, ".");
        if(st.countTokens() != 4)
            return ip;
        for(int i = 0; i < 4; i++)
        {
            Integer val = new Integer(st.nextToken());
            ip[i] = val.byteValue();
        }

        return ip;
    }
    
    private byte[] intToBytes(int value)
    {
        int quotient = value;
        byte b[] = new byte[4];
        for(int i = 3; i > -1; i--)
        {
            b[i] = (byte)((char)quotient % 256);
            quotient /= 256;
        }

        return b;
    }

    private String ipToString(byte address[])
    {
        String s = "";
        for(int i = 0; i < 4; i++)
        {
            int current = (char)address[i] % 256;
            s = (new StringBuilder()).append(s).append("").append(current).toString();
            if(i < 3)
                s = (new StringBuilder()).append(s).append(".").toString();
        }

        return s;
    }
   
    private boolean sendMessage(String status, DHCPMessage outMessage, DHCPSocket socket, int xid, int type, byte offeredAddr[])
    {
        if(type == 3)
            outMessage.setOption(50, offeredAddr);
        return sendMessage(status, outMessage, socket, xid, type);
    }
    
    private boolean sendMessage(String status, DHCPMessage outMessage, DHCPSocket socket, int xid, int type)
    {
        outMessage.setOp((byte)1);
        outMessage.setHtype((byte)1);
        outMessage.setHlen((byte)6);
        outMessage.setHops((byte)0);
        outMessage.setXid(xid);
        outMessage.setSecs((short)0);
        if(type == 1 || type == 3)
        {
            outMessage.setFlags((short)-32768);
        } else
        {
            outMessage.setFlags((short)0);
        }
        byte initAddr[] = new byte[4];
        for(int i = 0; i < 4; i++)
            initAddr[i] = 0;

        outMessage.setCiaddr(initAddr);
        outMessage.setYiaddr(initAddr);
        outMessage.setSiaddr(initAddr);
        outMessage.setGiaddr(initAddr);
        byte mtype[] = new byte[1];
        mtype[0] = (byte)type;
        outMessage.setOption(53, mtype);
        outMessage.setOption(51, intToBytes(1));
        try
        {
            socket.send(outMessage);
        }
        catch(IOException ie)
        {
            status = ie.getMessage();
            return false;
        }
        return true;
    }
    
    private DHCPSocket openSocket(DHCPSocket s)
    {
        try
        {
            s = new DHCPSocket(68);
        }
        catch(SocketException se)
        {
        	if(s != null)
        		s.close();
            lastError = se.getMessage();
            return null;
        }
        return s;
    }

    private boolean checkOffer(DHCPMessage discoverReply, int xid)
    {
        byte replType[] = new byte[1];
        replType = discoverReply.getOption(53);
        if(replType[0] != 2)
        {
            return false;
        }
        if(discoverReply.getXid() != xid)
        {
            return false;
        }
 
        String offered = ipToString(discoverReply.getYiaddr());
        String requested = IP;
        if(IP.length() > 0 && !offered.equals(requested))
        {
            lastError = (new StringBuilder()).append("Requested address (").append(IP).append(") unavailable").toString();
            unavailable = true;
            return false;
        } else
        {
            return true;
        }
    }
    
    private Object[] acquireAddress(int xid, byte hwaddr[], int sleepTime)
    {
        Object results[] = new Object[5];
        results[0] = "n/a";
        results[1] = String.valueOf(0);
        results[2] = "error";
        results[3] = "no data";
        results[4] = "no data";
        int releaseTime = -1;
        DHCPSocket socket = null;
        DHCPMessage outMessage = new DHCPMessage();
        DHCPMessage replyMessage = new DHCPMessage();
        byte offeredAddr[] = new byte[4];
        byte backupAddr[] = new byte[4];
        
        Random r = new Random();
 
        if((socket = openSocket(socket)) == null)
        {
            results[3] = results[4] = lastError;
            return results;
        }
        outMessage.setChaddr(hwaddr);
        if(IP.length() > 0)
        {
            outMessage.setOption(50, stringToIP(IP));
        }
        for(int i = 0; i < 10; i++)
        {
            if(!sendMessage(lastError, outMessage, socket, xid, 1))
            {
                results[3] = results[4] = lastError;
                socket.close();
                return results;
            }

            try 
            {
            	Thread.sleep(sleepTime);
            }
            catch(InterruptedException e)
            {
            
            }
            
            if(socket.receive(replyMessage))
            {
                if(checkOffer(replyMessage, xid))
                {
                    offeredAddr = replyMessage.getYiaddr();
                    System.arraycopy(offeredAddr, 0, backupAddr, 0, 4);
                    break;
                }
                if(unavailable)
                {
                    results[3] = results[4] = lastError;
                    socket.close();
                    return results;
                }
            }
            if(i == 9)
            {
                results[3] = results[4] = "Timed out waiting for DHCPOFFER";
                socket.close();
                return results;
            }
        }

        releaseTime = 1;
        outMessage.setOption(54, replyMessage.getOption(54));
        sendMessage(lastError, outMessage, socket, xid, 3, offeredAddr);
        if(!socket.receive(replyMessage))
        {
            results[3] = results[4] = "Timed out waiting for DHCPACK";
            socket.close();
            return results;
        }
        if(replyMessage.getXid() != xid)
        {
            results[3] = results[4] = "Received invalid DHCPACK from server";
            socket.close();
            return results;
        }
        
        try
        {
        	Thread.sleep(1000);
        }
        catch(InterruptedException e)
        {
        
        }
        
        outMessage.setCiaddr(backupAddr);
        outMessage.setOption(50, new byte[4]);
        outMessage.setOption(51, new byte[4]);
        outMessage.setOption(54, replyMessage.getOption(54));
        sendMessage(lastError, outMessage, socket, r.nextInt(), 7);
        socket.close();
        long totalDuration = System.currentTimeMillis() - startTime;
        String stateString = (new StringBuilder()).append("leased address ").append(ipToString(backupAddr)).toString();
        String duration = (float)totalDuration / 1000F + " sec";
        results[0] = results[1] = duration;
        results[2] = "ok";
        results[3] = (new StringBuilder()).append("leased address ").append(ipToString(backupAddr)).toString();
        results[4] = stateString;
        return results;
    }	
    
	public int handleMessage() {
		SvMessage msg = this.getMessage();
		if (msg.getAction().equals("getValues")){
			Map<String,Object> params = msg.getParams();
			Map<String,Object> values = new HashMap<String,Object>();
			String ip = (String)params.get("ip");
			String timeout = (String)params.get("timeout");
			
			if (0==this.getValues(values, ip, timeout)){
				this.sendResponse2("ok", values);
			}else{
				if(this.getLastError().length()>250)
					this.sendResponse(this.getLastError().substring(0, 250), values);
				else
					this.sendResponse(this.getLastError(), values);
			}
		}
		return ErrorCode.OK;
	}

	public String getLastError() {
		return lastError;
	}
	
	private void setLastError(String Error) {
		lastError = Error;
	}

	public static void main(String[] args) {
		DHCPMonitor ipmi = new DHCPMonitor();
		Map<String,Object> values = new HashMap<String,Object>();
		ipmi.getValues(values, "192.168.2.215", "3000");
//		ipmi.getCounters(counters, "192.168.3.2", "623", "root", "root");
	}
}
