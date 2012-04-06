package com.dragonflow.siteview.infra.ipmi;

import java.security.MessageDigest;

import com.dragonflow.siteview.infra.ipmi.Constants;
import com.dragonflow.siteview.infra.ipmi.IPMIException;
import com.dragonflow.siteview.infra.ipmi.IPMIMessage;
import com.dragonflow.siteview.infra.ipmi.IPMISession;


public class IPMISessionedMessage extends IPMIMessage
{

 public IPMISessionedMessage(IPMISession ipmiSession)
 {
     this.ipmiSession = ipmiSession;
 }

 public int parseDataToMessage(byte responseData[], int offset)
     throws IPMIException
 {
     int size = ipmiSession.processData(responseData, offset);
     offset += size;
     return super.parseDataToMessage(responseData, offset);
 }

 public int buildDataToSend(byte data[], int offset)
     throws IPMIException
 {
     int originalOffset = offset;
     int sessionOffset = offset;
     int sessionHeaderSize = ipmiSession.getMessageData(data, offset);
     offset += sessionHeaderSize;
     int messageBodyOffset = offset;
     int messageBodySize = super.buildDataToSend(data, offset);
     offset += messageBodySize;
     if(ipmiSession.isSessionActivated() && (ipmiSession.getAuthType() == Constants.IPMI_SESSION_AUTHTYPE_MD5 || ipmiSession.getAuthType() == Constants.IPMI_SESSION_AUTHTYPE_MD2))
     {
         String algorithmStr = null;
         if(ipmiSession.getAuthType() == Constants.IPMI_SESSION_AUTHTYPE_MD5)
             algorithmStr = "MD5";
         else
         if(ipmiSession.getAuthType() == Constants.IPMI_SESSION_AUTHTYPE_MD2)
             algorithmStr = "MD2";
         byte messageBody[] = new byte[messageBodySize - 1];
         System.arraycopy(data, messageBodyOffset + 1, messageBody, 0, messageBodySize - 1);
         byte hashedAuthCode[] = hashMessage(algorithmStr, messageBody);
         if(hashedAuthCode == null || hashedAuthCode.length != 16)
             throw new IPMIException("Failed to hash the message");
         System.arraycopy(hashedAuthCode, 0, data, sessionOffset + 9, 16);
     }
     return offset - originalOffset;
 }

 public byte[] hashMessage(String algorithmStr, byte messageBody[])
     throws IPMIException
 {
     if(!algorithmStr.equalsIgnoreCase("MD5") && !algorithmStr.equalsIgnoreCase("MD2"))
         throw new IPMIException((new StringBuilder()).append("algorithm not supported:").append(algorithmStr).append(". can accept MD5 or MD2").toString());
     MessageDigest algorithm = null;
     try
     {
         algorithm = MessageDigest.getInstance(algorithmStr);
     }
     catch(Exception e)
     {
         throw new IPMIException((new StringBuilder()).append("Failed to find algorithm :").append(algorithmStr).toString());
     }
     algorithm.reset();
     algorithm.update(ipmiSession.getUserPassword());
     algorithm.update(ipmiSession.getSessionID());
     algorithm.update(messageBody);
     algorithm.update(ipmiSession.getInboundSessionSeq());
     algorithm.update(ipmiSession.getUserPassword());
     byte digest[] = algorithm.digest();
     return digest;
 }

 protected IPMISession ipmiSession;
}

