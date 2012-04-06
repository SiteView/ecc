package com.dragonflow.siteview.infra.ipmi.interfaces.lan;

import com.dragonflow.siteview.infra.ipmi.util.IPMIUtils;


public class ASFMessage
{
 protected class ASFMessageBody
 {

     public int getSize()
     {
         return 8;
     }

     public int getData(byte data[], int offset)
     {
         System.arraycopy(IANA, 0, data, offset, 4);
         offset += 4;
         data[offset] = messageType;
         data[offset + 1] = messageTag;
         data[offset + 2] = reserved;
         data[offset + 3] = 0;
         return getSize();
     }

     private byte IANA[];
     private byte reserved;
     private byte messageType;
     private byte messageTag;
//     final ASFMessage this$0;

     public ASFMessageBody(byte messageType, byte messageTag)
     {
//         this$0 = ASFMessage.this;
//         super();
         reserved = 0;
         IANA = new byte[4];
         IPMIUtils.intToBytes(IANA, 0, 4542);
         this.messageType = messageType;
         this.messageTag = messageTag;
     }
 }


 private ASFMessage()
 {
 }

 public ASFMessage(byte messageType, byte messageTag)
 {
     data = new byte[256];
     asfMessageBody = new ASFMessageBody(messageType, messageTag);
 }

 public int getMessageData(byte data[], int offset)
 {
     int originalOffset = offset;
     int headerSize = asfHeader.getMessageData(data, offset);
     offset += headerSize;
     int messageBodySize = asfMessageBody.getData(data, offset);
     offset += messageBodySize;
     return offset - originalOffset;
 }

 public static byte PING = -128;
 public static byte PONG = 64;
 private static RMCP.RMCPHeader asfHeader = new RMCP.RMCPHeader((byte)6, (byte)-1);
 private ASFMessageBody asfMessageBody;
 private byte data[];

}

