package com.dragonflow.siteview.infra.ipmi;

import com.dragonflow.siteview.infra.ipmi.Command;
import com.dragonflow.siteview.infra.ipmi.util.IPMIUtils;


public class CommandBase
 implements Command
{

 public CommandBase(byte netFn, byte command, byte commandData[])
 {
     this.netFn = netFn;
     this.command = command;
     if(commandData != null)
         this.commandData = commandData;
     else
         this.commandData = new byte[0];
 }

 public byte getNetFn()
 {
     return netFn;
 }

 public byte[] getCommandData()
 {
     return commandData;
 }

 public byte getCommand()
 {
     return command;
 }

 public String toString()
 {
     return (new StringBuilder()).append("Commad code:").append(IPMIUtils.byteToHex(command)).append(" NetFn: ").append(IPMIUtils.byteToHex(netFn)).append(" Command data: ").append(IPMIUtils.bytesToHex(commandData)).toString();
 }

 private byte netFn;
 private byte command;
 private byte commandData[];
}
