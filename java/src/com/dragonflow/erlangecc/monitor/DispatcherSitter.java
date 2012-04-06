/*
 * 
 * Created on 2005-2-15 12:56:27
 *
 * DispatcherSitter.java
 *
 * History:
 *
 */
package com.dragonflow.erlangecc.monitor;

/**
 * Comment for <code>DispatcherSitter</code>
 * 
 * @author
 * @version 0.0
 * 
 * 
 */
import java.io.File;
import java.io.IOException;
import java.net.ServerSocket;
import java.util.Enumeration;

import jgl.Array;
import jgl.HashMap;
//import COM.dragonflow.Log.LogManager;
//import COM.dragonflow.Properties.FrameFile;
//import COM.dragonflow.StandardMonitor.URLMonitor;
//import COM.dragonflow.Utils.CommandLine;
//import COM.dragonflow.Utils.FileUtils;
//import COM.dragonflow.Utils.TextUtils;
import com.dragonflow.siteview.infra.util.ServicePlatform;

// Referenced classes of package COM.dragonflow.SiteView:
// SiteViewGroup, Platform, DispatcherMonitor

public class DispatcherSitter extends Thread {

    public static final String DISPATCHER_CONFIG_FILENAME = "dispatcher.config";

    public static final String DISPATCHER_WINSTART_FILENAME = "disp_start.bat";

    public static final String DISPATCHER_UNIXSTART_FILENAME = "disp_start.sh";

    public static final String DISPATCHER_PORT_FILENAME = "tss_port";

    public static final String DISPATCHER_CLIENT_FILENAME = "tss_mons";

    public static String configPath;

    public static String dispatcherPortPath;

    public static String dispatcherClientPath;

    public static String dispatcherWinStartPath;

    public static String dispatcherUnixStartPath;

    private static String dispBinDirectory;

    private static String dispDirectory = ServicePlatform.getRoot();

    public String cmd;

    boolean portSet;

    public static boolean isRunning = false;

    public static String port = "";

    public static ServerSocket parentSocket = null;

    Process process;

//    public static String getParentPort() {
//        if (parentSocket != null) {
//            return Integer.toString(parentSocket.getLocalPort());
//        }
//        try {
//            parentSocket = new ServerSocket(0);
//        } catch (Exception exception) {
////            LogManager.log("Error",
////                    "Could not create parent port for dispatcher. Description: "
////                            + exception.getMessage());
//        }
//        String s;
//        if (parentSocket == null) {
//            SiteViewGroup siteviewgroup = SiteViewGroup.currentSiteView();
//            s = siteviewgroup.getSetting("_httpActivePort");
//            if (s.length() <= 0) {
//                s = siteviewgroup.getSetting("_httpPort");
//            }
//        } else {
//            s = Integer.toString(parentSocket.getLocalPort());
//        }
//        return s;
//    }

    public DispatcherSitter() {
        cmd = "";
        portSet = false;
        process = null;
//        String s = getParentPort();
//        if (Platform.isWindows()) {
//            cmd = dispatcherWinStartPath + " " + dispBinDirectory + " " + s;
//        } else {
//            cmd = dispatcherUnixStartPath + " " + dispBinDirectory + " " + s;
//        }
    }

//    public static boolean isEnabled() {
//        if ((new File(configPath)).exists()) {
//            HashMap hashmap = getDispatcherConfig();
//            if (TextUtils.getValue(hashmap, "_disabled").length() <= 0) {
//                return true;
//            }
//        }
//        return false;
//    }
//
//    public static HashMap getDispatcherConfig() {
//        HashMap hashmap = null;
//        if (!(new File(configPath)).exists()) {
//            HashMap hashmap1 = new HashMap();
//            hashmap1.put("_dispatcherPort", port);
//            saveDispatcherConfig(hashmap1);
//        }
//        try {
//            Array array = FrameFile.readFromFile(configPath);
//            if (array.size() == 0) {
//                hashmap = new HashMap();
//            } else {
//                hashmap = (HashMap) array.at(0);
//            }
//        } catch (Exception exception) {
////            LogManager.log("Error", "Could not read dispatcher.config from "
////                    + configPath + ": " + exception.getMessage());
//        }
//        return hashmap;
//    }
//
//    public static void saveDispatcherConfig(HashMap hashmap) {
//        try {
//            FrameFile.writeToFile(configPath, hashmap);
//        } catch (Exception exception) {
////            LogManager.log("Error", "Could not write dispatcher.config to "
////                    + configPath + ": " + exception.getMessage());
//        }
//    }
//
//    public static void hardKillProcessOnNT(String s, int i) {
//        if (Platform.isWindows()) {
//            long al[] = Platform.processUsed("", s, 0L, 0L);
//            if (al[1] <= 0L) {
////                LogManager.log("RunMonitor", "Process name " + s
////                        + " was not running");
//                return;
//            }
////            LogManager.log("RunMonitor", "Waiting " + i
////                    + " seconds before killing all processes with the name: "
////                    + s);
//            Platform.sleep(i * 1000);
//            String s1 = Platform.perfexCommand("") + " Process";
//            CommandLine commandline = new CommandLine();
//            Array array = commandline.exec(s1, Platform.getLock(""));
//            Enumeration enumeration = array.elements();
//            boolean flag = false;
//            String s3 = "0";
//            while (enumeration.hasMoreElements()) {
//                String s5 = (String) enumeration.nextElement();
//                int j = s5.indexOf("name:");
//                if (j >= 0) {
//                    int k = s5.indexOf(":");
//                    if (k > 0) {
//                        if (s5.substring(k + 2).indexOf(s) >= 0) {
//                            flag = true;
//                        } else {
//                            flag = false;
//                        }
//                    }
//                }
//                int l = s5.indexOf("ID Process:");
//                if (flag && l >= 0) {
//                    int i1 = s5.indexOf(":");
//                    if (i1 > 0) {
//                        int j1 = s5.substring(i1 + 2).indexOf(" ");
//                        if (j1 > 0) {
//                            String s4 = s5.substring(i1 + 2, i1 + 2 + j1);
////                            LogManager.log("RunMonitor", "Killing process: "
////                                    + s + " (" + s4 + ")");
//                            String s2 = Platform.perfexCommand("") + "-k " + s4;
//                            commandline.exec(s2, Platform.getLock(""));
//                        }
//                    }
//                }
//            } 
//        }
//    }
//
//    public void kill() {
//        stop();
//        DispatcherMonitor.killDispatcher();
//        isRunning = false;
////        LogManager.log("RunMonitor",
////                "Dispatcher Sitter :Killing the Dispatcher");
//        try {
//            if (parentSocket != null) {
//                parentSocket.close();
//            }
//        } catch (Exception exception) {
////            LogManager.log("Error",
////                    "DispatcherSitter.Kill: Not able to close the parent Socket "
////                            + exception.getMessage());
//        }
//    }
//
//    public static boolean isDispatcherInstalled() {
//        return (new File(dispDirectory)).exists();
//    }
//
//    public static boolean isDispatcherRunning() {
//        if (!Platform.isWindows() || !isDispatcherInstalled()) {
//            isRunning = false;
//            return false;
//        }
//        boolean flag = false;
//        long al[] = Platform.processUsed("", "tss_dispatcher", 0L, 0L);
//        if (al[1] > 0L) {
//            flag = true;
//        }
//        isRunning = flag;
//        return flag;
//    }
//
//    public static void setDispatcherClients() {
//        SiteViewGroup siteviewgroup = SiteViewGroup.currentSiteView();
//        try {
//            StringBuffer stringbuffer = FileUtils
//                    .readFile(dispatcherClientPath);
//            String as[] = TextUtils.split(stringbuffer.toString(),
//                    URLMonitor.CRLF);
//            String s = "";
//            for (int i = 0; i < as.length; i++) {
//                s = s + as[i] + "\t";
//            }
//
//            if (s.length() > 0) {
//                siteviewgroup.setProperty("_dispatcherClients", s);
//            }
//        } catch (Exception exception) {
////            LogManager.log("Error",
////                    " Dispatcher Sitter: Was not able to read file "
////                            + dispatcherClientPath);
//            exception.printStackTrace();
//        }
//    }
//
//    public static int getPort() {
//        int i = 0;
//        try {
//            StringBuffer stringbuffer = FileUtils.readFile(dispatcherPortPath);
//            i = TextUtils.toInt(stringbuffer.toString());
//        } catch (Exception exception) {
////            LogManager.log("Error",
////                    " Dispatcher Sitter: Was not able to read file "
////                            + dispatcherPortPath);
//            exception.printStackTrace();
//        }
//        port = String.valueOf(i);
//        return i;
//    }
//
//    /**
//     * CAUTION: Decompiled by hand.
//     */
//    public void run() {
//        while_25_: // TODO need review
//        while (!isRunning) {
//            do {
//                try {
//                    if (cmd.length() > 0) {
//                        process = CommandLine.execSync(cmd);
////                        LogManager.log("RunMonitor",
////                                "Dispatcher Sitter : Starting Process");
//                        break;
//                    }
////                    LogManager
////                            .log("Error",
////                                    "Dispatcher Sitter : Command line parameter is empty");
//                    kill();
//                } catch (IOException ioexception) {
//                    ioexception.printStackTrace();
////                    LogManager.log("Error",
////                            ("Dipatcher Exec Sync Error: " + ioexception
////                                    .getMessage()));
//                }
//                break while_25_;
//            } while (false);
//            int i = 0;
//            int i_9_ = 0;
//            for (/**/; i < 10; i++) {
//                i_9_ = getPort();
//                if (i_9_ > 0)
//                    break;
////                LogManager.log("Error", ("Could not read " + dispatcherPortPath
////                        + " trying again # " + i));
//                Platform.sleep(2000L);
//            }
//            if (i_9_ > 0) {
//                HashMap hashmap = getDispatcherConfig();
//                hashmap.put("_dispatcherPort", String.valueOf(i_9_));
//                portSet = true;
//            } else {
////                LogManager
////                        .log(
////                                "Error",
////                                ("Could not read " + dispatcherPortPath + " stopping the dispatcher"));
//                kill();
//                break;
//            }
//            try {
//                isRunning = true;
//                int i_10_ = process.waitFor();
////                LogManager
////                        .log("RunMonitor",
////                                "Dispatcher Sitter : Normal Termination Process Restart");
//                isRunning = false;
//            } catch (InterruptedException interruptedexception) {
////                LogManager.log("Error",
////                        (" Dispatcher Thread Error: " + interruptedexception
////                                .getMessage()));
//                break;
//            }
//        }
//    }

    static {
    	
        configPath = ServicePlatform.getRoot() + File.separator + "groups"
                + File.separator + "dispatcher.config";
        dispatcherPortPath = ServicePlatform.getRoot() + File.separator + "dat"
                + File.separator + "tss_port";
        dispatcherClientPath = ServicePlatform.getRoot() + File.separator + "dat"
                + File.separator + "tss_mons";
        dispatcherWinStartPath = ServicePlatform.getRoot() + File.separator + "bin"
                + File.separator + "disp_start.bat";
        dispatcherUnixStartPath = ServicePlatform.getRoot() + File.separator + "bin"
                + File.separator + "disp_start.sh";
        dispBinDirectory = ServicePlatform.getRoot() + File.separator + "bin"
                + File.separator;
    }
}
