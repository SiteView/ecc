/*
 * 
 * Created on 2005-2-15 12:54:01
 *
 * DispatcherConnection.java
 *
 * History:
 *
 */
package com.dragonflow.erlangecc.monitor;

/**
 * Comment for <code>DispatcherConnection</code>
 * 
 * @author
 * @version 0.0
 * 
 * 
 */
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

//import COM.dragonflow.Log.LogManager;
//import COM.dragonflow.Utils.TextUtils;

// Referenced classes of package COM.dragonflow.SiteView:
// Platform, DispatcherSitter

public class DispatcherConnection {

    Socket socket;

    OutputStream os;

    DataOutputStream dos;

    InputStream in;

    public DispatcherConnection() {
        socket = null;
        os = null;
        dos = null;
        in = null;
    }

    public synchronized String connectToDispatcher(String s) {
        String s1 = writeCommand(s);
        if (s1.length() == 0) {
            s1 = readCommand();
        }
        return s1;
    }

    String makeErrMsg(int i, String s) {
        String s1 = i + " " + s;
        s1 = s1.length() + " " + s1;
        return s1;
    }

    String connect(int i, long l) {
        String s = "";
        try {
            if (socket == null) {
                socket = new Socket("127.0.0.1", i);
                os = socket.getOutputStream();
                dos = new DataOutputStream(os);
                in = socket.getInputStream();
            }
            int j = (int) (l - System.currentTimeMillis());
            if (j > 10) {
//                Platform.setSocketTimeout(socket, j);
                socket.setSoTimeout(j);
            }
//            LogManager.log("RunMonitor", "DispatcherMonitor Connected");
        } catch (Exception exception) {
            close();
//            LogManager.log("error", "DispatcherMonitor Connect Exception, "
//                    + exception.toString());
//            LogManager.log("RunMonitor",
//                    "DispatcherMonitor Connect Exception, "
//                            + exception.toString());
            s = makeErrMsg(1, "Error connecting to dispatcher");
        }
        return s;
    }

    String writeCommand(String s) {
        long l = System.currentTimeMillis() + 0x2bf20L;
        String s1 = "";
        try {
            s1 = connect(getPort(), l);
            if (s1.length() == 0) {
                dos.writeBytes(s);
                dos.writeByte(0);
            }
        } catch (Exception exception) {
            close();
//            LogManager.log("error", "DispatcherMonitor Write Exception, "
//                    + exception.toString());
//            LogManager.log("RunMonitor", "DispatcherMonitor Write Exception, "
//                    + exception.toString());
//            s1 = makeErrMsg(1, "Error writing to dispatcher");
        }
        return s1;
    }

    void close() {
        try {
            if (os != null) {
                os.close();
            }
        } catch (IOException ioexception) {
        }
        try {
            if (in != null) {
                in.close();
            }
        } catch (IOException ioexception1) {
        }
        try {
            if (dos != null) {
                dos.close();
            }
        } catch (IOException ioexception2) {
        }
        try {
            if (socket != null) {
                socket.close();
//                LogManager.log("error",
//                        "DispatcherMonitor: closing dispatcher socket");
            }
        } catch (Exception exception) {
        }
        socket = null;
        os = null;
        dos = null;
        in = null;
    }

    static int getPort() {
//        if (DispatcherSitter.isRunning) {
//            return DispatcherSitter.getPort();
//        } else {
            return 2001;
//        }
    }

    /**
     * CAUTION: Decompiled by hand.
     * 
     * @return
     */
    String readCommand() {
        byte[] buf = new byte[1024];
        String cmd = new String();
        if (this.in != null) {
            while (!cmd.startsWith("\0", cmd.length() - 1)) {
                try {
                    int i = in.read(buf);
                    if (i < 0) {
                        break;
                    } else {
                        String tmp = new String(buf, 0, i);
                        if (i > 0) {
                            cmd = cmd + tmp;
                        }
                    }
                } catch (Exception e) {
//                    LogManager
//                            .log("error", "DispatcherMonitor Read Exception, "
//                                    + e.toString());
//                    LogManager
//                            .log("RunMonitor",
//                                    "DispatcherMonitor Read Exception, "
//                                            + e.toString());
                    return makeErrMsg(1, "Error reading from dispatcher");
                }
            }
        }

        int j = cmd.indexOf(" ");
        if (j >= 0) {
            String sub = cmd.substring(0, j);
            int i = java.lang.Integer.parseInt(sub);
//            int k = TextUtils.toInt(sub) + sub.length() + 1;
            int k = i + sub.length() + 1;
            if (k < cmd.length()) {
//                TextUtils
//                        .debugPrint("Declared length is different than actual length");
                cmd = cmd.substring(0, k);
            }
        }
//        LogManager.log("RunMonitor", "DispatcherMonitor readCommand=" + cmd);
        return cmd;
    }
}
