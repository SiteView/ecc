package com.dragonflow.siteview.exchange;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
public class ExchMngtShellProcess {
	   private long readOutputTimeout;
	   private Process proc;
	   private OutputReaderThread outputReader;
	 
	   public static ExchMngtShellProcess create(int timeout)
	   {
	     return new ExchMngtShellProcess(timeout * 1000);
	   }
	 
	   private ExchMngtShellProcess(long timeout) {
	     this.readOutputTimeout = timeout;
	   }
	 
	   public List<AbstractExchMngtShellData> runCommand(ExchMngtShellCmdlet cmdlet, MultiValueHashMap<String, String> countersNames, Map<String, String> propsMapper, Map<String, Map<String, String>> results, Map<String, String> errResults) 
	   {
	     String[] command = cmdlet.buildCmdletAsArray(propsMapper);
	 
	     ProcessBuilder pb = new ProcessBuilder(command);
	     pb.redirectErrorStream(true);
	    try
	     {
	       this.proc = pb.start();
	     } catch (RuntimeException e) {
	     // throw new SiteScopeOperationalException(30201L);
	     } catch (IOException e) {
	      // throw new SiteScopeOperationalException(30201L);
	     }
	 
	     OutputStream outputStream = this.proc.getOutputStream();
	     try {
	       outputStream.close();
	     } catch (IOException e) {
	       //throw new SiteScopeOperationalException(30200L);
	     }
	 
	     InputStream inputStream = this.proc.getInputStream();
	     if (inputStream == null) {
	      // throw new SiteScopeOperationalException(30200L);
	     }
	 
	    this.outputReader = new OutputReaderThread(inputStream);
	    this.outputReader.start();
	 
	     List data = null;
	     if (waitForOutput()) {
	       List outputResult = this.outputReader.getResultList();
	       try {
			data = ExchMngtShellParser.parseSingleCommand(cmdlet, countersNames, outputResult, results, errResults);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	     }
	 
	     this.proc.destroy();
	     return data;
	   } 
	   public void stop()
	   {
	     try
	     {
	       this.outputReader.join(5000L);
	     } catch (InterruptedException e) {
	     }
	 
	     this.proc.destroy();
	   }

	   private boolean waitForOutput() {
		      try {
		    	  
		       this.outputReader.join(this.readOutputTimeout);
		       if (this.outputReader.isSucceeded()) {
		         return true;
		         }
		       if (this.outputReader.isAlive()) {
		           this.outputReader.interrupt();
		       } else {
		          return false;}
		         }
		        catch (InterruptedException e) {
		       
		      }
		        return false;
	   }
}
