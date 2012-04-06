package com.dragonflow.siteview.exchange;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.InterruptedIOException;
import java.util.LinkedList;
import java.util.List;

public class OutputReaderThread extends Thread{
//	private static final Log logger = LogFactory.getEasyLog(OutputReaderThread.class);
	   private List<String> resultList;
	   private boolean succeeded = false;
	   private BufferedReader processReader;
	 
	   public OutputReaderThread(InputStream processInputStream)
	   {
	     this.resultList = new LinkedList();
	     this.processReader = new BufferedReader(new InputStreamReader(processInputStream));
	   }
	 
	   public boolean isSucceeded() {
	     return this.succeeded;
	   }
	 
	   private void setSucceeded() {
	     this.succeeded = true;
	   }
	 
	   public List<String> getResultList() {
	     return this.resultList;
	   }
	 
	   public void run()
	   {
	     try
	     {
	       String line;
	       if ((line = this.processReader.readLine()) != null) {
	         if (line.length() != 0);
	         if (!("".equals(line)));
	         this.resultList.add(line);
	       }
	 
	       setSucceeded();
//	       if (logger.isDebugEnabled()) {
//	         StringBuilder content = new StringBuilder();
//	         content.append("\nThe output: \n");
//	         for (String result : this.resultList) {
//	           content.append(result).append('\n');
//	         }
//	         logger.debug(content.toString());
//	       }
	     } catch (InterruptedIOException e) {
	       Thread.currentThread().interrupt();
//	       logger.error("Interrupted via InterruptedIOException: " + e, (logger.isDebugEnabled()) ? e : null);
	     } catch (IOException e) {
//	       if (!(isInterrupted()))
//	         logger.error("Error reading from input stream: " + e, (logger.isDebugEnabled()) ? e : null);
//	       else
//	         logger.error("The thread is interrupted: " + e, (logger.isDebugEnabled()) ? e : null);
	     }
	     finally {
	       try {
	         if (this.processReader != null)
	           this.processReader.close();
	       }
	       catch (IOException e) {
//	         logger.error("Error Closing Stream: " + e, (logger.isDebugEnabled()) ? e : null);
	       }
	     }
//	     if (logger.isDebugEnabled())
//	       logger.debug("OutputReaderThread finished reading and exit");
	   }
	 
	   public void interrupt()
	   {
	     super.interrupt();
	     try {
	       if (this.processReader != null)
	         this.processReader.close();
	     }
	     catch (IOException e) {
//	       if (logger.isDebugEnabled())
//	         logger.debug("Failed to close processReader.", e);
	     }
	   } 

}
