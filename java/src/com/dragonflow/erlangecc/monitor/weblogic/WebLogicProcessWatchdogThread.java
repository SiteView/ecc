package com.dragonflow.erlangecc.monitor.weblogic;


import java.rmi.Naming;


class WebLogicProcessWatchdogThread extends Thread
{

    public WebLogicProcessWatchdogThread(WeblogicProcessProperties webLogicProcessProperties)
    {
        processProperties = webLogicProcessProperties;
    }

    public void run()
    {
        try
        {
            Thread.sleep(120000L);
        }
        catch(InterruptedException e)
        {
            System.err.println(e.toString());
        }
        do
        {
            try
            {
                WeblogicService wlService = (WeblogicService)Naming.lookup(processProperties.getURL());
                wlService.getToken();
            }
            catch(Exception e)
            {
            	System.err.println("WebLogicProcessWatchdogThread failed to invoke remote method WebLogicService.getToken().  Terminating process, will be automatically restarted during next WebLogic Monitor run.");
                 processProperties.kill();
                return;
            }
            try
            {
                Thread.sleep(WeblogicProcessProperties.getWatchdogThreadFrequency());
            }
            catch(InterruptedException e)
            {
                System.err.println("Sleep interrupted in WebLogicProcessWatchdogThread." + e.toString());
            }
        } while(true);
    }

    private WeblogicProcessProperties processProperties;

}