package com.dragonflow.siteview.exchange;

import java.util.HashMap;
import java.util.Map;



public class ServerRolesMapper {

	  private static Map<String, Map<String, String>> mapper = new HashMap();
	  
	   public static void addServerRolesMapper(String serverName, Map<String, String> roles)
	   {
	      mapper.put(serverName, roles);
	   }
	  
	    public static Map<String, String> getServerRolesMapper(String serverName) {
	      Map roles = (Map)mapper.get(serverName);
	     if (roles == null) {
	       return new HashMap();
	      }
	     return roles;
	    }

}
