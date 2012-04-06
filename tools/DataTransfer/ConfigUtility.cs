////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-23
//Time: 16:38
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Collections.Specialized;
using System.Collections.Generic;
using System.Xml;

namespace DataTransfer
{
	/// <summary>
	/// Description of ConfigUtility.
	/// </summary>
	public class ConfigUtility
	{
		private static Dictionary<string, Dictionary<string,string>> generalMappingConfig;
		private const string FILE_CONFIG_PATH = @"./config/config.xml";
		
		public const string CONFIG_MAPPING_CATEGORY_DEVICETYPE = "DeviceType";
		public const string CONFIG_MAPPING_CATEGORY_OS = "OS";
		public const string CONFIG_MONITOR_TEMPLATE = "Monitor_Template";
		
		
		#region Public Method
		
		public static bool LoadConfig()
		{
			bool success =  false;
			try 
			{				
				XmlDocument doc = new XmlDocument();
				doc.Load(FILE_CONFIG_PATH);
				XmlNode siteviewNode = doc.DocumentElement;
				buildGeneralMapping(siteviewNode);
				buildMonitorCounterDicts(siteviewNode);
				buildMonitorTemplateConfig(siteviewNode);
				buildAlertCondationMapping(siteviewNode);
				success = true;
			} catch (Exception) {
				
			}
			return success;
		}
		
		/// <summary>
		/// 获取Ecc8中设备操作系统到Ecc9中远程主机操作系统的映射
		/// </summary>
		/// <param name="deviceOs">Ecc8中设备操作系统</param>
		/// <returns>Ecc9中远程主机操作系统</returns>
		public static string GetOsMapping(string deviceOs)
		{
			return GetMappingValue(CONFIG_MAPPING_CATEGORY_OS,deviceOs);
		}
		
		public static string GetDeviceTypeMapping(string deviceType)
		{
			return GetMappingValue(CONFIG_MAPPING_CATEGORY_DEVICETYPE,deviceType);
		}
		
		public static Dictionary<string,Counter> GetMonitorCounters(string monitorType)
		{
			if(monitorCounterDicts != null && monitorCounterDicts.ContainsKey(monitorType))
				return monitorCounterDicts[monitorType];
			return null;
		}
		
		public static List<Counter> GetMonitorDefaultCounters(string monitorType)
		{
			if(defaultMonitorCounterDicts != null && defaultMonitorCounterDicts.ContainsKey(monitorType))
				return defaultMonitorCounterDicts[monitorType];
			return null;
		}
		
		public static Dictionary<string,string> GetAlertCondations(string monitorType)
		{
			if(alertCondationDicts != null && alertCondationDicts.ContainsKey(monitorType))
				return alertCondationDicts[monitorType];
			return null;
		}
		
		public static Dictionary<string, string> GetMappingDict(string category)
		{
			if(generalMappingConfig.ContainsKey(category))
				return generalMappingConfig[category];
			return null;
		}
		#endregion
		
		#region Private Method
		private static void buildAlertCondationMapping(XmlNode siteviewNode)
		{
			if(null == siteviewNode)
				return;
			
			string alertCondationXmlPath = "./Mappings/AlertCondation/*";
			XmlNodeList alertCondationNodes = siteviewNode.SelectNodes(alertCondationXmlPath);
			if(null == alertCondationNodes)
				return;
			
			alertCondationDicts = new Dictionary<string, Dictionary<string, string>>();
			foreach(XmlNode alertCondationNode in alertCondationNodes)
			{
				XmlNodeList alertCondations = alertCondationNode.SelectNodes("./Mapping[@origin!='' and @target!='']");
				if(null == alertCondations)
					continue;
				
				Dictionary<string, string> condations = new Dictionary<string, string>();
				foreach(XmlNode alertCondation in alertCondations)
				{
					condations.Add(alertCondation.Attributes["origin"].Value
					               ,alertCondation.Attributes["target"].Value);
				}
				
				string monitorType = alertCondationNode.Name.ToLower();
				alertCondationDicts.Add(monitorType,condations);
			}			
		}
		
		private static void buildMonitorCounterDicts(XmlNode siteviewNode)
		{
			if(null == siteviewNode)
				return;
			
			string countersXmlPath = "./Mappings/Counters/*";
			
			XmlNodeList monitorCountersNodes = siteviewNode.SelectNodes(countersXmlPath);
			if(null == monitorCountersNodes)
				return;
			
			monitorCounterDicts = new Dictionary<string, Dictionary<string, Counter>>();
			defaultMonitorCounterDicts = new Dictionary<string, List<Counter>>();
			
			foreach(XmlNode monitorCountersNode in monitorCountersNodes)
			{
				XmlNodeList counters = monitorCountersNode.SelectNodes("./Mapping[@targetIndex!='' and @target!='']");
				
				if(null == counters)
					continue;
				Dictionary<string, Counter> monitorCounters = new Dictionary<string, Counter>();
				List<Counter> defaultMonitorCounters = new List<Counter>();
				foreach(XmlNode counter in counters)
				{
					Counter c = new Counter(counter.Attributes["targetIndex"].Value
					                        ,counter.Attributes["target"].Value);
					
					if(counter.Attributes["origin"] != null 
					   && !string.IsNullOrEmpty(counter.Attributes["origin"].Value))
						monitorCounters.Add(counter.Attributes["origin"].Value,c);
					if(counter.Attributes["default"] != null 
					   && "true".Equals(counter.Attributes["default"].Value,StringComparison.InvariantCultureIgnoreCase))
						defaultMonitorCounters.Add(c);
				}
				
				string monitorType = monitorCountersNode.Name.ToLower();
				monitorCounterDicts.Add(monitorType,monitorCounters);
				defaultMonitorCounterDicts.Add(monitorType,defaultMonitorCounters);
			}
		}
		
		private static void buildMonitorTemplateConfig(XmlNode siteviewNode)
		{
			if(null == siteviewNode)
				return;
			
			tempaltesConfigDict = new Dictionary<string, Dictionary<string, NameValueCollection>>();
			monitorMappingDict = new Dictionary<string, string>();
			string templateXmlPath = "./MonitorTemplateApply/Template[@origin!=''][@target!='']";
			
			XmlNodeList templateNodes = siteviewNode.SelectNodes(templateXmlPath);
			
			if(null != templateNodes)
			{
				foreach(XmlNode template in templateNodes)
				{
					string[] origins = template.Attributes["origin"].Value.Split(',');
					string target = template.Attributes["target"].Value;
					foreach(string origin in origins)
					{
						monitorMappingDict.Add(origin,target);
					}
					XmlNodeList tuplesNodeList = template.ChildNodes;
					if(null == tuplesNodeList)
						continue;
					
					Dictionary<string,NameValueCollection> paramConfig 
						= new Dictionary<string, NameValueCollection>();
					foreach(XmlNode tupleNode in tuplesNodeList)
					{
						XmlNodeList paramNodeList = tupleNode.ChildNodes;
						int count = paramNodeList.Count;
						if( count < 2)
						{
							//不符合key-value
							continue;
						}
						
						NameValueCollection valueParamConfig = new NameValueCollection();
						
						string key = paramNodeList[0].InnerText;
						string tag = paramNodeList[1].Name.ToLower();
						valueParamConfig.Add("tag",tag);
						string property = paramNodeList[1].InnerText;
						valueParamConfig.Add("property",property);
						foreach(XmlAttribute attr in paramNodeList[1].Attributes)
						{
							valueParamConfig.Add(attr.Name.ToLower(),attr.Value);
						}
						
						paramConfig.Add(key,valueParamConfig);
					}
					
					foreach(string origin in origins)
					{
						tempaltesConfigDict.Add(origin,paramConfig);
					}
				}				
			}
		}
		
		private static void buildGeneralMapping(XmlNode siteviewNode)
		{
			generalMappingConfig = new Dictionary<string, Dictionary<string, string>>();
			
			string generalMappingXmlPath = "./Mappings/GeneralMapping/*";
			
			XmlNodeList generalMappingNodes = siteviewNode.SelectNodes(generalMappingXmlPath);
			if(null == generalMappingNodes)
				return;
			
			foreach(XmlNode generalMappingNode in generalMappingNodes)
			{
				string category = generalMappingNode.Name;
				buildMuilToOneMapping(category,generalMappingNode,"./Mapping[@origin!='' and @target!='']");
			}
			
			//buildMuilToOneMapping(CONFIG_MAPPING_CATEGORY_DEVICETYPE,siteviewNode,"./Mappings/DeviceType/Mapping[@origin!=''][@target!='']");
			//buildMuilToOneMapping(CONFIG_MAPPING_CATEGORY_OS,siteviewNode,"./Mappings/Os/Mapping[@origin!=''][@target!='']");
		}
		
		private static void buildMuilToOneMapping(string category,XmlNode siteviewNode,string mappingPath)
		{
			if(null == siteviewNode || string.IsNullOrEmpty(mappingPath))
				return;
						
			XmlNodeList nodes = siteviewNode.SelectNodes(mappingPath);
			if(nodes != null)
			{
				Dictionary<string, string> dict = new Dictionary<string, string>();
				foreach(XmlNode node in nodes)
				{
					string target = node.Attributes["target"].Value.Trim();
					string[] origins = node.Attributes["origin"].Value.Split(',');
					foreach(string origin in origins)
						dict.Add(origin.Trim(),target);
				}
				
				generalMappingConfig.Add(category,dict);
			}
		}
		
		public static Dictionary<string,NameValueCollection> GetConfigParam(string oldMonitorType)
		{
			if(string.IsNullOrEmpty(oldMonitorType) || null == tempaltesConfigDict
			   || !tempaltesConfigDict.ContainsKey(oldMonitorType))
				return null;
			return tempaltesConfigDict[oldMonitorType];
		}
		
		private static Dictionary<string, string> monitorMappingDict;
		public static string getMappingMonitorType(string oldMonitorType)
		{
			if(monitorMappingDict != null && monitorMappingDict.ContainsKey(oldMonitorType))
				return monitorMappingDict[oldMonitorType];
			return null;
		}
		
		public static string GetMappingValue(string category,string key)
		{
			if(generalMappingConfig.ContainsKey(category))
			{
				Dictionary<string, string> mappingDict = generalMappingConfig[category];
				if(null != mappingDict && mappingDict.ContainsKey(key))
					return mappingDict[key];
			}
			
			return string.Empty;
		}
		#endregion
			
		private static Dictionary<string, List<Counter>> defaultMonitorCounterDicts;
		private static Dictionary<string,Dictionary<string,Counter>> monitorCounterDicts;
		private static Dictionary<string,Dictionary<string,string>> alertCondationDicts;
		private static Dictionary<string, Dictionary<string,NameValueCollection>> tempaltesConfigDict;
	}
}
