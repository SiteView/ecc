////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 14:47
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Collections.Specialized;
using System.Collections.Generic;

namespace DataTransfer
{
	/// <summary>
	/// Description of UserInfo.
	/// </summary>
	public class UserInfo
	{
		private string id;
		private string name;
		private string password;
		private string ldapServer;
		private string ldapSecurity;
		private string title;
		private string desc;
		private bool disabled;
		private bool importable;
		private List<string> nodeRights = new List<string>();
		private List<string> cpes = new List<string>();
		private List<string> nt = new List<string>();
		private List<string> unix = new List<string>();
		private List<string> groups = new List<string>();
		private List<string> device = new List<string>();
		private Dictionary<string,List<string>> nodeDetailsRight = new Dictionary<string,List<string>>();
		
		private bool isExtracted;
		private void extract()
		{
			if(!isExtracted)
			{
				isExtracted = true;
				if(ParamCollection == null || isAdmin)
				{
					importable = false;
					return;
				}	
				
				importable = true;
				
				id = ParamCollection["LoginName"];
				name = ParamCollection["LoginName"];
				password = ParamCollection["Password"];
				ldapServer = ParamCollection["LDAPProviderUrl"];
				ldapSecurity = ParamCollection["LDAPSecurityPrincipal"];
				title = ParamCollection["UserName"];
				desc = ParamCollection["UserName"];
				disabled = ParamCollection["nIsUse"] != "1";
				
				Dictionary<string, string> groupRightMap = new Dictionary<string, string>();
				groupRightMap.Add("editgroup","groupEdit");
				groupRightMap.Add("grouprefresh","groupRefresh");
				groupRightMap.Add("addsongroup","addsongroup");
				groupRightMap.Add("delgroup","groupDel");
				groupRightMap.Add("editmonitor","monitorEdit");
				groupRightMap.Add("addmonitor","addmonitor");
				groupRightMap.Add("delmonitor","monitorDelete");
				groupRightMap.Add("monitorrefresh","monitorRefresh");
				
				Dictionary<string,string> moduleRightMap = new Dictionary<string,string>();
				moduleRightMap.Add("m_allview","WholeView"); //整体视图
				moduleRightMap.Add("m_monitorDisplay","MonitorBrower"); //监测器浏览
				//moduleRightMap.Add("","RemoteSever"); //远程服务器
				//moduleRightMap.Add("","MonitorProxy"); //监测器节点
				moduleRightMap.Add("m_tuop","TopoView"); //Visio图
				//moduleRightMap.Add("","Alert"); //报警
				moduleRightMap.Add("m_AlertRule","AlertRule"); //报警规则
				moduleRightMap.Add("m_alertLogs","AlertLog"); //报警日志
				//moduleRightMap.Add("","Report"); //报表
				moduleRightMap.Add("m_StatisticReport","ReportStatistic"); //统计报告
				moduleRightMap.Add("m_TrendReport","ReportTrend"); //趋势报告
				moduleRightMap.Add("m_topnadd","ReportTopN"); //TopN报告
				moduleRightMap.Add("m_ShowStatusReport","ReportStatus"); //状态统计报告
				moduleRightMap.Add("m_ContrastReport","ReportContrast"); //对比报告
				moduleRightMap.Add("m_TimeContrastReport","ReportTimeContrast"); //时段对比报告
				moduleRightMap.Add("m_logshower","MonitorInfo"); //监测报告信息
				moduleRightMap.Add("m_syslogquery","SysLogQuery"); //SysLog查询
				//moduleRightMap.Add("","Set"); //设置
				moduleRightMap.Add("m_general","SetGeneral"); //基本设置
				moduleRightMap.Add("m_mailsetting","SetMail"); //邮件设置
				moduleRightMap.Add("m_smssetting","SetSms"); //短信设置
				moduleRightMap.Add("m_maintainsetting","SetMaintain"); //值班表设置
				moduleRightMap.Add("m_usermanager","UserManager"); //用户管理
				//moduleRightMap.Add("","Task"); //任务计划
				moduleRightMap.Add("m_taskabsolute","TaskAbsolute"); //绝对时间任务计划
				moduleRightMap.Add("m_taskperiod","TaskPeriod"); //时间段任务计划
				moduleRightMap.Add("m_taskrelative","TaskRelative"); //相对时间任务计划
				moduleRightMap.Add("m_syslogset","SysLogSet"); //SysLog设置
				moduleRightMap.Add("m_operatelog","OperateLog"); //用户操作日志
				moduleRightMap.Add("m_system_diagnosis","SystemDiagnosis"); //系统诊断
				//moduleRightMap.Add("","License"); //软件许可
				//moduleRightMap.Add("","About"); //产品介绍
				
				Dictionary<string,Dictionary<string,string>> moduleDetailsRightMap = new Dictionary<string,Dictionary<string,string>>();
				
				//告警规则
				Dictionary<string,string> alertRuleDetailsRight = new Dictionary<string,string>();
				alertRuleDetailsRight.Add("m_AlertRuleAdd","m_AlertRuleAdd");
				alertRuleDetailsRight.Add("m_AlertRuleDel","m_AlertRuleDel");
				alertRuleDetailsRight.Add("m_AlertRuleEdit","m_AlertRuleEdit");
				
				//统计报告
				Dictionary<string,string> reportDetailsRight = new Dictionary<string,string>();
				reportDetailsRight.Add("m_statisticReportlistAdd","m_statisticReportlistAdd");
				reportDetailsRight.Add("m_statisticReportlistDel","m_statisticReportlistDel");
				reportDetailsRight.Add("m_statisticReportlistEdit","m_statisticReportlistEdit");
				
				//TopN报告
				Dictionary<string,string> topnReportDetailsRight = new Dictionary<string,string>();
				reportDetailsRight.Add("m_topNReportlistAdd","m_topNReportlistAdd");
				reportDetailsRight.Add("m_topNReportlistDel","m_topNReportlistDel");
				reportDetailsRight.Add("m_topNReportlistEdit","m_topNReportlistEdit");
				
				moduleDetailsRightMap.Add("AlertRule",alertRuleDetailsRight);
				moduleDetailsRightMap.Add("Report",reportDetailsRight);
				moduleDetailsRightMap.Add("ReportTopN",topnReportDetailsRight);
 		
				Dictionary<string,List<string>> moduleChildMap = new Dictionary<string,List<string>>();
				List<string> alertChildModule = new List<string>();
				alertChildModule.Add("m_AlertRule");
				alertChildModule.Add("m_alertLogs");
				moduleChildMap.Add("Alert",alertChildModule);
				List<string> reportChildModule = new List<string>();
				reportChildModule.Add("m_StatisticReport");
				reportChildModule.Add("m_TrendReport");
				reportChildModule.Add("m_topnadd");
				reportChildModule.Add("m_ShowStatusReport");
				reportChildModule.Add("m_ContrastReport");
				reportChildModule.Add("m_TimeContrastReport");
				reportChildModule.Add("m_logshower");
				moduleChildMap.Add("Report",reportChildModule);
				
				List<string> setChildModule = new List<string>();
				setChildModule.Add("m_general");
				setChildModule.Add("m_mailsetting");
				setChildModule.Add("m_smssetting");
				setChildModule.Add("m_maintainsetting");
				setChildModule.Add("m_usermanager");
				moduleChildMap.Add("Set",setChildModule);
				
				List<string> taskChildModule = new List<string>();
				taskChildModule.Add("m_taskabsolute");
				taskChildModule.Add("m_taskperiod");
				taskChildModule.Add("m_taskrelative");
				moduleChildMap.Add("Task",taskChildModule);
				
				
				/*
				Dictionary<string,string> groupRightMap = ConfigUtility.GetMappingDict("GroupRight");
				Dictionary<string, string> moduleRightDict = ConfigUtility.GetMappingDict("ModuleRight");
				Dictionary<string, string> moduleChildRightDict = ConfigUtility.GetMappingDict("ChildRight");				
				*/
				
				foreach(string key in ParamCollection.AllKeys)
				{
					//Console.WriteLine("Key is " + key);
					bool isEntity = System.Text.RegularExpressions.Regex.IsMatch(key, @"^(?:\d+\.?)+");
					if(isEntity)
					{
						string rightStr = ParamCollection[key];
						//Root
						//if(rightStr.Contains("se_addse"))
							//continue;
						
						//Root,Group
						if(rightStr.Contains("addsongroup"))
						{
							//Console.WriteLine("Key is a Group ID");
							
							nodeRights.Add(key);
							groups.Add(key);
							
							List<string> groupRight = new List<string>();
							foreach(string rightKey in groupRightMap.Keys)
							{
								if(rightStr.Contains(string.Format("{0}=1",rightKey)))
									groupRight.Add(groupRightMap[rightKey]);
							}
							nodeDetailsRight.Add(key,groupRight);
						}
						
						//Device
						//if(ParamCollection[key].Contains("adddevice"))
						//{	
						//	device.Add(key);
						//}	
						
						continue;
					}
					
					/*
					//模块权限
					if(moduleRightDict.ContainsKey(key))
					{
						nodeRights.Add(moduleRightDict[key]);
						continue;
					}
					
					//子模块权限
					if(moduleChildRightDict.ContainsKey(key))
					{
						//找到上级模块
						string parent = getParentModule(key);
						//上级模块的子权限已经处理没有处理
						string mappingParent = moduleChildRightDict[parent];
						if(nodeDetailsRight.ContainsKey(mappingParent))
							continue;
					    //获取该上级模块下的所有子权限映射
					    
					    List<string> childModule = getChildModule(parent);
					    List<string> detailsRight = new List<string>();
					    if(null == childModule)
					    	continue;
					    foreach(string child in childModule)
					    {
					    	if(ParamCollection[key] == "1")
					    		detailsRight.Add(moduleChildRightDict[child]);
					    }
						nodeDetailsRight.Add(mappingParent,detailsRight);
						continue;
					}
					*/
					
					//模块节点权限
					if(moduleRightMap.ContainsKey(key) && ParamCollection[key] == "1")
					{												
						nodeRights.Add(moduleRightMap[key]);
						
						//是否为子节点，是则将父节点计入权限
						foreach(string module in moduleChildMap.Keys)
						{
							if( moduleChildMap[module].Contains(key) && nodeRights.Contains(module) == false)
							{
								nodeRights.Add(module);
							}
						}
						
						//模块节点详细权限
						if(!moduleDetailsRightMap.ContainsKey(moduleRightMap[key]))
							continue;
						
						Dictionary<string,string> moduleDetailsRight = moduleDetailsRightMap[moduleRightMap[key]];
						if(null != moduleDetailsRight)
						{
							List<string> detailsRight = new List<string>();
							foreach(string oldKey in moduleDetailsRight.Keys)
							{
								if(ParamCollection[key] == "1")
									detailsRight.Add(moduleDetailsRight[oldKey]);
							}
							
							nodeDetailsRight.Add(moduleRightMap[key],detailsRight);
						}
					}
					
				}				
			}
		}
		
		private string getParentModule(string child)
		{
			return ConfigUtility.GetMappingValue("Child2Module",child);
		}
		
		private List<string> getChildModule(string module)
		{
			Dictionary<string, string> child2ModuleDict = ConfigUtility.GetMappingDict("Child2Module");
			if(null == child2ModuleDict)
				return null;
			
			List<string> childs = new List<string>();
			foreach(string child in child2ModuleDict.Keys)
			{
				if(child2ModuleDict[child] == module)
					childs.Add(child);
			}
			
			return childs;
		}
		
		private bool isAdmin
		{
			get
			{
				name = ParamCollection["LoginName"];
				return ParamCollection["nIndex"] != null && ParamCollection["nIndex"] == "1";
			}
		}
						
		public string Id
		{
			get {return id;}
			//set {id = value;}
		}
		public string Name
		{
			get 
			{
				extract();
				return name;
			}
			//set {name = value;}
		}
		public string Password
		{
			get 
			{
				extract();
				return password;
			}
			set {password = value;}
		}
		public string LdapServer
		{
			get 
			{
				extract();
				return ldapServer;
			}
			//set {ldapServer = value;}
		}
		public string LdapSecurity
		{
			get 
			{
				extract();
				return ldapSecurity;
			}
			//set {ldapSecurity = value;}
		}
		public string Title
		{
			get 
			{
				extract();
				return title;
			}
			//set {title = value;}
		}
		public string Desc
		{
			get 
			{
				extract();
				return desc;
			}
			//set {desc = value;}
		}
		public bool Disabled
		{
			get 
			{
				extract();
				return disabled;
			}
			//set {disabled = value;}
		}		
		public bool Importable
		{
			get 
			{
				extract();
				return importable;
			}
			//set {importable = value;}
		}
		
		public List<string> NodeRights
		{
			get 
			{
				extract();
				return nodeRights;
			}
			//set {nodeRights = value;}
		}
		public List<string> Cpes
		{
			get 
			{
				extract();
				return cpes;
			}
			//set {cpes = value;}
		}
		public List<string> NT
		{
			get 
			{
				extract();
				return nt;
			}
			//set {nt = value;}
		}
		public List<string> Unix
		{
			get 
			{
				extract();
				return unix;
			}
			//set {unix = value;}
		}
		public List<string> Groups
		{
			get 
			{
				extract();
				return groups;
			}
			//set {groups = value;}
		}
		public List<string> Device
		{
			get 
			{
				extract();
				return device;
			}
		}
		public Dictionary<string,List<string>> NodeDetailsRight
		{
			get 
			{
				extract();
				return nodeDetailsRight;
			}
			//set {nodeDetailsRight = value;}
		}
		
		public NameValueCollection ParamCollection;
	}
}
