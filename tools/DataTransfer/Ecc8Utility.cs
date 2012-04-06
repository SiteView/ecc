////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 14:26
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Collections.Specialized;
using System.Collections.Generic;
using SiteView.Ecc.Core;
using SiteView.Ecc.Core.Models;
using SiteView.Ecc.Core.Dao;
using SiteView.Ecc.WSClient;

namespace DataTransfer
{
	/// <summary>
	/// Description of Ecc8Utility.
	/// </summary>
	public class Ecc8Utility
	{
		public Ecc8Utility(string serviceUrl)
		{
			UserPermissionContext.Instance.Url = serviceUrl;			
			UserPermissionContext.Instance.CurrentUser = new User();
        	UserPermissionContext.Instance.CurrentUser.LoginName = "admin";
		}
		
		#region Public Method
		
		/// <summary>
		/// 解密
		/// </summary>
		/// <param name="str">已加密的字符串</param>
		/// <returns>未加密前的字符串</returns>
		public string DesDecrypt(string str)
		{
			if(string.IsNullOrEmpty(str))
				return string.Empty;
			
			string[] txts = new string[2];
	        txts[0] = "zNz4N71cQ4KNN47O";
	        txts[1] = str; 
	        IDictionary<string, NameValueCollection> valueDict = cryptDao.Decrypt(txts);
			
			return valueDict["return"][txts[1]];
		}
		
		public void ExportEntity(
			out List<GroupEntity> groupList,out List<RemoteServerEntity> remoteServerList
			,out List<MonitorEntity> monitorList)
		{
			groupList = new List<GroupEntity>();
			remoteServerList = new List<RemoteServerEntity>();
			monitorList = new List<MonitorEntity>();
			
			IList<NameValueCollection> dataList = treeDao.GetAll1();
						
			foreach (NameValueCollection data in dataList)
            {
				if(data["type"] == "group") 
				{		
					Group group = groupDao.FindById(data["sv_id"]);
					GroupEntity groupEntity = new GroupEntity(group);
					groupEntity.Parent = getGroupEntityParent(group.ParentId);
					groupList.Add(groupEntity);
					addToEntityBaseDict(groupEntity);
				}				
				
				else if(data["type"] == "entity") 
				{
					Entity entity = entityDao.FindById(data["sv_id"]);
					RemoteServerEntity remoteServerEntity = new RemoteServerEntity(entity);
					remoteServerEntity.Parent = getRemoteServerEntityParent(entity.ParentId);
					remoteServerList.Add(remoteServerEntity);
					addToEntityBaseDict(remoteServerEntity);
				}
				
				else if(data["type"] == "monitor") 
				{
					Monitor monitor = monitorDao.FindById(data["sv_id"]);
					MonitorEntity monitorEntity = new MonitorEntity(monitor);
					monitorEntity.Parent = getMonitorEntityParent(monitor.ParentId);					
					monitorList.Add(monitorEntity);
					addToEntityBaseDict(monitorEntity);
				}	
			}	
		}
	
		/// <summary>
		/// 导出用户相关信息
		/// </summary>
		/// <returns>用户信息列表</returns>
		public List<UserInfo> ExportUserInfo()
		{
			IDictionary<string, NameValueCollection> iniFile = iniDao.GetIniFile("user.ini");
			List<UserInfo> userInfoList = new List<UserInfo>();
			foreach (string section in iniFile.Keys)
	        {
				if("return" == section)
					continue;
				
				UserInfo userInfo = new UserInfo();
				userInfo.ParamCollection = iniFile[section];
				userInfo.Password = DesDecrypt(userInfo.Password);
				userInfoList.Add(userInfo);
			}
			
			return userInfoList;
		}
		
		/// <summary>
		/// 导出任务计划
		/// </summary>
		/// <returns>任务计划列表</returns>
		public List<TaskSchedule> ExportTask()
		{
			TaskDaoImpl taskDaoImpl = new TaskDaoImpl();
			Task[] tasks = taskDaoImpl.FindAll();
			List<TaskSchedule> taskList = new List<TaskSchedule>();
			if(null != tasks)
				foreach(Task task in tasks)
				{
					taskList.Add(new TaskSchedule(task));
				}
			return taskList;
		}
		
		/// <summary>
		/// 导出邮件设置
		/// </summary>
		/// <returns>邮件设置列表</returns>
		public List<EmailSetting> ExportEmailSetting()
		{
			IDictionary<string, NameValueCollection> iniFile = iniDao.GetIniFile("emailAdress.ini");
			List<EmailSetting> emailSettingList = new List<EmailSetting>();
			foreach (string section in iniFile.Keys)
	        {
				if("return" == section)
					continue;
				EmailSetting emailSetting = new EmailSetting(iniFile[section]["Name"],iniFile[section]["MailList"]);
				emailSetting.Section = section;
				emailSetting.Schedule = iniFile[section]["Schedule"];
				emailSetting.Template = "Default";
				emailSettingList.Add(emailSetting);
			}
			
			return emailSettingList;
		}
		
		/// <summary>
		/// 导出告警规则
		/// </summary>
		/// <returns>告警规则列表</returns>
		public List<AlertRule> ExportAlertRule()
		{
			exportSmsSetting();
			IDictionary<string, NameValueCollection> iniFile = iniDao.GetIniFile("alert.ini");
	        //ExportSmsSetting();
			List<AlertRule> alertRuleList = new List<AlertRule>();
			foreach(string section in iniFile.Keys)
			{
				if("return".Equals(section))
					continue;
				//cout("开始导出section: " + section + " ,Name: " + iniFile[section]["AlertName"]);
				AlertRule rule = buildAlertRuleFromIni(iniFile[section]);
				if(null != rule) 
				{
					rule.Section = section;
					rule.Template = "Default";
					alertRuleList.Add(rule);
				}
			}
			return alertRuleList;
		}
		
		#endregion
		
		#region Private Method
		private EntityBase getGroupEntityParent(string groupParentId)
		{
			if(string.IsNullOrEmpty(groupParentId) || "1" == groupParentId)
				return null;
			
			if(entityBaseDict.ContainsKey(groupParentId))
				return entityBaseDict[groupParentId];
			
			Group groupParent = groupDao.FindById(groupParentId);
			if(groupParent != null)
			{
				GroupEntity parent = new GroupEntity(groupParent);
				addToEntityBaseDict(parent);
				return parent;
			}
			
			return null;			
		}
		
		private EntityBase getRemoteServerEntityParent(string entityParentId)
		{
			if(string.IsNullOrEmpty(entityParentId))
				return null;
			
			if(entityBaseDict.ContainsKey(entityParentId))
				return entityBaseDict[entityParentId];
			
			Group entityParent = groupDao.FindById(entityParentId);
			if(entityParent != null)
			{
				GroupEntity parent = new GroupEntity(entityParent);
				addToEntityBaseDict(parent);
				return parent;
			}
			
			return null;
		}
		
		private EntityBase getMonitorEntityParent(string monitorParentId)
		{
			if(string.IsNullOrEmpty(monitorParentId))
				return null;
			
			if(entityBaseDict.ContainsKey(monitorParentId))
				return entityBaseDict[monitorParentId];
			
			Entity monitorParent = entityDao.FindById(monitorParentId);
			if(monitorParent != null)
			{
				RemoteServerEntity parent = new RemoteServerEntity(monitorParent);
				addToEntityBaseDict(parent);
				return parent;
			}
			
			return null;
		}
		
		private void addToEntityBaseDict(EntityBase entityBase)
		{
			if(null != entityBase)
				entityBaseDict.Add(entityBase.OldId,entityBase);
		}
		
		private AlertRule buildAlertRuleFromIni(NameValueCollection sectionKeyValues)
		{
			string alertType = sectionKeyValues["AlertType"];
					
			if("EmailAlert" == alertType)
			{
				string emailAddress = sectionKeyValues["EmailAdress"]; //email or 其他
				string otherEmailAddress = sectionKeyValues["OtherAdress"];
				EmailAlertRule rule = new EmailAlertRule();
				setAlertRuleCommonProperty(rule,sectionKeyValues);
				rule.EmailAddress = emailAddress;
				rule.OtherEmailAddress = otherEmailAddress;
				return rule;
			}
			
			if("SmsAlert" == alertType)
			{
				string smsNumber = sectionKeyValues["SmsNumber"];
				string otherNumber = sectionKeyValues["OtherNumber"];
				string mode = sectionKeyValues["SmsSendMode"];
				SmsAlertRule rule = new SmsAlertRule();
				setAlertRuleCommonProperty(rule,sectionKeyValues);
				if("web".Equals(mode,StringComparison.OrdinalIgnoreCase))
				{
					rule.SmsSendMode = SmsSendMode.Web;
				}
				if("com".Equals(mode,StringComparison.OrdinalIgnoreCase))
				{
					rule.SmsSendMode = SmsSendMode.GSM;
				}
				if(!string.IsNullOrEmpty(mode) && mode.ToLower().EndsWith("dll"))
				{
					rule.SmsSendMode = SmsSendMode.DLL;
				}
				
				if("其他" == smsNumber)
					rule.PhoneNumber = otherNumber;
				else
					rule.PhoneNumber = exportedSmsSettingDict[smsNumber];
				return rule;
			}
		
			if("ScriptAlert" == alertType)
			{
				string server = sectionKeyValues["ScriptServer"];
				string script = sectionKeyValues["ScriptFile"];
	            string param = sectionKeyValues["ScriptParam"];
				ScriptAlertRule rule = new ScriptAlertRule();
				setAlertRuleCommonProperty(rule,sectionKeyValues);
				rule.Server = server;
				rule.Script = script;
				rule.Param = param;
				
				return rule;
			}
		
			if("SoundAlert" == alertType)
			{
				string sound = "Default";
				SoundAlertRule rule = new SoundAlertRule();
				setAlertRuleCommonProperty(rule,sectionKeyValues);
				rule.Sound = sound;
				
				return rule;
			}
			
			throw null;
		}
		
		private void setAlertRuleCommonProperty(AlertRule rule,NameValueCollection sectionKeyValues)
		{
			if(null != rule)
			{
				string name = sectionKeyValues["AlertName"];
				string state = sectionKeyValues["AlertState"];
				string category = sectionKeyValues["AlertCategory"];
				string targets = sectionKeyValues["AlertTarget"];
				
				string alwayTimes = sectionKeyValues["AlwaysTimes"];
				string onlyTimes = sectionKeyValues["OnlyTimes"];
				string selTimes1 = sectionKeyValues["SelTimes1"];
				string selTimes2 = sectionKeyValues["SelTimes2"];
				string alertCond = sectionKeyValues["AlertCond"];
				
				rule.Name = name;
				rule.Enable = string.Compare(state,"enable",true) == 0;
				//正常 危险 错误
				//good warning error
				if("错误" == category)
					rule.Category = "error";  
				else if ("危险" == category)
					rule.Category = "warning";
				else 
					rule.Category = "good";
				if(!string.IsNullOrEmpty(targets))
				{
					rule.Targets = targets.Split(new char[] {','},StringSplitOptions.RemoveEmptyEntries);
				}
				rule.AlwayTimes = alwayTimes;
				rule.OnlyTimes = onlyTimes;
				rule.SelTimes1 = selTimes1;
				rule.SelTimes2 = selTimes2;
				rule.AlertCond = alertCond;
			}
		}
		
		private void exportSmsSetting()
		{
			IniFileDaoImpl iniDao = new IniFileDaoImpl();
			IDictionary<string, NameValueCollection> smsSettingFile = iniDao.GetIniFile("smsphoneset.ini");
			if(null != smsSettingFile)
			{
				foreach(string section in smsSettingFile.Keys)
				{
					if("return".Equals(section))
						continue;
					exportedSmsSettingDict.Add(smsSettingFile[section]["Name"],smsSettingFile[section]["Phone"]);
				}
			}
		}
		#endregion
	
		#region Private Property
		private Dictionary<string, EntityBase> entityBaseDict = new Dictionary<string, EntityBase>();
		private NameValueCollection exportedSmsSettingDict = new NameValueCollection();
		private MonitorDaoImpl _monitorDao;
		private MonitorDaoImpl monitorDao
		{
			get
			{
				if(null == _monitorDao)
					_monitorDao = new MonitorDaoImpl();
				return _monitorDao;
			}
		}
			
		private EntityDaoImpl _entityDao;
		private EntityDaoImpl entityDao
		{
			get
			{
				if(null == _entityDao)
					_entityDao = new EntityDaoImpl();
				return _entityDao;
			}
		}
		
		private GroupDaoImpl _groupDao;
		private GroupDaoImpl groupDao
		{
			get
			{
				if(null == _groupDao)
					_groupDao = new GroupDaoImpl();
				return _groupDao;
			}
		}
		
		private TreeDaoImpl _treeDao;
		private TreeDaoImpl treeDao
		{
			get
			{
				if(null == _treeDao)
					_treeDao = new TreeDaoImpl();
				return _treeDao;
			}
		}
		
		private TaskDaoImpl _taskDao;
		private TaskDaoImpl taskDao
		{
			get
			{
				if(null == _taskDao)
					_taskDao = new TaskDaoImpl();
				return _taskDao;
			}
		}
		
		private IniFileDaoImpl _iniDao;
		private IniFileDaoImpl iniDao
		{
			get
			{
				if(null == _iniDao)
					_iniDao = new IniFileDaoImpl();
				return _iniDao;
			}
		}
		
		private CryptDaoImpl _cryptDao;
		private CryptDaoImpl cryptDao
		{
			get
			{
				if(null == _cryptDao)
					_cryptDao = new CryptDaoImpl();
				return _cryptDao;
			}
		}		
		#endregion
	}	
}
