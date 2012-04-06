////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 14:26
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using Otp;

namespace DataTransfer
{
	/// <summary>
	/// Description of Ecc9Utility.
	/// </summary>
	public class Ecc9Utility
	{	
		public const string LOG_CATEGORY_GROUP = "group";
		public const string LOG_CATEGORY_REMOTESERVER = "entity";
		public const string LOG_CATEGORY_MONITOR = "monitor";
		public const string LOG_CATEGORY_TASK = "task";
		public const string LOG_CATEGORY_EMAILSETTING = "email";
		public const string LOG_CATEGORY_USERINFO = "user";
		public const string LOG_CATEGORY_ALERTRULE = "alert_rule";

		public const string LOG_KEY_IMPORT_SUCCESS = "S";
		public const string LOG_KEY_IMPORT_FAILURE = "F";
		public const string LOG_KEY_UPDATE_SUCCESS = "US";
		public const string LOG_KEY_UPDATE_FAILURE = "UF";
		public const string LOG_KEY_OTHER = "O";
			
		public Ecc9Utility(string nodeName,string cookieName)
		{
			OtpSelf node = new OtpSelf("clientnode", cookieName);			
			OtpPeer peer = new OtpPeer(nodeName);			
			connection = node.connect(peer);
		}
		
		#region Public Method	
		public NameValueCollection GetLog(string logCategory)
		{
			if(this.Logs.ContainsKey(logCategory))
				return Logs[logCategory];
			
			return null;
		}
		
		/// <summary>
		/// 导入组
		/// </summary>
		/// <param name="groups">组列表</param>
		public void ImportGroup(IEnumerable<GroupEntity> groups)
		{
			foreach(GroupEntity group in groups)
			{
				try
				{
					import(group);
				}
				catch(Exception err)
				{
					addToLog(LOG_CATEGORY_GROUP,LOG_KEY_IMPORT_FAILURE
							 ,string.Format("导入ID为 {0} 节点为 {1} 的组出现异常：{2}",group.OldId,group.Path,err.ToString()));
				}
			}
		}
		
		/// <summary>
		/// 导入远程服务器
		/// </summary>
		/// <param name="remoteServers">远程服务器列表</param>
		public void ImportRemoteServer(IEnumerable<RemoteServerEntity> remoteServers)
		{
			foreach(RemoteServerEntity remoteServer in remoteServers)
			{
				try
				{
					import(remoteServer);
				}
				catch(Exception err)
				{
					addToLog(LOG_CATEGORY_REMOTESERVER,LOG_KEY_IMPORT_FAILURE
							 ,string.Format("导入ID为 {0} 节点为 {1} 的远程服务器出现异常：{2}",remoteServer.OldId,remoteServer.Path,err.ToString()));
				}
			}
		}		
		
		/// <summary>
		/// 导入监测器
		/// </summary>
		/// <param name="monitors">监测器列表</param>
		public void ImportMonitor(IEnumerable<MonitorEntity> monitors)
		{
			foreach(MonitorEntity monitor in monitors)
			{
				try
				{
					import(monitor);
				}
				catch(Exception err)
				{
					addToLog(LOG_CATEGORY_MONITOR,LOG_KEY_IMPORT_FAILURE
							 ,string.Format("导入ID为 {0} 节点为 {1} 的监测器出现异常：{2}",monitor.OldId,monitor.Path,err.ToString()));
				}
			}
		}
		
		public void UpdateDependOn()
		{
			updateGroupDependOn();
		}
		
		/// <summary>
		/// 导入任务计划
		/// </summary>
		/// <param name="tasks">任务计划列表</param>
		public void ImportTask(IEnumerable<TaskSchedule> tasks)
		{	
			foreach(TaskSchedule task in tasks)
			{
				try
				{
					import(task);
				}
				catch(Exception err)
				{
					addToLog(LOG_CATEGORY_TASK,LOG_KEY_IMPORT_FAILURE
							 ,string.Format("导入ID为 {0} 的任务计划出现异常：{1}",task.Name,err.ToString()));
				}
			}
		}
		
		/// <summary>
		/// 导入邮件设置
		/// </summary>
		/// <param name="settings">邮件设置列表</param>
		public void ImportEmailSetting(IEnumerable<EmailSetting> settings)
		{
			foreach(EmailSetting setting in settings)
			{
				try
				{
					import(setting);
				}
				catch(Exception err)
				{
					addToLog(LOG_CATEGORY_EMAILSETTING,LOG_KEY_IMPORT_FAILURE
							 ,string.Format("导入名称为 {0} 出现异常：{1}",setting.Name,err.ToString()));
				}
			}
		}
		
		/// <summary>
		/// 导入用户信息
		/// </summary>
		/// <param name="users">用户信息列表</param>
		public void ImportUserInfo(IEnumerable<UserInfo> users)
		{
			foreach(UserInfo user in users)
			{
				try
				{
					import(user);
				}
				catch(Exception err)
				{
					addToLog(LOG_CATEGORY_USERINFO,LOG_KEY_IMPORT_FAILURE
							 ,string.Format("导入名称为 {0} 的用户出现异常：{1}",user.Name,err.ToString()));
				}
			}
		}
		
		/// <summary>
		/// 导入告警规则
		/// </summary>
		/// <param name="rules">告警规则列表</param>
		public void ImportAlertRule(IEnumerable<AlertRule> rules)
		{
			foreach(AlertRule rule in rules)
			{
				try
				{
					import(rule);
				}
				catch(NotSupportedException)
				{
					addToLog(LOG_CATEGORY_ALERTRULE,LOG_KEY_OTHER
					         ,string.Format("名称为 {0} 的告警规则没有合适的导入参数，未导入",rule.Name));
				}
				catch(Exception err)
				{
					addToLog(LOG_CATEGORY_ALERTRULE,LOG_KEY_IMPORT_FAILURE
							 ,string.Format("导入名称为 {0} 的告警规则出现异常：{1}",rule.Name,err.ToString()));
				}
			}
		}
		
		#endregion
		
		#region Public Property
		private Dictionary<string, NameValueCollection> logs;
		public Dictionary<string, NameValueCollection> Logs
		{
			get
			{
				if(null == logs)
					logs = new Dictionary<string, NameValueCollection>();
				return logs;				
			}
		}
		#endregion
		
		#region Private Method
		private void import(GroupEntity groupEntity)
		{						
			connection.sendRPC("api_group", "create", groupEntity.GetImportParam());
			Otp.Erlang.Object temp = connection.receiveRPC();
			Otp.Erlang.Object[] erlObj = ((Otp.Erlang.Tuple)temp).elements();
			bool result = erlObj !=null && erlObj.Length == 2 && String.Compare(erlObj[0].ToString(),"ok",true) == 0;			
			if(result)
			{
				ErlangEntity erlEntity =  new ErlangEntity(erlObj[1]);
				string newID = erlEntity.GetPropertyStr("id");
									
				groupEntity.NewId = newID;
				importedGroups.Add(groupEntity.OldId,groupEntity);
				addToLog(LOG_CATEGORY_GROUP,LOG_KEY_IMPORT_SUCCESS
						 ,string.Format("导入ID为 {0} 节点为 {1} 的组成功,新ID为 {2}",groupEntity.OldId,groupEntity.Path, groupEntity.NewId));
				
			}
			else
			{
				addToLog(LOG_CATEGORY_GROUP,LOG_KEY_IMPORT_FAILURE
						 ,string.Format("导入ID为 {0} 节点为 {1} 的组失败,原因: {2}",groupEntity.OldId,groupEntity.Path, erlObj[1]));
			}
		}
		
		private void import(RemoteServerEntity remoteServerEntity)
		{
			string deviceType = remoteServerEntity.DeviceType;		
		
			if(string.IsNullOrEmpty(ConfigUtility.GetDeviceTypeMapping(deviceType)))
			{
				addToLog(LOG_CATEGORY_REMOTESERVER,LOG_KEY_OTHER
						 ,string.Format("ID为 {0} 节点为 {1} 的设备类型为 {2} 没有对应的远程服务操作系统，未导入",remoteServerEntity.OldId,remoteServerEntity.Path,remoteServerEntity.DeviceType));
			}
			else
			{
				connection.sendRPC("api_machine", "create_machine", remoteServerEntity.GetImportParam());
				Otp.Erlang.Object temp = connection.receiveRPC();
				Otp.Erlang.Object[] erlObj = ((Otp.Erlang.Tuple)temp).elements();
				bool result = erlObj !=null && erlObj.Length == 2 && String.Compare(erlObj[0].ToString(),"ok",true) == 0;			
				if(result)
				{				
					string newID = ErlangEntity.TrimSpecialChar(erlObj[1].ToString());
					
					remoteServerEntity.NewId = newID;
					
					addToLog(LOG_CATEGORY_REMOTESERVER,LOG_KEY_IMPORT_SUCCESS
							 ,string.Format("导入ID为 {0} 节点为 {1} 的设备成功,新ID为 {2}",remoteServerEntity.OldId,remoteServerEntity.Path, remoteServerEntity.NewId));
				}
				else
				{
					addToLog(LOG_CATEGORY_REMOTESERVER,LOG_KEY_IMPORT_FAILURE
							 ,string.Format("导入ID为 {0} 节点为 {1} 的设备失败,原因: {2}",remoteServerEntity.OldId,remoteServerEntity.Path,erlObj[1]));
				}
			}
			
		}
				
		private void import(MonitorEntity monitorEntity)
		{	
			string mappingMonitorType = 
				ConfigUtility.getMappingMonitorType(monitorEntity.OldMonitorType);
			if(string.IsNullOrEmpty(mappingMonitorType))
			{
				addToLog(LOG_CATEGORY_MONITOR,LOG_KEY_OTHER
						 ,string.Format("ID为 {0} 节点为 {1} 的监测器没有可用于类型为 {2} 的模板",monitorEntity.OldId,monitorEntity.Path,monitorEntity.OldMonitorType));
				return;
			}
			
			monitorEntity.NewMonitorType = mappingMonitorType;
			connection.sendRPC("api_monitor", "create", monitorEntity.GetImportParam());
			Otp.Erlang.Object temp = connection.receiveRPC();
			
			Otp.Erlang.Object[] erlObj = ((Otp.Erlang.Tuple)temp).elements();
			bool result = erlObj !=null && erlObj.Length == 2 && String.Compare(erlObj[0].ToString(),"ok",true) == 0;			
			if(result)
			{			
				ErlangEntity erlEntity =  new ErlangEntity(erlObj[1]);
				string newId = erlEntity.GetPropertyStr("id");			
				monitorEntity.NewId = newId;
				
				importedMinitors.Add(monitorEntity.OldId,monitorEntity);
				addToLog(LOG_CATEGORY_MONITOR,LOG_KEY_IMPORT_SUCCESS
						 ,string.Format("导入ID为 {0} 节点为 {1} 的监测器成功，新ID为 {2}",monitorEntity.OldId,monitorEntity.Path,monitorEntity.NewId));
			}
			else
			{
				addToLog(LOG_CATEGORY_MONITOR,LOG_KEY_IMPORT_FAILURE
						 ,string.Format("导入ID为 {0} 节点为 {1} 的监测器失败,原因: {2}",monitorEntity.OldId,monitorEntity.Path,erlObj[1]));
			}		
		}
		
		private Otp.Erlang.List buildListParam(IList<string> list)
		{
			if(null == list)
				return new Otp.Erlang.List();
			
			int count = list.Count;
			Otp.Erlang.Object[] param = new Otp.Erlang.Object[count];
			for(int i = 0; i < count; i++)
			{
				param[i] = new Otp.Erlang.String(list[i]);
			}
			
			return new Otp.Erlang.List(param);
		}
		
		private List<string> buildMap(IList<string> list)
		{
			if(null == list)
				return null;
			
			List<string> result = new List<string>();
			foreach(string key in list)
			{
				//模块
				bool isEntity = System.Text.RegularExpressions.Regex.IsMatch(key, @"^(?:\d+\.?)+");
				if(!isEntity)
				{
					result.Add(key);
					continue;
				}
				//组
				if(importedGroups.ContainsKey(key))								
					result.Add(importedGroups[key].NewId);
			}
			return result;
		}
		
		private List<string> buildMapGroups(IList<string> list)
		{
			if(null == list)
				return null;
			
			List<string> result = new List<string>();
			foreach(string oldId in list)
			{		
				if(importedGroups.ContainsKey(oldId))								
					result.Add(importedGroups[oldId].NewId);
			}
			
			return result;
		}
		
		private void import(UserInfo user)
		{
			if(!user.Importable)
			{
				addToLog(LOG_CATEGORY_USERINFO,LOG_KEY_OTHER
				         ,string.Format("名称为 {0} 的用户信息不在导入范围，未导入",user.Name));
				return;
			}
			
			Otp.Erlang.Object[] parames = new Otp.Erlang.Object[] {
				new Otp.Erlang.String(user.Name)
				,new Otp.Erlang.String(user.Password)
				,new Otp.Erlang.String(user.LdapServer)
				,new Otp.Erlang.String(user.LdapSecurity)
				,new Otp.Erlang.String(user.Desc)
				,new Otp.Erlang.String(user.Title)
				,new Otp.Erlang.String(user.Disabled.ToString().ToLower())				
				,buildListParam(buildMap(user.NodeRights))
				,new Otp.Erlang.List() //,buildListParam(buildMapGroups(user.Groups))
				,buildListParam(user.Cpes)
				,buildListParam(user.NT)
				,buildListParam(user.Unix)
			};
			connection.sendRPC("api_user_spl", "user_create", parames);
			Otp.Erlang.Object temp = connection.receiveRPC();
			
			if(ErlangEntity.IsOk(temp))
			{
				addToLog(LOG_CATEGORY_USERINFO,LOG_KEY_IMPORT_SUCCESS
				         ,string.Format("名称为 {0} 的用户及节点权限导入成功",user.Name));
				
				foreach(string key in user.NodeDetailsRight.Keys)
				{
					try
					{	
						string nodeName = "";
						if(importedGroups.ContainsKey(key))
							nodeName = importedGroups[key].NewId;
						parames = new Otp.Erlang.Object[] {
							new Otp.Erlang.String(user.Name)
							,new Otp.Erlang.String(nodeName)
							,buildListParam(user.NodeDetailsRight[key])
						};
						
						connection.sendRPC("api_user_spl", "add_right", parames);
						Otp.Erlang.Object temp2 = connection.receiveRPC();	
						
						if(ErlangEntity.IsOk(temp2))
						{
							addToLog(LOG_CATEGORY_USERINFO,LOG_KEY_UPDATE_SUCCESS
							         ,string.Format("名称为 {0} 节点为 {1} 的用户节点详细权限导入成功",user.Name,key));
						}
						else
						{
							addToLog(LOG_CATEGORY_USERINFO,LOG_KEY_UPDATE_FAILURE
							         ,string.Format("名称为 {0} 节点为 {1} 的用户节点详细权限导入失败",user.Name,key));
						}
					}
					catch(Exception err)
					{
						addToLog(LOG_CATEGORY_USERINFO,LOG_KEY_UPDATE_FAILURE
						         ,string.Format("名称为 {0} 节点为 {1} 的用户节点详细权限导入失败,原因: {2}",user.Name,key,err));
					}
				}
			}
			else
			{
				addToLog(LOG_CATEGORY_USERINFO,LOG_KEY_IMPORT_FAILURE
				         ,string.Format("名称为 {0} 的用户及节点权限导入失败",user.Name));
			}
		}
		
		private string getAlertTarget(string[] targets)
		{
			string result = string.Empty;
			
			if(null != targets)
			{
				foreach(string target in targets)
				{
					if(!importedMinitors.ContainsKey(target))
						continue;
					string id = importedMinitors[target].NewId;
					result += string.Format(",<{0}>",id);
				}
			}
			if(string.Empty != result)
				return result.Substring(1);
			return result;
		}
		
		private void import(AlertRule rule)
		{
			string target = getAlertTarget(rule.Targets);
			if(string.IsNullOrEmpty(target))
			{
				addToLog(LOG_CATEGORY_ALERTRULE,LOG_KEY_OTHER
				         ,string.Format("Section为 {0} 名称为 {1} 的告警规则没有匹配的目标，未导入未。",rule.Section,rule.Name));
				return;
			}
			
			if(rule is EmailAlertRule)
			{
				EmailAlertRule emailAlertRule = ((EmailAlertRule)rule);
				if("其他" == emailAlertRule.EmailAddress)
					emailAlertRule.EmailAddress = "other";
				else
					emailAlertRule.EmailAddress = importedEmailSettings[emailAlertRule.EmailAddress];
			}
			Otp.Erlang.Object[] parames = new Otp.Erlang.Object[] {
				new Otp.Erlang.List(new Otp.Erlang.Object[] {
					new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("name")
						,new Otp.Erlang.String(rule.Name)
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("class")
						,new Otp.Erlang.Atom("rule")
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("target")
						,new Otp.Erlang.String(target) 
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("action")
						//mailto script sms sound
						,new Otp.Erlang.Atom(rule.Action) 
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("category")
						,new Otp.Erlang.Atom("error")
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("action_param")
						,rule.GetActionParam()
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("condition")
						,rule.GetCondation()
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("enabled")
						,new Otp.Erlang.Atom(rule.Enable.ToString().ToLower())
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("name_match")
						,new Otp.Erlang.String("")
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("status_match")
						,new Otp.Erlang.String("")
					})
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom("type_match")
						,new Otp.Erlang.String("any")
					})
				})
			};
					
			connection.sendRPC("api_alert", "create", parames);
			Otp.Erlang.Object temp = connection.receiveRPC();
			if(ErlangEntity.IsOk(temp))
			{
				addToLog(LOG_CATEGORY_ALERTRULE,LOG_KEY_IMPORT_SUCCESS
				         ,string.Format("Section为 {0} 名称为 {1} 的告警规则导入成功。",rule.Section,rule.Name));
			}
			else
			{
				addToLog(LOG_CATEGORY_ALERTRULE,LOG_KEY_IMPORT_FAILURE
				         ,string.Format("Section为 {0} 名称为 {1} 的告警规则导入失败。",rule.Section,rule.Name));
			}
		}
		
		private TaskSchedule FindTaskScheduleByName(string taskName)
		{
			if(importedTaskSchedule != null && importedTaskSchedule.ContainsKey(taskName))
				return importedTaskSchedule[taskName];
			
			return null;
		}
		
		private Otp.Erlang.Object buildEmailSettingTaskParam(string originTaskName)
		{
			TaskSchedule task = FindTaskScheduleByName(originTaskName);
			if(null == task || TaskSchedule.TaskScheduleTime.Range != task.NewType)		
				return new Otp.Erlang.List();
				
			Otp.Erlang.Object[] weekdaysTime = new Otp.Erlang.Object[7];
			for(int i = 0; i < 7; i++)
			{
				weekdaysTime[i] = buildEmailSettingWeekdaysTime(task,i);
			}
			return new Otp.Erlang.List(weekdaysTime);
		}
		
		private Otp.Erlang.Object buildEmailSettingWeekdaysTime(TaskSchedule task,int weekday)
		{
			string enable = task.Properties["Allow" + weekday ]  == "1" ? "enabled" : "disabled";
			string startTime = task.Properties["start" + weekday] ;
			string endTime = task.Properties["end" + weekday] ;
			
			int dayFlag = weekday == 0 ? 7 : weekday;
			
			return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
				new Otp.Erlang.Int(dayFlag)
				,new Otp.Erlang.String(enable)
				,new Otp.Erlang.String(startTime)
				,new Otp.Erlang.String(endTime)
			});
		}
		
		private void import(EmailSetting emailSetting)
		{
			Otp.Erlang.Object[] parames = new Otp.Erlang.Object[] {
				new Otp.Erlang.Atom("additional_email_settings")
				,new Otp.Erlang.Atom(emailSetting.ID)
				,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
					new Otp.Erlang.Atom("additional_email_settings")
					,new Otp.Erlang.String(emailSetting.Name)
					,new Otp.Erlang.String(emailSetting.Email)
					,new Otp.Erlang.String(emailSetting.Disable.ToString().ToLower()) 
					,new Otp.Erlang.String("use_alert") 
					,buildEmailSettingTaskParam(emailSetting.Schedule)
				})
			};
					
			connection.sendRPC("api_preferences", "set_prefs", parames);
			Otp.Erlang.Object temp = connection.receiveRPC();
					
			if(ErlangEntity.IsOk(temp))
			{
				importedEmailSettings.Add(emailSetting.Name,emailSetting.ID);
				addToLog(LOG_CATEGORY_EMAILSETTING,LOG_KEY_IMPORT_SUCCESS
				         ,string.Format("Section为 {0} 名称为 {1} 的邮件设置导入成功，新ID为 {2}"
				                        ,emailSetting.Section,emailSetting.Name,emailSetting.ID));				
			}
			else
			{
				addToLog(LOG_CATEGORY_EMAILSETTING,LOG_KEY_IMPORT_FAILURE
				         ,string.Format("Section为 {0} 名称为 {1} 的邮件设置导入失败",emailSetting.Section,emailSetting.Name));
			}
		}
		
		private Otp.Erlang.Object buildTaskParam(TaskSchedule task)
		{
			Otp.Erlang.Object[] param = new Otp.Erlang.Object[9];
			param[0] = new Otp.Erlang.String(task.Name);
			string taskType = null;
			if( TaskSchedule.TaskScheduleTime.Absolute == task.NewType)
			{
				taskType = "absolute";
			}
			else if( TaskSchedule.TaskScheduleTime.Range == task.NewType)
			{
				taskType = "range";
			}
			else if( TaskSchedule.TaskScheduleTime.Relative == task.NewType)
			{
				taskType = "hours";
			}
			
			param[1] = new Otp.Erlang.String(taskType);
			for(int i = 2,weekday =0; i< 9; i++,weekday++)
			{
				param[i] = buildTaskTime(task,weekday);
			}		
			
			return new Otp.Erlang.Tuple(param);
		}
		
		private Otp.Erlang.Object buildTaskTime(TaskSchedule task,int weekday)
		{
			if(TaskSchedule.TaskScheduleTime.Absolute == task.NewType)
			{
				string dateTime = task.Properties["start" + weekday];
				return new Otp.Erlang.String(Convert.ToDateTime(dateTime).ToString("h:mm"));
			}
			
			if(TaskSchedule.TaskScheduleTime.Range == task.NewType)
			{
				string enable = task.Properties["Allow" + weekday]  == "1" ? "enabled" : "disabled";
				string startTime = task.Properties["start" + weekday] ;
				string endTime = task.Properties["end" + weekday] ;
				
				return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
					new Otp.Erlang.String(enable)
					,new Otp.Erlang.String(startTime)
					,new Otp.Erlang.String(endTime)
				});
			}
		
			if(TaskSchedule.TaskScheduleTime.Relative == task.NewType)
			{
				string bitStr = task.Properties["start" + weekday] ;
				string times = string.Empty;
				for(int i = 0,len = bitStr.Length; i < len ; i++)
				{
					if('1' == bitStr[i])
					{
						times += "," + i;
					}
				}
				
				if(times.Length > 0)
				{
					times = times.Substring(1);
				}
				
				return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
					new Otp.Erlang.String("enabled")
					,new Otp.Erlang.String(times)
					//,new Otp.Erlang.String(string.Empty)
				});
			}
			
			throw new InvalidOperationException();
		}
		
		private void import(TaskSchedule taskSchedule)
		{
			Otp.Erlang.Object[] parames = new Otp.Erlang.Object[] {
				buildTaskParam(taskSchedule)
			};
				
			connection.sendRPC("api_schedule", "create", parames);
			Otp.Erlang.Object temp = connection.receiveRPC();
				
			if(ErlangEntity.IsOk(temp))
			{
				importedTaskSchedule.Add(taskSchedule.Name,taskSchedule);
				addToLog(LOG_CATEGORY_TASK,LOG_KEY_IMPORT_SUCCESS
				         ,string.Format("名称为 {0} 的任务计划导入成功",taskSchedule.Name));
			}
			else
			{
				addToLog(LOG_CATEGORY_TASK,LOG_KEY_IMPORT_FAILURE
				         ,string.Format("名称为 {0} 的任务计划导入失败",taskSchedule.Name));
			}	
		}
		
		private List<GroupEntity> getHadGroupDependOn()
		{
			if(null == this.importedMinitors)
				return null;
			
			List<GroupEntity> dependOnGroups = new List<GroupEntity>();
			foreach(GroupEntity group in this.importedGroups.Values)
			{
				if(!string.IsNullOrEmpty(group.OldDependOnMonitorId))
					dependOnGroups.Add(group);
			}			
			
			return dependOnGroups;
		}
		
		private bool isImported(string monitorId)
		{
			if(this.importedMinitors != null && this.importedMinitors.ContainsKey(monitorId))
				return true;
			
			return false;
		}
		
		private void updateGroupDependOn()
		{
			IEnumerable<GroupEntity> groups = getHadGroupDependOn();
			
			if(null == groups)
				return;
			
			foreach(GroupEntity group in groups)
			{
				try
				{
					if(isImported(group.OldDependOnMonitorId))
					{
						group.NewDependOnMonitorId = importedMinitors[group.OldDependOnMonitorId].NewId;
						
						connection.sendRPC("api_group", "update", group.GetUpdateDependOnParam());
						Otp.Erlang.Object temp = connection.receiveRPC();
						Otp.Erlang.Object[] erlObj = ((Otp.Erlang.Tuple)temp).elements();
						bool result = erlObj !=null && erlObj.Length == 2 && String.Compare(erlObj[0].ToString(),"ok",true) == 0;			
						if(result)
						{
							addToLog(LOG_CATEGORY_GROUP,LOG_KEY_UPDATE_SUCCESS
									 ,string.Format("更新依赖ID为 {0} 节点为 {1} 的组成功,依赖ID为 {2}",group.OldId,group.Path, group.NewDependOnMonitorId));
						}
						else
						{
							addToLog(LOG_CATEGORY_GROUP,LOG_KEY_UPDATE_FAILURE
									 ,string.Format("更新依赖ID为 {0} 节点为 {1} 的组失败,原因: {2}",group.OldId,group.Path,erlObj[1]));
						}
					}
					else
					{
						addToLog(LOG_CATEGORY_GROUP,LOG_KEY_OTHER
								 ,string.Format("组ID为 {0} 节点为 {1} 的依赖监测器未导入，因此未更新此依赖.",group.OldId,group.Path));
					}
				}
				catch(Exception err)
				{
					addToLog(LOG_CATEGORY_GROUP,LOG_KEY_UPDATE_FAILURE
							 ,string.Format("更新依赖时出现异常：{0}",err.ToString()));
				}
			}			
		}
		
		private void addToLog(string category,string key,string info)
		{
			if(string.IsNullOrEmpty(category) || string.IsNullOrEmpty(key))
				return;
			
			NameValueCollection log = this.Logs.ContainsKey(category) 
				? this.Logs[category] : new NameValueCollection() ;
			log.Add(key,info);
			
			if(!this.Logs.ContainsKey(category))
				this.Logs.Add(category,log);
		}
		#endregion
			
		#region Private Property		
		private Dictionary<string,MonitorEntity> importedMinitors = new Dictionary<string, MonitorEntity>();
		//private Dictionary<string,RemoteServerEntity> importedRemoteServers = null;
		private Dictionary<string,GroupEntity> importedGroups = new Dictionary<string, GroupEntity>();
		private Dictionary<string,string> importedEmailSettings = new Dictionary<string, string>();
		private Dictionary<string,TaskSchedule> importedTaskSchedule = new Dictionary<string, TaskSchedule>();
				
		private OtpConnection _connection;
		private OtpConnection connection
		{
			get
			{return _connection;}
			set
			{_connection = value;}
		}		
		
		#endregion
	}
}
