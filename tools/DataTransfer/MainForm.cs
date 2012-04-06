////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 11:36
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Collections.Specialized;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Drawing;
using System.Windows.Forms;

namespace DataTransfer
{
	/// <summary>
	/// Description of MainForm.
	/// </summary>
	public partial class MainForm : Form
	{
		private const string FILE_LOG_PATH = @"./log/log.txt";
		private const string FILE_CONFIG_PATH = @"./config/config.xml";
		
		public MainForm()
		{
			//
			// The InitializeComponent() call is required for Windows Forms designer support.
			//
			InitializeComponent();
			
			//
			// TODO: Add constructor code after the InitializeComponent() call.
			//
		}		
		
		#region Event Handle		
		void ImportBtnClick(object sender, EventArgs e)
		{		
			string ecc8WebServiceUrl = ecc8WebServiceUrlTxt.Text.Trim();
			string ecc9NodeName = this.ecc9NodeNameTxt.Text.Trim();
			string ecc9Cookie =this.ecc9CookieTxt.Text.Trim();
			
			CheckBox[] options = {groupOptionChk,deviceOptionChk,monitorOptionChk
									,emailSetOptionChk,alertRuleOptionChk,taskOptionChk
									,userRightOptionChk};
			bool checkedOption = false;
			foreach(CheckBox option in options)
			{
				if(option.Checked)
				{
					checkedOption = true;
					break;
				}
			}
			
			if(!checkedOption)
			{
				MessageBox.Show("请选择要导入的数据项","数据导入",MessageBoxButtons.OK);
				return;
			}
						
			Control[] controls = {ecc8WebServiceUrlTxt,ecc9NodeNameTxt,ecc9CookieTxt
					,groupOptionChk,deviceOptionChk,monitorOptionChk
					,emailSetOptionChk,alertRuleOptionChk,taskOptionChk
					,userRightOptionChk,importBtn,configBtn,showLogBtn};
						
			ControlUtility.Enable(controls,false);
			try
			{
				BackgroundWorker bgWorker = new BackgroundWorker();
				bgWorker.DoWork += delegate {
					DoTask(new Ecc8Utility(ecc8WebServiceUrl),
					    new Ecc9Utility(ecc9NodeName,ecc9Cookie));
				};
				bgWorker.RunWorkerCompleted += delegate {
					ControlUtility.Enable(controls,true);
				};
				bgWorker.RunWorkerAsync();
				
			}
			catch(Exception err)
			{
				cout(err.ToString());
			}			
		}		
			
		void ShowLogBtnClick(object sender, EventArgs e)
		{
			if(hasFile(FILE_LOG_PATH))
				showPlainFile(FILE_LOG_PATH);
		}
		
		void ConfigBtnClick(object sender, EventArgs e)
		{
			if(!hasFile(FILE_CONFIG_PATH))
				File.Create(FILE_CONFIG_PATH);
			showPlainFile(FILE_CONFIG_PATH);
		}
		
		void DeviceOptionChkCheckedChanged(object sender, EventArgs e)
		{
			CheckBox[] required = {groupOptionChk};
			CheckBox[] related = {monitorOptionChk};
			setOption((CheckBox)sender,required,related);
		}
		
		void GroupOptionChkCheckedChanged(object sender, EventArgs e)
		{
			CheckBox[] required = {};
			CheckBox[] related = {deviceOptionChk};
			setOption((CheckBox)sender,required,related);
		}
		
		void MonitorOptionChkCheckedChanged(object sender, EventArgs e)
		{
			CheckBox[] required = {deviceOptionChk};
			CheckBox[] related = {alertRuleOptionChk,userRightOptionChk};
			setOption((CheckBox)sender,required,related);
		}
		
		void TaskOptionChkCheckedChanged(object sender, EventArgs e)
		{
			CheckBox[] required = {};
			CheckBox[] related = {emailSetOptionChk};
			setOption((CheckBox)sender,required,related);
		}
		
		void EmailSetOptionChkCheckedChanged(object sender, EventArgs e)
		{
			CheckBox[] required = {taskOptionChk};
			CheckBox[] related = {alertRuleOptionChk};
			setOption((CheckBox)sender,required,related);
		}
		
		void UserRightOptionChkCheckedChanged(object sender, EventArgs e)
		{
			CheckBox[] required = {monitorOptionChk};
			CheckBox[] related = {};
			setOption((CheckBox)sender,required,related);
		}
		
		void AlertRuleOptionChkCheckedChanged(object sender, EventArgs e)
		{
			CheckBox[] required = {emailSetOptionChk,monitorOptionChk};
			CheckBox[] related = {};
			setOption((CheckBox)sender,required,related);
		}
		#endregion
		
		#region Private Method		
		private void DoTask(Ecc8Utility ecc8Utility,Ecc9Utility ecc9Utility)
		{	
			cout("开始加载导入配置参数...");
			if(ConfigUtility.LoadConfig())
				cout("加载导入配置参数完成");
			else
			{
				cout("加载导入配置参数失败，不能执行导入");
				return;
			}
			
			List<GroupEntity> groupList = null;
			List<RemoteServerEntity> remoteServerList = null;
			List<MonitorEntity> monitorList = null;
			
			if(groupOptionChk.Checked)
			{
				cout("开始导出组数据...");
				
				ecc8Utility.ExportEntity(out groupList,out remoteServerList,out monitorList);
				
				cout(string.Format("导出组数量为：{0}",groupList.Count));
				cout("开始导入组...");
				ecc9Utility.ImportGroup(groupList);
				cout("导入组结束");	
			}
			
			if(deviceOptionChk.Checked)
			{
				cout("开始导出设备数据...");
				cout(string.Format("导出设备数量为：{0}",remoteServerList.Count));
				cout("开始导入远程服务器...");
				ecc9Utility.ImportRemoteServer(remoteServerList);
				cout("导入远程服务器结束");	
			}
			
			if(monitorOptionChk.Checked)
			{
				cout("开始导出监测器数据...");
				cout(string.Format("导出监测器数量为：{0}",monitorList.Count));
				cout("开始导入监测器...");
				ecc9Utility.ImportMonitor(monitorList);
				cout("导入监测器结束");
				
				cout("更新依赖...");
				ecc9Utility.UpdateDependOn();
				cout("更新依赖结束");
			}
			
			if(taskOptionChk.Checked)
			{
				cout("开始导出任务计划数据...");
				List<TaskSchedule> taskList = ecc8Utility.ExportTask();
				cout("ECC8中的任务计划数: " + taskList.Count);
				cout("开始导入任务计划...");
				ecc9Utility.ImportTask(taskList);
				cout("导入任务计划结束");
			}
			
			if(emailSetOptionChk.Checked)
			{
				cout("开始导出邮件设置数据...");
				List<EmailSetting> emailSettingList = ecc8Utility.ExportEmailSetting();
				cout("ECC8中的邮件设置数: " + emailSettingList.Count);
				cout("开始导入邮件设置...");
				ecc9Utility.ImportEmailSetting(emailSettingList);
				cout("导入邮件设置结束");
			}
			
			if(alertRuleOptionChk.Checked)
			{
				cout("开始导出告警规则数据...");
				List<AlertRule> alertRuleList = ecc8Utility.ExportAlertRule();
				cout("ECC8中的告警规则数: " + alertRuleList.Count);
				cout("开始导入告警规则...");
				ecc9Utility.ImportAlertRule(alertRuleList);
				cout("导入告警规则结束");
			}
			
			if(userRightOptionChk.Checked)
			{
				cout("开始导出用户信息数据...");
				List<UserInfo> users = ecc8Utility.ExportUserInfo();
				cout("开始导入用户信息...");
				ecc9Utility.ImportUserInfo(users);
				cout("导入用户信息结束");
			}
			
			cout("写入日志...");
			using (StreamWriter sw = new StreamWriter(FILE_LOG_PATH)) 
			{
				writeLog(sw,ecc9Utility.GetLog(Ecc9Utility.LOG_CATEGORY_GROUP),"关于组");
				writeLog(sw,ecc9Utility.GetLog(Ecc9Utility.LOG_CATEGORY_REMOTESERVER),"关于设备");
				writeLog(sw,ecc9Utility.GetLog(Ecc9Utility.LOG_CATEGORY_MONITOR),"关于监测器");
				writeLog(sw,ecc9Utility.GetLog(Ecc9Utility.LOG_CATEGORY_EMAILSETTING),"关于邮件设置");
				writeLog(sw,ecc9Utility.GetLog(Ecc9Utility.LOG_CATEGORY_ALERTRULE),"关于告警规则");
				writeLog(sw,ecc9Utility.GetLog(Ecc9Utility.LOG_CATEGORY_TASK),"关于任务计划");
				writeLog(sw,ecc9Utility.GetLog(Ecc9Utility.LOG_CATEGORY_USERINFO),"关于用户信息");
			}
			cout("写入日志结束.");	
		}
		
		private static void writeLog(StreamWriter sw,NameValueCollection logCollection,string title)
		{
			if(null == logCollection)
				return;
			sw.WriteLine(string.Format("------------------------------- {0} -------------------------------",title));
			sw.WriteLine(string.Format("导入成功记录数：{0}，导入失败记录数：{1}，更新依赖成功记录数：{2}，更新依赖失败记录数：{3}，其他记录数：{4}"
					,getLogInfoCount(logCollection,Ecc9Utility.LOG_KEY_IMPORT_SUCCESS)
					,getLogInfoCount(logCollection,Ecc9Utility.LOG_KEY_IMPORT_FAILURE)
					,getLogInfoCount(logCollection,Ecc9Utility.LOG_KEY_UPDATE_SUCCESS)
					,getLogInfoCount(logCollection,Ecc9Utility.LOG_KEY_UPDATE_FAILURE)
					,getLogInfoCount(logCollection,"O")));
			writeLogInfo(sw,logCollection,"导入成功记录:",Ecc9Utility.LOG_KEY_IMPORT_SUCCESS);
			writeLogInfo(sw,logCollection,"导入失败记录:",Ecc9Utility.LOG_KEY_IMPORT_FAILURE);		
			writeLogInfo(sw,logCollection,"其他记录:",Ecc9Utility.LOG_KEY_OTHER);
			writeLogInfo(sw,logCollection,"更新依赖成功记录:",Ecc9Utility.LOG_KEY_UPDATE_SUCCESS);
			writeLogInfo(sw,logCollection,"更新依赖失败记录:",Ecc9Utility.LOG_KEY_UPDATE_FAILURE);
		}
		
		private static void writeLogInfo(StreamWriter sw,NameValueCollection logCollection,string title,string key)
		{
			if(logCollection == null)
				return;
				
			string[] infos = logCollection.GetValues(key);
			if(infos!=null)
			{
				sw.WriteLine(title);
				foreach(string info in infos)
				{
					sw.WriteLine(info);
				}
			}
		}
		
		private static int getLogInfoCount(NameValueCollection logCollection,string key)
		{
			if(logCollection == null || logCollection.Count == 0)
				return 0;
				
			string[] infos = logCollection.GetValues(key);
			if(infos!=null)
				return infos.Length;
			
			return 0;
		}
				
		private void cout(string msg)
		{
			if(string.IsNullOrEmpty(msg))
				return;
			
			progressMsgTxt.Text += msg + Environment.NewLine;
		}	
		
		private bool hasFile(string filePath)
		{
			return File.Exists(filePath);
		}
		
		private void showPlainFile(string filePath)
		{
			Process cmd = new Process();
			cmd.StartInfo.FileName = "notepad.exe";
			cmd.StartInfo.Arguments = Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory,filePath);
			cmd.Start();
		}
		
		private void setOption(CheckBox chk,CheckBox[] required,CheckBox[] related)
		{
			if(null == chk || null == required || null == related)
				return ;
			
			CheckBox[] controls = chk.Checked ? required : related;
			foreach(CheckBox option in controls)
				option.Checked = chk.Checked;
		}
		#endregion			
		
		
		
	}
}
