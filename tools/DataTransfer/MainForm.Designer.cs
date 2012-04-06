////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 11:36
//
////////////////////////////////////////////////////////////////////
 
namespace DataTransfer
{
	partial class MainForm
	{
		/// <summary>
		/// Designer variable used to keep track of non-visual components.
		/// </summary>
		private System.ComponentModel.IContainer components = null;
		
		/// <summary>
		/// Disposes resources used by the form.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			if (disposing) {
				if (components != null) {
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}
		
		/// <summary>
		/// This method is required for Windows Forms designer support.
		/// Do not change the method contents inside the source code editor. The Forms designer might
		/// not be able to load this method if it was changed manually.
		/// </summary>
		private void InitializeComponent()
		{
			this.importBtn = new System.Windows.Forms.Button();
			this.progressMsgTxt = new System.Windows.Forms.TextBox();
			this.label1 = new System.Windows.Forms.Label();
			this.groupOptionChk = new System.Windows.Forms.CheckBox();
			this.deviceOptionChk = new System.Windows.Forms.CheckBox();
			this.monitorOptionChk = new System.Windows.Forms.CheckBox();
			this.emailSetOptionChk = new System.Windows.Forms.CheckBox();
			this.alertRuleOptionChk = new System.Windows.Forms.CheckBox();
			this.taskOptionChk = new System.Windows.Forms.CheckBox();
			this.userRightOptionChk = new System.Windows.Forms.CheckBox();
			this.groupBox1 = new System.Windows.Forms.GroupBox();
			this.ecc8WebServiceUrlTxt = new System.Windows.Forms.TextBox();
			this.label2 = new System.Windows.Forms.Label();
			this.label3 = new System.Windows.Forms.Label();
			this.label4 = new System.Windows.Forms.Label();
			this.ecc9NodeNameTxt = new System.Windows.Forms.TextBox();
			this.ecc9CookieTxt = new System.Windows.Forms.TextBox();
			this.groupBox2 = new System.Windows.Forms.GroupBox();
			this.textBox2 = new System.Windows.Forms.TextBox();
			this.textBox1 = new System.Windows.Forms.TextBox();
			this.showLogBtn = new System.Windows.Forms.Button();
			this.configBtn = new System.Windows.Forms.Button();
			this.groupBox1.SuspendLayout();
			this.groupBox2.SuspendLayout();
			this.SuspendLayout();
			// 
			// importBtn
			// 
			this.importBtn.Location = new System.Drawing.Point(349, 210);
			this.importBtn.Name = "importBtn";
			this.importBtn.Size = new System.Drawing.Size(75, 23);
			this.importBtn.TabIndex = 0;
			this.importBtn.Text = "开始导入";
			this.importBtn.UseVisualStyleBackColor = true;
			this.importBtn.Click += new System.EventHandler(this.ImportBtnClick);
			// 
			// progressMsgTxt
			// 
			this.progressMsgTxt.Location = new System.Drawing.Point(12, 239);
			this.progressMsgTxt.Multiline = true;
			this.progressMsgTxt.Name = "progressMsgTxt";
			this.progressMsgTxt.Size = new System.Drawing.Size(586, 217);
			this.progressMsgTxt.TabIndex = 1;
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(12, 222);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(100, 14);
			this.label1.TabIndex = 2;
			this.label1.Text = "进度信息：";
			// 
			// groupOptionChk
			// 
			this.groupOptionChk.Checked = true;
			this.groupOptionChk.CheckState = System.Windows.Forms.CheckState.Checked;
			this.groupOptionChk.Location = new System.Drawing.Point(16, 25);
			this.groupOptionChk.Name = "groupOptionChk";
			this.groupOptionChk.Size = new System.Drawing.Size(46, 24);
			this.groupOptionChk.TabIndex = 3;
			this.groupOptionChk.Text = "组";
			this.groupOptionChk.UseVisualStyleBackColor = true;
			this.groupOptionChk.CheckedChanged += new System.EventHandler(this.GroupOptionChkCheckedChanged);
			// 
			// deviceOptionChk
			// 
			this.deviceOptionChk.Checked = true;
			this.deviceOptionChk.CheckState = System.Windows.Forms.CheckState.Checked;
			this.deviceOptionChk.Location = new System.Drawing.Point(92, 25);
			this.deviceOptionChk.Name = "deviceOptionChk";
			this.deviceOptionChk.Size = new System.Drawing.Size(52, 24);
			this.deviceOptionChk.TabIndex = 5;
			this.deviceOptionChk.Text = "设备";
			this.deviceOptionChk.UseVisualStyleBackColor = true;
			this.deviceOptionChk.CheckedChanged += new System.EventHandler(this.DeviceOptionChkCheckedChanged);
			// 
			// monitorOptionChk
			// 
			this.monitorOptionChk.Checked = true;
			this.monitorOptionChk.CheckState = System.Windows.Forms.CheckState.Checked;
			this.monitorOptionChk.Location = new System.Drawing.Point(174, 25);
			this.monitorOptionChk.Name = "monitorOptionChk";
			this.monitorOptionChk.Size = new System.Drawing.Size(65, 24);
			this.monitorOptionChk.TabIndex = 6;
			this.monitorOptionChk.Text = "监测器";
			this.monitorOptionChk.UseVisualStyleBackColor = true;
			this.monitorOptionChk.CheckedChanged += new System.EventHandler(this.MonitorOptionChkCheckedChanged);
			// 
			// emailSetOptionChk
			// 
			this.emailSetOptionChk.Checked = true;
			this.emailSetOptionChk.CheckState = System.Windows.Forms.CheckState.Checked;
			this.emailSetOptionChk.Location = new System.Drawing.Point(92, 55);
			this.emailSetOptionChk.Name = "emailSetOptionChk";
			this.emailSetOptionChk.Size = new System.Drawing.Size(72, 24);
			this.emailSetOptionChk.TabIndex = 7;
			this.emailSetOptionChk.Text = "邮件设置";
			this.emailSetOptionChk.UseVisualStyleBackColor = true;
			this.emailSetOptionChk.CheckedChanged += new System.EventHandler(this.EmailSetOptionChkCheckedChanged);
			// 
			// alertRuleOptionChk
			// 
			this.alertRuleOptionChk.Checked = true;
			this.alertRuleOptionChk.CheckState = System.Windows.Forms.CheckState.Checked;
			this.alertRuleOptionChk.Location = new System.Drawing.Point(174, 55);
			this.alertRuleOptionChk.Name = "alertRuleOptionChk";
			this.alertRuleOptionChk.Size = new System.Drawing.Size(79, 24);
			this.alertRuleOptionChk.TabIndex = 8;
			this.alertRuleOptionChk.Text = "告警规则";
			this.alertRuleOptionChk.UseVisualStyleBackColor = true;
			this.alertRuleOptionChk.CheckedChanged += new System.EventHandler(this.AlertRuleOptionChkCheckedChanged);
			// 
			// taskOptionChk
			// 
			this.taskOptionChk.Checked = true;
			this.taskOptionChk.CheckState = System.Windows.Forms.CheckState.Checked;
			this.taskOptionChk.Location = new System.Drawing.Point(16, 55);
			this.taskOptionChk.Name = "taskOptionChk";
			this.taskOptionChk.Size = new System.Drawing.Size(80, 24);
			this.taskOptionChk.TabIndex = 9;
			this.taskOptionChk.Text = "任务计划";
			this.taskOptionChk.UseVisualStyleBackColor = true;
			this.taskOptionChk.CheckedChanged += new System.EventHandler(this.TaskOptionChkCheckedChanged);
			// 
			// userRightOptionChk
			// 
			this.userRightOptionChk.Checked = true;
			this.userRightOptionChk.CheckState = System.Windows.Forms.CheckState.Checked;
			this.userRightOptionChk.Location = new System.Drawing.Point(16, 85);
			this.userRightOptionChk.Name = "userRightOptionChk";
			this.userRightOptionChk.Size = new System.Drawing.Size(104, 24);
			this.userRightOptionChk.TabIndex = 10;
			this.userRightOptionChk.Text = "用户及权限";
			this.userRightOptionChk.UseVisualStyleBackColor = true;
			this.userRightOptionChk.CheckedChanged += new System.EventHandler(this.UserRightOptionChkCheckedChanged);
			// 
			// groupBox1
			// 
			this.groupBox1.Controls.Add(this.emailSetOptionChk);
			this.groupBox1.Controls.Add(this.userRightOptionChk);
			this.groupBox1.Controls.Add(this.groupOptionChk);
			this.groupBox1.Controls.Add(this.taskOptionChk);
			this.groupBox1.Controls.Add(this.deviceOptionChk);
			this.groupBox1.Controls.Add(this.alertRuleOptionChk);
			this.groupBox1.Controls.Add(this.monitorOptionChk);
			this.groupBox1.Location = new System.Drawing.Point(338, 12);
			this.groupBox1.Name = "groupBox1";
			this.groupBox1.Size = new System.Drawing.Size(260, 188);
			this.groupBox1.TabIndex = 11;
			this.groupBox1.TabStop = false;
			this.groupBox1.Text = "导入选项";
			// 
			// ecc8WebServiceUrlTxt
			// 
			this.ecc8WebServiceUrlTxt.Location = new System.Drawing.Point(9, 39);
			this.ecc8WebServiceUrlTxt.Name = "ecc8WebServiceUrlTxt";
			this.ecc8WebServiceUrlTxt.Size = new System.Drawing.Size(277, 21);
			this.ecc8WebServiceUrlTxt.TabIndex = 12;
			// 
			// label2
			// 
			this.label2.Location = new System.Drawing.Point(8, 25);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(163, 23);
			this.label2.TabIndex = 13;
			this.label2.Text = "ECC 8.1.3 Web Service Url";
			// 
			// label3
			// 
			this.label3.Location = new System.Drawing.Point(8, 82);
			this.label3.Name = "label3";
			this.label3.Size = new System.Drawing.Size(100, 23);
			this.label3.TabIndex = 14;
			this.label3.Text = "ECC9 Node Name";
			// 
			// label4
			// 
			this.label4.Location = new System.Drawing.Point(8, 139);
			this.label4.Name = "label4";
			this.label4.Size = new System.Drawing.Size(100, 23);
			this.label4.TabIndex = 15;
			this.label4.Text = "ECC9 Cookie";
			// 
			// ecc9NodeNameTxt
			// 
			this.ecc9NodeNameTxt.Location = new System.Drawing.Point(9, 97);
			this.ecc9NodeNameTxt.Name = "ecc9NodeNameTxt";
			this.ecc9NodeNameTxt.Size = new System.Drawing.Size(277, 21);
			this.ecc9NodeNameTxt.TabIndex = 16;
			// 
			// ecc9CookieTxt
			// 
			this.ecc9CookieTxt.Location = new System.Drawing.Point(9, 155);
			this.ecc9CookieTxt.Name = "ecc9CookieTxt";
			this.ecc9CookieTxt.Size = new System.Drawing.Size(277, 21);
			this.ecc9CookieTxt.TabIndex = 17;
			// 
			// groupBox2
			// 
			this.groupBox2.Controls.Add(this.textBox2);
			this.groupBox2.Controls.Add(this.textBox1);
			this.groupBox2.Controls.Add(this.ecc9CookieTxt);
			this.groupBox2.Controls.Add(this.ecc9NodeNameTxt);
			this.groupBox2.Controls.Add(this.ecc8WebServiceUrlTxt);
			this.groupBox2.Controls.Add(this.label2);
			this.groupBox2.Controls.Add(this.label3);
			this.groupBox2.Controls.Add(this.label4);
			this.groupBox2.Location = new System.Drawing.Point(12, 12);
			this.groupBox2.Name = "groupBox2";
			this.groupBox2.Size = new System.Drawing.Size(300, 188);
			this.groupBox2.TabIndex = 20;
			this.groupBox2.TabStop = false;
			this.groupBox2.Text = "连接设置";
			// 
			// textBox2
			// 
			this.textBox2.BorderStyle = System.Windows.Forms.BorderStyle.None;
			this.textBox2.Location = new System.Drawing.Point(7, 121);
			this.textBox2.Name = "textBox2";
			this.textBox2.ReadOnly = true;
			this.textBox2.Size = new System.Drawing.Size(289, 14);
			this.textBox2.TabIndex = 21;
			this.textBox2.TabStop = false;
			this.textBox2.Text = "(如:ecc9@siteview)";
			// 
			// textBox1
			// 
			this.textBox1.BorderStyle = System.Windows.Forms.BorderStyle.None;
			this.textBox1.Location = new System.Drawing.Point(7, 62);
			this.textBox1.Name = "textBox1";
			this.textBox1.ReadOnly = true;
			this.textBox1.Size = new System.Drawing.Size(289, 14);
			this.textBox1.TabIndex = 20;
			this.textBox1.TabStop = false;
			this.textBox1.Text = "(如:http://127.0.0.1:80/svdb/eccservices/eccapi)";
			// 
			// showLogBtn
			// 
			this.showLogBtn.Location = new System.Drawing.Point(511, 210);
			this.showLogBtn.Name = "showLogBtn";
			this.showLogBtn.Size = new System.Drawing.Size(75, 23);
			this.showLogBtn.TabIndex = 21;
			this.showLogBtn.Text = "查看日志";
			this.showLogBtn.UseVisualStyleBackColor = true;
			this.showLogBtn.Click += new System.EventHandler(this.ShowLogBtnClick);
			// 
			// configBtn
			// 
			this.configBtn.Location = new System.Drawing.Point(430, 210);
			this.configBtn.Name = "configBtn";
			this.configBtn.Size = new System.Drawing.Size(75, 23);
			this.configBtn.TabIndex = 22;
			this.configBtn.Text = "配置参数";
			this.configBtn.UseVisualStyleBackColor = true;
			this.configBtn.Click += new System.EventHandler(this.ConfigBtnClick);
			// 
			// MainForm
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 12F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(612, 468);
			this.Controls.Add(this.groupBox2);
			this.Controls.Add(this.configBtn);
			this.Controls.Add(this.showLogBtn);
			this.Controls.Add(this.groupBox1);
			this.Controls.Add(this.label1);
			this.Controls.Add(this.progressMsgTxt);
			this.Controls.Add(this.importBtn);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
			this.Name = "MainForm";
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
			this.Text = "数据升级工具";
			this.groupBox1.ResumeLayout(false);
			this.groupBox2.ResumeLayout(false);
			this.groupBox2.PerformLayout();
			this.ResumeLayout(false);
			this.PerformLayout();
		}
		private System.Windows.Forms.TextBox textBox1;
		private System.Windows.Forms.TextBox textBox2;
		private System.Windows.Forms.TextBox progressMsgTxt;
		private System.Windows.Forms.TextBox ecc8WebServiceUrlTxt;
		private System.Windows.Forms.TextBox ecc9NodeNameTxt;
		private System.Windows.Forms.TextBox ecc9CookieTxt;
		private System.Windows.Forms.Button configBtn;
		private System.Windows.Forms.Button showLogBtn;
		private System.Windows.Forms.GroupBox groupBox2;
		private System.Windows.Forms.Label label4;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.CheckBox userRightOptionChk;
		private System.Windows.Forms.CheckBox taskOptionChk;
		private System.Windows.Forms.CheckBox alertRuleOptionChk;
		private System.Windows.Forms.CheckBox emailSetOptionChk;
		private System.Windows.Forms.GroupBox groupBox1;
		private System.Windows.Forms.CheckBox deviceOptionChk;
		private System.Windows.Forms.CheckBox monitorOptionChk;
		private System.Windows.Forms.CheckBox groupOptionChk;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Button importBtn;
	}
}
