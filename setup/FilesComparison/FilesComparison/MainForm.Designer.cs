namespace FilesComparison
{
    partial class MainForm
    {
        /// <summary>
        /// 必需的设计器变量。
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// 清理所有正在使用的资源。
        /// </summary>
        /// <param name="disposing">如果应释放托管资源，为 true；否则为 false。</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows 窗体设计器生成的代码

        /// <summary>
        /// 设计器支持所需的方法 - 不要
        /// 使用代码编辑器修改此方法的内容。
        /// </summary>
        private void InitializeComponent()
        {
            this.txt_old = new System.Windows.Forms.TextBox();
            this.lab_old = new System.Windows.Forms.Label();
            this.btn_control = new System.Windows.Forms.Button();
            this.txt_new = new System.Windows.Forms.TextBox();
            this.lab_new = new System.Windows.Forms.Label();
            this.txt_export = new System.Windows.Forms.TextBox();
            this.lab_export = new System.Windows.Forms.Label();
            this.lb_result = new System.Windows.Forms.ListBox();
            this.SuspendLayout();
            // 
            // txt_old
            // 
            this.txt_old.Location = new System.Drawing.Point(106, 11);
            this.txt_old.Name = "txt_old";
            this.txt_old.Size = new System.Drawing.Size(422, 21);
            this.txt_old.TabIndex = 0;
            // 
            // lab_old
            // 
            this.lab_old.AutoSize = true;
            this.lab_old.Location = new System.Drawing.Point(16, 15);
            this.lab_old.Name = "lab_old";
            this.lab_old.Size = new System.Drawing.Size(89, 12);
            this.lab_old.TabIndex = 1;
            this.lab_old.Text = "原始结构路径：";
            // 
            // btn_control
            // 
            this.btn_control.Location = new System.Drawing.Point(453, 96);
            this.btn_control.Name = "btn_control";
            this.btn_control.Size = new System.Drawing.Size(75, 23);
            this.btn_control.TabIndex = 2;
            this.btn_control.Text = "开始分析";
            this.btn_control.UseVisualStyleBackColor = true;
            this.btn_control.Click += new System.EventHandler(this.btn_control_Click);
            // 
            // txt_new
            // 
            this.txt_new.Location = new System.Drawing.Point(106, 38);
            this.txt_new.Name = "txt_new";
            this.txt_new.Size = new System.Drawing.Size(422, 21);
            this.txt_new.TabIndex = 0;
            // 
            // lab_new
            // 
            this.lab_new.AutoSize = true;
            this.lab_new.Location = new System.Drawing.Point(16, 42);
            this.lab_new.Name = "lab_new";
            this.lab_new.Size = new System.Drawing.Size(89, 12);
            this.lab_new.TabIndex = 1;
            this.lab_new.Text = "最新结构路径：";
            // 
            // txt_export
            // 
            this.txt_export.Location = new System.Drawing.Point(106, 65);
            this.txt_export.Name = "txt_export";
            this.txt_export.Size = new System.Drawing.Size(422, 21);
            this.txt_export.TabIndex = 0;
            // 
            // lab_export
            // 
            this.lab_export.AutoSize = true;
            this.lab_export.Location = new System.Drawing.Point(16, 69);
            this.lab_export.Name = "lab_export";
            this.lab_export.Size = new System.Drawing.Size(89, 12);
            this.lab_export.TabIndex = 1;
            this.lab_export.Text = "导出结构路径：";
            // 
            // lb_result
            // 
            this.lb_result.FormattingEnabled = true;
            this.lb_result.HorizontalScrollbar = true;
            this.lb_result.ItemHeight = 12;
            this.lb_result.Location = new System.Drawing.Point(18, 128);
            this.lb_result.Name = "lb_result";
            this.lb_result.ScrollAlwaysVisible = true;
            this.lb_result.Size = new System.Drawing.Size(510, 292);
            this.lb_result.TabIndex = 3;
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 12F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(545, 434);
            this.Controls.Add(this.lb_result);
            this.Controls.Add(this.btn_control);
            this.Controls.Add(this.lab_export);
            this.Controls.Add(this.lab_new);
            this.Controls.Add(this.lab_old);
            this.Controls.Add(this.txt_export);
            this.Controls.Add(this.txt_new);
            this.Controls.Add(this.txt_old);
            this.Name = "MainForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "文件对比分析工具";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox txt_old;
        private System.Windows.Forms.Label lab_old;
        private System.Windows.Forms.Button btn_control;
        private System.Windows.Forms.TextBox txt_new;
        private System.Windows.Forms.Label lab_new;
        private System.Windows.Forms.TextBox txt_export;
        private System.Windows.Forms.Label lab_export;
        private System.Windows.Forms.ListBox lb_result;
    }
}

