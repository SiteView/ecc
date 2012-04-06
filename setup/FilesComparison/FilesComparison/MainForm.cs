using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Collections;

namespace FilesComparison
{
    public partial class MainForm : Form
    {
        string folder_old = "";
        string folder_new = "";
        string folder_export = "";

        string logpath = @"c:\filescomparison.log";
        string log_delpath = @"c:\filescomparison_del.log";

        public MainForm()
        {
            InitializeComponent();
        }

        private void btn_control_Click(object sender, EventArgs e)
        {
            //清空listbox
            this.lb_result.Items.Clear();

            //读取地址路径
            folder_old = this.txt_old.Text.ToString().Trim();
            folder_new = this.txt_new.Text.ToString().Trim();
            folder_export = this.txt_export.Text.ToString().Trim();

            //生成log文件
            FileObj.WriteFile(logpath, "");

            //判断操作
            if (folder_old == "" || folder_new == "" || folder_export == "")
            {
                MessageBox.Show("对比路径没有填写完整！");
            }
            else
            {
                try
                {
                    //判断文件修改和新增
                    this.lb_result.Items.Add("开始扫描新增或者修改文件列表……");
                    DirectoryInfo AllFiles_new = new System.IO.DirectoryInfo(folder_new);
                    getallfiles(AllFiles_new);

                    //判断文件删除
                    this.lb_result.Items.Add("正在扫描删除文件列表，请等待……");
                    this.lb_result.SetSelected(this.lb_result.Items.Count - 1, true);
                    DirectoryInfo AllFiles_old = new System.IO.DirectoryInfo(folder_old);
                    getallfiles2(AllFiles_old);

                    this.lb_result.Items.Add("完成，扫描文件结束！");
                    this.lb_result.SetSelected(this.lb_result.Items.Count - 1, true);
                }
                catch
                { MessageBox.Show("失败，未知名错误！"); }
            }
        }

        public void getallfiles2(DirectoryInfo di)
        {
            int folder_length = folder_old.Length;
            FileInfo[] files = di.GetFiles("*.*");

            foreach (FileInfo file in files)
            {
                string path = file.FullName.Substring(folder_length, file.FullName.Length - folder_length);
                //this.lb_result.Items.Add(file.FullName.Substring(folder_length, file.FullName.Length - folder_length));
                //this.lb_result.SetSelected(this.lb_result.Items.Count - 1, true);

                if (!System.IO.File.Exists(folder_new + path))
                {
                    //生成日志文件
                    FileObj.FileAdd(log_delpath, "删除文件 -> " + path + "\r\n");

                    continue;
                }
            }

            foreach (DirectoryInfo directory in di.GetDirectories())
            {
                if (directory.Name != ".svn")
                {
                    getallfiles2(directory);
                }
            }
        }

        public void getallfiles(DirectoryInfo di)
        {
            int folder_length = folder_new.Length;
            FileInfo[] files = di.GetFiles("*.*");

            foreach (FileInfo file in files)
            {
                string path = file.FullName.Substring(folder_length, file.FullName.Length - folder_length);
                this.lb_result.Items.Add(file.FullName.Substring(folder_length, file.FullName.Length - folder_length));
                this.lb_result.SetSelected(this.lb_result.Items.Count - 1, true);

                if (!System.IO.File.Exists(folder_old + path))
                {
                    if (!Directory.Exists(folder_export + path.Substring(0, path.LastIndexOf("\\"))))
                    {
                        Directory.CreateDirectory(folder_export + path.Substring(0, path.LastIndexOf("\\")));
                    }

                    //复制文件
                    System.IO.File.Copy(file.FullName, folder_export + path, true);

                    //生成日志文件
                    FileObj.FileAdd(logpath, "新增文件 -> " + path + "\r\n");

                    continue;
                }

                if (!FileCompare(file.FullName, folder_old + path))
                {
                    if (!Directory.Exists(folder_export + path.Substring(0, path.LastIndexOf("\\"))))
                    {
                        Directory.CreateDirectory(folder_export + path.Substring(0, path.LastIndexOf("\\")));
                    }

                    //复制文件
                    System.IO.File.Copy(file.FullName, folder_export + path, true);

                    //生成日志文件
                    FileObj.FileAdd(logpath, "修改文件 -> " + path + "\r\n");

                    continue;
                }
            }

            foreach (DirectoryInfo directory in di.GetDirectories())
            {
                if (directory.Name != ".svn")
                {
                    getallfiles(directory);
                }
            }
        } 

        private bool FileCompare(string file1, string file2)
        {
            //判断相同的文件是否被参考两次。
            if (file1 == file2)
            {
                return true;
            }

            int file1byte = 0;
            int file2byte = 0;

            using (FileStream fs1 = new FileStream(file1, FileMode.Open), fs2 = new FileStream(file2, FileMode.Open))
            {
                //检查文件大小。如果两个文件的大小并不相同，则视为不相同。
                if (fs1.Length != fs2.Length)
                {
                    //关闭文件。
                    fs1.Close();
                    fs2.Close();

                    return false;
                }

                //逐一比较两个文件的每一个字节，直到发现不相符或已到达文件尾端为止。
                do
                {
                    //从每一个文件读取一个字节。
                    file1byte = fs1.ReadByte();
                    file2byte = fs2.ReadByte();
                }

                while ((file1byte == file2byte) && (file1byte != -1));

                //关闭文件。
                fs1.Close();
                fs2.Close();
            }

            //返回比较的结果。在这个时候，只有当两个文件的内容完全相同时，"file1byte" 才会等于 "file2byte"。
            return ((file1byte - file2byte) == 0);
        }
    }
}