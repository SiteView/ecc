////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 15:47
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Collections.Specialized;

using SiteView.Ecc.Core.Models;

namespace DataTransfer
{
	/// <summary>
	/// Description of TaskSchedule.
	/// </summary>
	public class TaskSchedule
	{
		public NameValueCollection Properties;
		public TaskSchedule.TaskScheduleTime NewType;
		public string Name;
		
		public TaskSchedule(Task task)
		{
			this.Name = task.Name;
			this.Properties = task.Properties;
			if(Task.TaskType.RegionalTime == task.Type)
				this.NewType = TaskScheduleTime.Range;
			else if(Task.TaskType.AbsoluteTime == task.Type)
				this.NewType = TaskScheduleTime.Absolute;
			else
				this.NewType = TaskScheduleTime.Relative;
		}
		
		public enum TaskScheduleTime
		{
			Absolute
			,Relative
			,Range
		}
	}
}
