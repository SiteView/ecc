////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 14:52
//
////////////////////////////////////////////////////////////////////
 
using System;

namespace DataTransfer
{
	/// <summary>
	/// Description of EmailSetting.
	/// </summary>
	public class EmailSetting
	{
		public EmailSetting(string name,string email)
		{
			this.id = Guid.NewGuid().ToString();
			this.name = name;
			this.email = email;
		}
		
		private string id;
		public string ID
		{
			get {return id;}
			set {id = value;}			
		}
	
		private string name;
		public string Name
		{
			get {return name;}
			set {name = value;}			
		}
		
		private string email;
		public string Email
		{
			get {return email;}
			set {email = value;}			
		}
		
		private bool disable;
		public bool Disable
		{
			get {return disable;}
			set {disable = value;}			
		}
		
		private string template;
		public string Template
		{
			get {return template;}
			set {template = value;}
		}
		
		public string Section;
		
		public string Schedule;
	}
}
