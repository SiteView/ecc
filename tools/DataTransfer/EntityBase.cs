////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 15:41
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Collections.Specialized;

namespace DataTransfer
{
	/// <summary>
	/// Description of EntityBase.
	/// </summary>
	public abstract class EntityBase
	{
		public string NewId;
		public string OldId;
		public string Name;
		public string State;
		public string DependsCondation;
		public string Desc;
		private EntityBase parent;
		public EntityBase Parent
		{
			get{return parent;}
			set{parent = value;}
		}
		private NameValueCollection properties;
		public NameValueCollection Properties
		{
			get
			{
				return properties;
			}
			set
			{
				properties = value;
			}
		}
		
		public string Path
		{
			get
			{
				string parentPath = null == this.parent ? string.Empty : this.parent.Path;
				return string.Format("{0}/{1}",parentPath,this.Name);
			}
		}
		
		public abstract Otp.Erlang.Object[] GetImportParam();
		
		protected string getPropertyValue(string propertyName)
		{
			if(null == this.Properties || string.IsNullOrEmpty(propertyName))
				return string.Empty;
			
			return this.Properties[propertyName];
		}
		
		protected string getPropertyValueOrDefault(string propertyName,string defaultValue)
		{
			string value = getPropertyValue(propertyName);
			if(string.IsNullOrEmpty(value))
				return defaultValue;
			return value;
		}
	}
}
