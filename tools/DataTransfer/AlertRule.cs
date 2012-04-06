////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 14:53
//
////////////////////////////////////////////////////////////////////
 
using System;

namespace DataTransfer
{
	/// <summary>
	/// Description of AlertRule.
	/// </summary>
	public abstract class AlertRule
	{
		private string name;
		private string[] targets;
		private bool enable;
		private string template;
		
		public string Name
		{
			get {return name;}
			set {name = value;}
		}
		//"<0.1.2>"
		public string[] Targets
		{
			get {return targets;}
			set {targets = value;}
		}
		public abstract string Action
		{
			get;
		}
		public bool Enable
		{
			get {return enable;}
			set {enable = value;}
		}
		public string Template
		{
			get {return template;}
			set {template = value;}
		}
		
		public string Section;
		public string Category;
		
		public string AlwayTimes;
		public string OnlyTimes;
		public string SelTimes1;
		public string SelTimes2;
		
		public string AlertCond;
		
		public abstract Otp.Erlang.Object GetActionParam();
		
		//{once,1},{always,1},{select,{1,1}}
		public Otp.Erlang.Object GetCondation()
		{
			if("1" == AlertCond)
			{
				return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
					new Otp.Erlang.Atom("always")
					,new Otp.Erlang.Int(Convert.ToInt32(AlwayTimes))
				});
			}
			
			if("2" == AlertCond)
			{
				return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
					new Otp.Erlang.Atom("once")
					,new Otp.Erlang.Int(Convert.ToInt32(OnlyTimes))
				});
			}
			
			if("3" == AlertCond)
			{
				return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
					new Otp.Erlang.Atom("select")
					,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Int(Convert.ToInt32(SelTimes1))
						,new Otp.Erlang.Int(Convert.ToInt32(SelTimes2))
					})
				});
			}
			
			throw new InvalidOperationException();
		}
	}
}
