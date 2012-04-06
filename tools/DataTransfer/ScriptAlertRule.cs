////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 14:55
//
////////////////////////////////////////////////////////////////////
 
using System;

namespace DataTransfer
{
	/// <summary>
	/// Description of ScriptAlertRule.
	/// </summary>
	public class ScriptAlertRule : AlertRule
	{
		//{action_param,{script_alert,["192.168.5.3"],["a"],["param"],"Default"}}
		//{action_param,{script_alert,[[]],["dirdir.bat"],["param"],"Default"}}
		
		public string Server;
		public string Script;
		public string Param;
		
		public override Otp.Erlang.Object GetActionParam()
		{
			throw new NotSupportedException();
			
			/*
			return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
				new Otp.Erlang.Atom("script_alert")
				,new Otp.Erlang.List(new Otp.Erlang.String(this.Server))
				,new Otp.Erlang.List(new Otp.Erlang.String(this.Script))
				,new Otp.Erlang.List(new Otp.Erlang.String(this.Param))
				,new Otp.Erlang.String(this.Template)
			});
			*/
		}
		
		public override string Action
		{
			get {return "script";}
		}
	}
}
