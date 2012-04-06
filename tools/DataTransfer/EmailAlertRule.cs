////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 14:54
//
////////////////////////////////////////////////////////////////////
 
using System;

namespace DataTransfer
{
	/// <summary>
	/// Description of EmailAlertRule.
	/// </summary>
	public class EmailAlertRule : AlertRule
	{
		//{mail_alert,["2204A581EA14826D5E5554BD08F6B5B98A7E7A97"],[],"Default"}
		//{mail_alert,["other"],"lianbing.wang@siteview.com","Default"}
		
		public string OtherEmailAddress;
		public string EmailAddress;
		
		public override Otp.Erlang.Object GetActionParam()
		{
			//Console.WriteLine("Email Alert Rule:");
			//Console.WriteLine(this.OtherEmailAddress);
			//Console.WriteLine(this.EmailAddress);
			
			return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
				new Otp.Erlang.Atom("mail_alert")
				,new Otp.Erlang.List(this.EmailAddress)
				,new Otp.Erlang.String(this.OtherEmailAddress)
				,new Otp.Erlang.String(this.Template)
			});
		}
		
		public override string Action
		{
			get {return "mailto";}
		}
	}
}
