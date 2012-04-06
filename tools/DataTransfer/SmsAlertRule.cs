////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 14:56
//
////////////////////////////////////////////////////////////////////
 
using System;

namespace DataTransfer
{
	/// <summary>
	/// Description of SmsAlertRule.
	/// </summary>
	public class SmsAlertRule : AlertRule
	{
		private SmsSendMode smsSendMode;
		public SmsSendMode SmsSendMode
		{
			get {return smsSendMode;}
			set {smsSendMode = value;}
		}
		
		public string PhoneNumber;
		
		public override string Action
		{
			get {return "sms";}
		}
		
		//Web方式不导入，Com->GSM Smsdll.dll->DLL
		//{action_param,{sms_alert,["1101101101"],"Web",[[]],[[]],[[]],[],"Default",undefined}}
		public override Otp.Erlang.Object GetActionParam()
		{
			if(SmsSendMode.Web == smsSendMode)
				throw new NotSupportedException();
			
			//Console.WriteLine(this.PhoneNumber);
			//Console.WriteLine(this.smsSendMode.ToString());
			
			if(SmsSendMode.GSM == smsSendMode || SmsSendMode.DLL == smsSendMode)
			{
				return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
					new Otp.Erlang.Atom("sms_alert")
					,new Otp.Erlang.List(new Otp.Erlang.String(this.PhoneNumber))
					,new Otp.Erlang.String(this.smsSendMode.ToString())
					,new Otp.Erlang.List(new Otp.Erlang.String(""))
					,new Otp.Erlang.List(new Otp.Erlang.String(""))
					,new Otp.Erlang.List(new Otp.Erlang.String(""))
					,new Otp.Erlang.String("")
					,new Otp.Erlang.String(this.Template)
					,new Otp.Erlang.Atom("undefined")
				});
			}
				
			throw new NotSupportedException();
		}
	}
}
