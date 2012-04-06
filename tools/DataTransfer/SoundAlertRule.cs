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
	/// Description of SoundAlertRule.
	/// </summary>
	public class SoundAlertRule : AlertRule
	{
		private string sound;
		public string Sound
		{
			get {return sound;}
			set {sound = value;}
		}
		
		public override string Action
		{
			get {return "sound";}
		}
		
		//{action_param,{sound_alert,"Default"}}
		public override Otp.Erlang.Object GetActionParam()
		{
			return new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
				new Otp.Erlang.Atom("sound_alert")
				,new Otp.Erlang.String(this.sound)
			});
		}
	}
}
