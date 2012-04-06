////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 15:40
//
////////////////////////////////////////////////////////////////////
 
using System;
using SiteView.Ecc.Core.Models;

namespace DataTransfer
{
	/// <summary>
	/// Description of RemoteServerEntity.
	/// </summary>
	public class RemoteServerEntity : EntityBase
	{
		public RemoteServerEntity(Entity entity)
		{
			this.OldId = entity.ID;
			this.Name = entity.Name;
			this.DependsCondation =entity.DependsCondition;			
			this.Properties = entity.Properties;
			
			this.Desc = entity.Description;
			this.DeviceType = entity.DeviceType;			
			this.DependsOn = entity.Dependson;			
		}
		
		public string DeviceType;
		public string DependsOn;
		
		public override Otp.Erlang.Object[] GetImportParam()
		{
			Otp.Erlang.Object[] param = new Otp.Erlang.Object[27];
			param[0] = new Otp.Erlang.Atom("machine");
			param[1] = new Otp.Erlang.Atom("id");
			param[2] = new Otp.Erlang.String(this.Name);
			string machine = getPropertyValueOrDefault("_MachineName","");
			string method = "SSH";
			string mappingDeviceType = ConfigUtility.GetDeviceTypeMapping(this.DeviceType);
			if(mappingDeviceType == "_win" )
			{
				method = "WMI";
			}
			param[3] = new Otp.Erlang.String(machine);
			param[4] = new Otp.Erlang.String(getPropertyValueOrDefault("_UserAccount",""));
			param[5] = new Otp.Erlang.String(getPropertyValueOrDefault("_PassWord",""));
			param[6] = new Otp.Erlang.String("0"); //trace
			
			string os = ConfigUtility.GetOsMapping(getPropertyValueOrDefault("_OsType","liunx"));
			param[7] = new Otp.Erlang.String(mappingDeviceType == "_win"
			                                 ? "nt" : os);  //os
			param[8] = new Otp.Erlang.String("unknown"); //status		 
			param[9] = new Otp.Erlang.String(method); //method
			param[10] = new Otp.Erlang.String(getPropertyValueOrDefault("_Prompt",""));  //prompt
			param[11] = new Otp.Erlang.String(getPropertyValueOrDefault("_LoginPrompt","")); //loginprom
			param[12] = new Otp.Erlang.String(getPropertyValueOrDefault("_PWPrompt","")); //passwdprom
			param[13] = new Otp.Erlang.String(""); //secondprom
			param[14] = new Otp.Erlang.String(""); //secondresp
			param[15] = new Otp.Erlang.String(""); //initshell
			param[16] = new Otp.Erlang.String("GBK");
			param[17] = new Otp.Erlang.String(""); //sshcommand
			param[18] = new Otp.Erlang.String("interJavalib"); //sshclient
			param[19] = new Otp.Erlang.Int(Convert.ToInt32(getPropertyValueOrDefault("_Port","0"))); //port
			param[20] = new Otp.Erlang.String("0"); //disableconncaching
			param[21] = new Otp.Erlang.Int(3);
			param[22] = new Otp.Erlang.String(""); //version
			param[23] = new Otp.Erlang.String(getPropertyValueOrDefault("_PriKeyPath",""));  //keyfile
			param[24] = new Otp.Erlang.String(""); //sshauthmethod
			param[25] = new Otp.Erlang.String(this.Desc); //label
			param[26] = new Otp.Erlang.Int(0);
			
			return new Otp.Erlang.Object[] { 
				new Otp.Erlang.Tuple(
					param		
				)
			};
		}
	}
}
