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
	/// Description of GroupEntity.
	/// </summary>
	public class GroupEntity : EntityBase
	{
		public string OldDependOnMonitorId;
		public string NewDependOnMonitorId;
		public string Disable;
		
		public GroupEntity(Group group)
		{
			this.OldId = group.ID;
			this.Name = group.Name;
			this.OldDependOnMonitorId = group.DependsOn;
			//1正常good 2危险waring 3错误error
			if(group.DependsCondition == DependsCondition.Error)
				this.DependsCondation = "error";
			else if(group.DependsCondition == DependsCondition.Danger)
				this.DependsCondation = "warning";
			else
				this.DependsCondation = "good";
			this.Disable = group.Disable;
			this.Desc = group.Description;
		}
		
		public override Otp.Erlang.Object[] GetImportParam()
		{
			string parentId = this.Parent == null ? "0" : this.Parent.NewId;
			Otp.Erlang.Object[] param = new Otp.Erlang.Object[] {
	            	new Otp.Erlang.Atom(parentId),new Otp.Erlang.List(
							new Otp.Erlang.Object[] {
								new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
									new Otp.Erlang.Atom("name"),new Otp.Erlang.String(this.Name)
								})
								,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
									new Otp.Erlang.Atom("class"),new Otp.Erlang.Atom("group")
								})
								,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
									new Otp.Erlang.Atom("frequency"),new Otp.Erlang.Int(0)
								})
								,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
									new Otp.Erlang.Atom("depends_on"),new Otp.Erlang.String("none")
								})
								,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
									new Otp.Erlang.Atom("depends_condition"),new Otp.Erlang.String(this.DependsCondation)
								})
							}
						)
		        };
			return param;
		}
		
		public Otp.Erlang.Object[] GetUpdateDependOnParam()
		{
			Otp.Erlang.Object[] parames = new Otp.Erlang.Object[] {
				new Otp.Erlang.List(
					new Otp.Erlang.Object[] {
						new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
							new Otp.Erlang.Atom("id"),new Otp.Erlang.Atom(this.NewId)
						})
						,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
							new Otp.Erlang.Atom("parent"),new Otp.Erlang.Atom(this.Parent.NewId)
						})
						,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
							new Otp.Erlang.Atom("name"),new Otp.Erlang.String(this.Name)
						})
						,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
							new Otp.Erlang.Atom("class"),new Otp.Erlang.Atom("group")
						})
						,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
							new Otp.Erlang.Atom("frequency"),new Otp.Erlang.Int(0)
						})
						,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
							new Otp.Erlang.Atom("depends_on"),new Otp.Erlang.String(this.NewDependOnMonitorId)
						})
						,new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
							new Otp.Erlang.Atom("depends_condition"),new Otp.Erlang.String("good")
						})
					}
				)
			};
			
			return parames;
		}
	}
}
