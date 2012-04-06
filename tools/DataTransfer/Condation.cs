////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-23
//Time: 16:17
//
////////////////////////////////////////////////////////////////////
 
using System;

namespace DataTransfer
{
	/// <summary>
	/// Description of Condation.
	/// </summary>
	public class Condation
	{
		public string Operate;
		public string ParamName;
		public string ParamValue;
		public string Relation;
		
		public Condation(string paramName,string operate,string paramValue,string relation)
		{
			Operate = operate;
			ParamName = paramName;
			ParamValue = paramValue;
			Relation = relation;
		}
	}
}
