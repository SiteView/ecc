////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 14:28
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Collections.Generic;

namespace DataTransfer
{
	/// <summary>
	/// Description of ErlangEntity.
	/// </summary>
	public class ErlangEntity {
	private Dictionary<string,Otp.Erlang.Object> dict = null;
	public ErlangEntity(Otp.Erlang.Object erlObj)
	{
		if(!(erlObj is Otp.Erlang.List))
			throw new Exception("实体不是Erlang中的List");
			
		dict = new Dictionary<string,Otp.Erlang.Object>();
		foreach(Otp.Erlang.Object element in ((Otp.Erlang.List)erlObj).elements())
		{
			Otp.Erlang.Object[] tuple = ((Otp.Erlang.Tuple)element).elements();
			dict.Add(tuple[0].ToString(),tuple[1]);
		}
	}
	
	public String GetPropertyStr(String propertyName) {
		if(!dict.ContainsKey(propertyName))
			return "";
		
		Otp.Erlang.Object value = dict[propertyName];
		if(value == null)
			return "";
		
		return TrimSpecialChar(value.ToString());
	}
	
	public Dictionary<string,Otp.Erlang.Object> Dict
	{
		get {return dict;}
	}
	
	public static string TrimSpecialChar(string erlangStr)
	{
		return System.Text.RegularExpressions.Regex.Replace(erlangStr, @"^[[{""']|[\]}""']$","");
	}
	
	public static bool IsOk(Otp.Erlang.Object temp)
	{
		Otp.Erlang.Object[] erlObj = ((Otp.Erlang.Tuple)temp).elements();
		bool result = erlObj !=null && erlObj.Length == 2 && String.Compare(erlObj[0].ToString(),"ok",true) == 0;
		
		return result;
	}
	
	public override string ToString()
	{
		string str = string.Empty;
		foreach( string key in dict.Keys )
        {
			str += string.Format("{0} --- {1};", key, GetPropertyStr(key));
		}
		return str;
	}
}
}
