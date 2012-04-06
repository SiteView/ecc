////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-23
//Time: 16:15
//
////////////////////////////////////////////////////////////////////
 
using System;

namespace DataTransfer
{
	/// <summary>
	/// Description of Counter.
	/// </summary>
	public struct Counter
	{
		public string Flag;
		public string Desc;
		
		public Counter(string flag,string desc)
		{
			Flag = flag;
			Desc = desc;
		}
	}
}
