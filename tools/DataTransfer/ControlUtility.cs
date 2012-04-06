////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 16:14
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Windows.Forms;

namespace DataTransfer
{
	/// <summary>
	/// Description of ControlUtility.
	/// </summary>
	public sealed class ControlUtility
	{
		public ControlUtility()
		{
		}	
		
		public static void Enable(Control[] controls,bool enable)
		{
			if(null == controls)
				return;
			foreach(Control control in controls)
				control.Enabled = enable;
		}
	}
}
