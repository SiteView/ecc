////////////////////////////////////////////////////////////////////
//
//User: lianbing.Wang
//Date: 2010-8-20
//Time: 15:41
//
////////////////////////////////////////////////////////////////////
 
using System;
using System.Reflection;
using System.Collections.Specialized;
using System.Collections.Generic;

using SiteView.Ecc.Core.Models;

namespace DataTransfer
{
	/// <summary>
	/// Description of MonitorEntity.
	/// </summary>
	public class MonitorEntity : EntityBase
	{
		public string OldMonitorType;
		public string NewMonitorType;
		public NameValueCollection OldGood;
		public NameValueCollection OldError;
		public NameValueCollection OldWarning;
		public string NewGood;
		public string NewError;
		public string NewWarning;
		public NameValueCollection AdvanceParameter;
		public NameValueCollection Parameter;
		private Monitor monitor;
		public Monitor Monitor
		{
			get
			{
				return monitor;
			}
			set
			{
				monitor = value;
			}
		}
		
		public MonitorEntity(Monitor monitor)
		{
			this.OldId = monitor.ID;
			this.Name = monitor.Name;
			this.OldError = monitor.Error;
			this.OldWarning = monitor.Warning;
			this.OldGood = monitor.Good;
			this.Properties = monitor.Properties;
			this.State = monitor.State;
			
			this.Desc = monitor.Description;
			this.OldMonitorType = monitor.MonitorType;
			this.AdvanceParameter = monitor.AdvanceParameter;
			this.Parameter = monitor.Parameter;	
			this.monitor = monitor;
		}
				
		public override Otp.Erlang.Object[] GetImportParam()
		{
			Otp.Erlang.Object[] keyValueParams = getApiParamFromTemplate();
			
			Otp.Erlang.Object[] parames = new Otp.Erlang.Object[] {
	            	new Otp.Erlang.Atom(this.Parent.Parent.NewId),new Otp.Erlang.List(
							keyValueParams
							)
		        };
			return parames;
		}
		
		private Otp.Erlang.Object[] getApiParamFromTemplate()
		{
			Dictionary<string,NameValueCollection> configParam 
				= ConfigUtility.GetConfigParam(this.OldMonitorType);
			
			if(null == configParam)
				return null;
			
			int count = configParam.Count;
			Otp.Erlang.Object[] param = new Otp.Erlang.Object[count];
			int index = 0;
			foreach(string key in configParam.Keys)
			{
				param[index++] = buildTuple(key,configParam[key]);
			}
			
			return param;
		}
		
		private Otp.Erlang.Object buildTuple(string keyProperty,NameValueCollection valueConfig)
		{
			Otp.Erlang.Atom keyAtom = new Otp.Erlang.Atom(keyProperty);
			Otp.Erlang.Object valueObj = getValueByConfig(valueConfig);
			return new Otp.Erlang.Tuple(new Otp.Erlang.Object[]{keyAtom,valueObj});
		}
		
		private List<Condation> getCondationsFromCollection(NameValueCollection condationCollection)
		{
			if(null == condationCollection)
				return null;
			List<Condation> condationList = new List<Condation>();
			int count ;
			bool hasCondition = int.TryParse(condationCollection["sv_conditioncount"],out count);
			if(hasCondition)
			{
				for(int i = 1; i <= count; i++)
				{
					string paramName = condationCollection[ "sv_paramname" + i ];
					string operate = condationCollection[ "sv_operate" + i ];
					string paranValue = condationCollection[ "sv_paramvalue" + i ];
					string relation = condationCollection[ "sv_relation" + i ];
					condationList.Add(new Condation(paramName,operate,paranValue,relation));
				}
			}
			return condationList;
		}
		
		private List<Condation> getMappingCondations(List<Condation> condations)
		{
			if(null == condations)
				return null;
			
			Dictionary<string, string> condationMappingDict = ConfigUtility.GetAlertCondations(
				this.NewMonitorType);
			
			if(null == condationMappingDict)
				return null;
			
			List<Condation> mappingCondations = new List<Condation>();
			foreach(Condation condation in condations)
			{
				string origin = condation.ParamName;
				if(condationMappingDict.ContainsKey(origin))
				{
					condation.ParamName = condationMappingDict[origin];
					mappingCondations.Add(condation);
				}
			}
			
			return mappingCondations;
		}
		
		private Otp.Erlang.Object buildMappingCondations(List<Condation> mappingCondations)
		{
			if(null == mappingCondations)
				return new Otp.Erlang.List();
			int count = mappingCondations.Count;
			bool multi = count > 1;
			Otp.Erlang.Object[] param = null;
			if(multi)
			{
				param = new Otp.Erlang.Object[ count * 2 ];
				for(int i = 1 ; i <= count; i++ )
				{				
					Condation c = mappingCondations[ i - 1 ];
					int index = (i-1) * 2 ;
					int value;
					int.TryParse(c.ParamValue,out value);
					param[index] = new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom(c.ParamName)
						,new Otp.Erlang.Atom(c.Operate)
						,new Otp.Erlang.Int(value)
					});
					
					int relationIndex = 2 * i - 1 ;
					param[relationIndex] = new Otp.Erlang.Atom(c.Relation);
				}			
			}
			else
			{
				param = new Otp.Erlang.Object[ count ];
				for(int i = 0 ; i < count; i++ )
				{
					Condation c = mappingCondations[ i ];
					int value;
					int.TryParse(c.ParamValue,out value);					
					param[i] = new Otp.Erlang.Tuple(new Otp.Erlang.Object[] {
						new Otp.Erlang.Atom(c.ParamName)
						,new Otp.Erlang.Atom(c.Operate)
						,new Otp.Erlang.Int(value)
					});
				}
			}
			
			return new Otp.Erlang.List(param);
		}
		
		private Otp.Erlang.Object getAlertCondation(NameValueCollection condationCollection)
		{
			//从Error等中过滤出告警
			List<Condation> condations = getCondationsFromCollection(condationCollection);
			if(null == condations)
				return new Otp.Erlang.List();
			
			//映射
			List<Condation> mappingCondations = getMappingCondations(condations);
			
			return buildMappingCondations(mappingCondations);
		}
		
		private Otp.Erlang.Object getValueByConfig(NameValueCollection valueConfig)
		{
			string tag = valueConfig.Get("tag");
			
			if("counters" == tag)
			{
				return getMonitorCounters();
			}
			
			if("error" == tag)
			{
				//从错误中过滤
				return getAlertCondation(this.OldError);
			}
			
			if("warning" == tag)
			{
				return getAlertCondation(this.OldWarning);
			}
			
			if("good" == tag)
			{
				return getAlertCondation(this.OldGood);
			}
			
			string defaultValue = valueConfig.Get("defaultValue");
			Object value = getValue(valueConfig);
			
			switch(tag)
			{
				case "int":
					{
						return new Otp.Erlang.Int(
							Convert.ToInt32(checkNullOrEmpty(value)?defaultValue:value));
					}
				case "boolean":
					{
						return new Otp.Erlang.Atom(
							checkNullOrEmpty(value)?defaultValue:value.ToString().ToLower()); //Otp.Erlang.Boolean(str2bool(value.ToString(),defaultValue));
					}
				case "atom":
					return new Otp.Erlang.Atom(checkNullOrEmpty(value)?defaultValue:value.ToString());
					
				case "tuple":
					return null;
					
				case "list":
					return new Otp.Erlang.List();
					
				default:
					return new Otp.Erlang.String(value.ToString());
			}
		}
		
		private bool checkNullOrEmpty(object value)
		{
			if(null == value || string.IsNullOrEmpty(value.ToString()))
			   return true;
			return false;
		}
		
		private object getValue(NameValueCollection valueConfig)
		{
			try
			{	object result = null;
				
				string type = valueConfig.Get("type").ToLower();
				string property = valueConfig.Get("property");
				string defaultValue = valueConfig.Get("defaultValue");
				bool useMapping = valueConfig["useMapping"] != null;
				bool useFrequencytUnit = valueConfig["useFrequencytUnit"] != null;
				
				if(type == "property")
					result = getValueByProerty(property,this);
				
				if(type == "collection")
					result = getValueByNameValueCollection(property,this);
				
				if(type == "none")
					result = defaultValue;
				
				if(null != result)
				{
					if(useMapping)
					{
						string category = valueConfig["useMapping"];
						result = ConfigUtility.GetMappingValue(category,result.ToString());
					}
					
					if(useFrequencytUnit)
					{
						if("Second".Equals(valueConfig["useFrequencytUnit"]))
						{
							result = Convert.ToInt32(result) * 60 ;
						}
					}
				}
					
				return result == null ? defaultValue : result;
			}
			catch
			{
				return null;
			}
		}
		
		private Object getValueByNameValueCollection(string property,MonitorEntity monitor)
		{
			string[] objArr = property.Split('.');
			int len = objArr.Length;
			object collectionObj = getValueByProerty(string.Join(".",objArr,0,len-1),(Object)monitor);
			Object[] key = new object[] {objArr[len - 1]};
			MethodInfo m = collectionObj.GetType().GetMethod("Get",new Type[]{typeof(System.String)});
			return m.Invoke(collectionObj,key);
		}
		
		private Object getValueByProerty(string property,MonitorEntity monitor)
		{
			return getValueByProerty(property,(Object)monitor);
		}
		
		private Object getValueByProerty(string property,object obj)
		{
			object result = null;
			string[] objArr = property.Split('.');
			Type t = obj.GetType();
			result = t.GetProperty(objArr[0]).GetValue(obj,null);
			if(objArr.Length > 1)
				result = getValueByProerty(string.Join(".",objArr,1,objArr.Length-1),result);
			
			return result;
		}
		
		private Otp.Erlang.Object getMonitorCounters()
		{
			//判断是否是有计数器的监测器
			string result = string.Empty;
					
			List<Counter> counterList= getCounters();
			if(null == counterList)
				return new Otp.Erlang.List();
			
			int count = counterList.Count;
			Otp.Erlang.Tuple[] coutersTuple = new Otp.Erlang.Tuple[count];
			int i = 0;
			foreach ( Counter counter in counterList )
			{
				coutersTuple[i++] = new Otp.Erlang.Tuple(
					new Otp.Erlang.Object[] {
						new Otp.Erlang.String(counter.Flag)
						,new Otp.Erlang.String(counter.Desc)
					}
				);
			}
			return new Otp.Erlang.List(coutersTuple);
		}
		
		/// <summary>
		/// 获取映射集合中存在的计数器
		/// </summary>
		/// <returns></returns>
		private List<Counter> getCounters()
		{
			List<string> param = getParams();
			List<Counter> counterList = new List<Counter>();
			//获取监测器的Counter对象集合
			Dictionary<string,Counter> counterDict 
				= ConfigUtility.GetMonitorCounters(this.NewMonitorType);
			if(null != counterDict && param.Count > 0)
			{
				foreach(string p in param)
				{
					//与新版本的映射				
					if(counterDict.ContainsKey(p))
					{
						counterList.Add(counterDict[p]);
					}
				}
			}
			
			if(counterList.Count == 0)
				counterList = ConfigUtility.GetMonitorDefaultCounters(this.NewMonitorType);
			
			return counterList;
		}
		
		/// <summary>
		/// 取3个条件的参数，以便映射计数器
		/// </summary>
		/// <returns></returns>
		private List<string> getParams()
		{
			List<string> paramList = new List<string>();
			buildParam(paramList,this.OldGood);
			buildParam(paramList,this.OldError);
			buildParam(paramList,this.OldWarning);
			return paramList;
		}
		
		private void buildParam(List<string> paramList,NameValueCollection collection)
		{
			if(null == collection)
				return;
			
			if(null != collection["sv_conditioncount"])
			{
				int count ;
				bool hasCondition = int.TryParse(collection["sv_conditioncount"],out count);
				if(hasCondition)
				{
					for(int i = 1; i <= count; i++)
					{
						string param = collection[ "sv_paramname" + i ];
						if( !string.IsNullOrEmpty(param) && !paramList.Contains(param) )
							paramList.Add(param);
					}
				}
			}
		}
	}
}
