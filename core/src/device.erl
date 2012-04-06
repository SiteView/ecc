%% ---
%% device
%%
%%---
-module(device,[BASE]).
-extends(monitor_group).
-compile(export_all).
-include("monitor.hrl").
-include("monitor_template.hrl").

new()->
	Obj = monitor_group:new(),
	{?MODULE,Obj}.

reload()->
	THIS:stopGroup(),
	{ok,{?ID,Id}} = THIS:get_property(?ID),
	THIS:remove_childs(),
	%%THIS:remove_properties(),
	% THIS:set_property(?ID,Id),
	GrpData = dbcs_device:get_device(Id),
	THIS:init(THIS,GrpData),
	THIS:load_group(),
	THIS:startGroup(),
	{ok,reload_ok}.

%%load_group()->{ok,load_group_ok}
%%
load_group()->
	{ok,{id,Id}} = THIS:get_property(id),
	MonitorDatas = dbcs_device:get_childs(Id),
	%%io:format("Datas:~p~n",[MonitorDatas]),
	Monitors =[THIS:createObject(X)||X<-MonitorDatas], %%lists:map(fun(X)->M=THIS:create_object(X),M end,MonitorDatas),
	%%io:format("load_group,~p~n",[Monitors]),
	THIS:set_attribute(childs,Monitors),
	F = fun(X)->
			X:set_owner(THIS),
			case X:get_property(?CLASS) of
				{ok,{?CLASS,group}}->
					X:load_group();
				{ok,{?CLASS,device}}->
					X:load_group();
				_->
					ok
			end
		end,
	lists:foreach(F,Monitors),
	{ok,load_group_ok}.

getScalarValues(Prop,Params)->
	case Prop of
		'Device.LAN. AddressingType'->
			["DHCP(ID12 on,ID19off)","Fixed(ID12 off,ID19off)","PPPoE(ID12 off,ID19on)"];
		_->
			BASE:getScalarValues(Prop,Params)
	end.

get_template_property()->
	[
		#property_category{
			name='Basic',
			title="基本配置",
			properties= [
				#property_category{
					name='Network',
					title="网络",
					properties=[
						#property{name='Device.GateWay.Network.HostName',title="主机名",type=text,editable=true},
						#property{name='Device.LAN.IPAddress',title="本地IP地址",type=text,editable=true},
						#property_category{
								name='ETH3',
								title="网口三",
								properties=[
									#property{name='Device.LAN.MACAddress',title="MAC 地址",type=text,editable=false},
									#property{name='Device.LAN.AddressingType',title="网络类型",type=scalar,editable=true},
									#property{name='Device.LAN.IPAddress',title="IP 地址",type=text,editable=true},
									#property{name='Device.LAN.SubnetMask',title="子网掩码",type=text,editable=true},
									#property{name='Device.LAN.DefaultGateway',title="网关地址",type=text,editable=true}
									]
							},
							#property_category{
								name='PPPOE',
								title="PPPOE",
								properties=[
									#property{name='Device.GateWay.Network.PppoeUsername',title="用户名",type=text,editable=true},
									#property{name='Device.GateWay.Network.PppoePassword',title="密码",type=text,editable=true}
									]
							},
							#property_category{
								name='DNS',
								title="域名解析服务器",
								properties=[
									#property{name='Device.GateWay.Network.DnsEnable',title="启用",type=bool,editable=true},
									#property{name='Device.LAN.DNSServers',title="首选服务器,备用服务器",type=text,editable=true}
									]
							},
							#property_category{
								name='SNTP',
								title="时间服务器",
								properties=[
									#property{name='Device.Time.NTPServer1',title="首选服务器",type=text,editable=true},
									#property{name='Device.Time.NTPServer2',title="备用服务器",type=text,editable=true},
									#property{name='Device.Time.LocalTimeZone',title="时区",type=scalar,editable=true}
									]
							}
						]
				},
				#property_category{
					name='System',
					title="系统",
					properties = [
						#property{name='Device.Services.VoiceService.{i}.Capabilities.Codecs.{i}.Codec',title="编解码",description="可选：G729A/20,G723/30,PCMU/20,PCMA/20,iLBC/30,GSM/20",type=text,editable=true},
						#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.DTMFMethod',title="DTMF 传输方式",type=scalar,editable=true},
						#property{name='Device.Services..VoiceService.{i}.VoiceProfile.{i}.RTP.TelephoneEventPayloadType',title="2833 负载类型",description="96~127，缺省值为100。用户在配置时需将该参数与对端(如：软交换平台)支持的 2833 包类型值设置成一致",type=text,editable=true},
						#property{name='Device.GateWay.System.DtmfOnTime',title="DTMF 信号保持",description="80~150(毫秒)，缺省值为100。拨出号码的信号发送持续时间",type=text,editable=true},
						#property{name='Device.GateWay.System.DtmfOffTime',title="DTMF 信号间隔",description="80~150(毫秒)，缺省值为100。相邻号码间的信号间隙",type=text,editable=true}
					]
				},
				#property_category{
					name='SIP',
					title="SIP",
					properties = [
						#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.SIP.UserAgentPort',title="本地端口",description="1~9999，缺省值为5060",type=text,editable=true},
						#property{name='Device.Services..VoiceService.{i}.VoiceProfile.{i}.SIP.RegistrarServer',title="注册服务器",description="例：168.33.134.50:5060 或 www.sip.com:5060",type=text,editable=true},
						#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.SIP.OutboundProxy',title="IMS核心网入口",description="例：168.33.134.51:5000 或 www.sipproxy.com:5000",type=text,editable=true},
						#property{name='Device.GateWay.Capabilities.SIP. BackupProxyServer',title="IMS核心网备份入口",description="例：168.33.134.53:5060",type=text,editable=true},
						#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.SIP.UserAgentDomain',title="客户端域名",description="例：www.gatewaysip.com",type=text,editable=true},
						#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.SIP.RegistrationPeriod',title="注册时长",description="15~86400(秒)",type=text,editable=true}
					]
				},
				#property_category{
					name='TDM',
					title="TDM",
					properties = [
						#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Tdm.{i}.Ds1Type',title="DS1 类型",type=radio,editable=true},
						#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Tdm.{i}.Ds0Type',title="PCM 编码",type=radio,editable=true},
						#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Tdm.{i}.Clock',title="同步方式",type=scalar,editable=true},
						#property_category{
							name='TDM1',
							title="TDM1",
							properties = [
								#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Tdm.{i}.LineType',title="帧格式",type=scalar,editable=true},
								#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Tdm.{i}.LineCode',title="线路码",type=scalar,editable=true},
								#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Tdm.{i}.Length',title="线缆距离",type=scalar,editable=true},
								#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Tdm.{i}.DigitAjust',title="号码转换",type=textarea,editable=true}
							]
						}
					]
				}
			]
		},
		#property_category{
			name='ISDN',
			title="ISDN",
			properties= [
				#property_category{
					name='ISDN 1',
					title="ISDN 1",
					properties=[
						#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.Name',title="名称",type=text,editable=true},
						#property{name='Device.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.Enable',title="启用",type=check_grouop,editable=true,properties=[
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.ReceivingOverlaping',title="收号方式",type=bool,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.Dchannel',title="D 通道",type=radio,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.InterfaceType',title="接口协议",description="该设置必须与对端设备相对应，一边是网络侧，另一边是用户侧",type=radio,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.IsdnStandard',title="信令标准",description="一般 T1 采用 NI-2，E1 采用 CCITT",type=scalar,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.CircuitHunting',title="选线方式",type=scalar,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.DchServiceMessage',title="D 通道启用维护消息",type=bool,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.AllowNoCdpn',title="点对点连接",description="无需被叫号和通道号",type=bool,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.CpnCategory',title="主叫号码类型",description="设置CPN的Category子域",type=radio,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.CpnPresentation',title="主叫号码显示权限",description="设置CPN的Presentation子域",type=bool,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{I}.Isdn.{i}.CdpnCategory',title="被叫号码类型",description="设置CDPN被叫Category",type=radio,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.BusyPrompt',title="被叫忙线处理",type=radio,editable=true},
							#property{name='Device.Services.VoiceService.{i}.VoiceProfile.{i}.Isdn.{i}.CidExclusive',title="允许对端更换线路",description="在CID中选择Exclusive",type=bool,editable=true}
							]
						}
					]
				}
			]
		}				

	].