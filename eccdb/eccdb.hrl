
-record (subgroup, {
	id = null,
	parentid = null,
	name = null,
	description = null,
	user_name = null}).

-record (monitor, {
	id = null}).

-record (alert_log, {
	id = null,
	stype = null,
	name = null,
	monitor = null,
	receiver = null,
	title = null,
	time = null,
	result = null,
	content = null,
	alert_level = null,
	measurement = null,
	subgroup = null,
	responsetime = null,
	responder = null,
	responsecontent = null,
	cleartime = null,
	times = null,
	email = []}).

-record (alert_rule, {
	id = null,
	stype = null,
	skey = null,
	svalue = null}).

-record (configure, {
	id = null,
	name = null,
	section = null,
	skey = null,
	svalue = null}).

-record (counters, {
	id = null,
	name = null,
	isdefault = null}).

-record (machine, {
	id = null,
	name = null,
	host = null,
	login = null,
	password = null,
	trace = null,
	os = null,
	status = null,
	method = null,
	prompt = null,
	loginprom = null,
	passwdprom = null,
	secondprom = null,
	secondresp = null,
	initshell = null,
	remoteencoding = null,
	sshcommand = null,
	sshclient = null,
	sshport = null,
	disableconncaching = null,
	connlimit = null,
	version = null,
	keyfile = null,
	sshauthmethod = null,
	label = null,
	total = null,
	stype = null,
	pwdmode = null}).

-record (machine_label, {
	id = null,
	name = null,
	stype = null,
	idx = null,
	syslabel = null,
	ishide = null,
	svalue = null,
	treeindex = null,
	machine_label = null}).

-record (machine_other, {
	id = null,
	machine = null,
	name = null,
	svalue = null}).

-record (monitor_attribute, {
	id = null,
	monitor = null,
	name = null,
	svalue = null,
	stype = null}).

-record (monitor_group, {
	id = null,
	subgroup = null,
	monitor = null}).

-record (monitor_property, {
	id = null,
	monitor = null,
	name = null,
	svalue = null,
	stype = null}).

-record (monitorlog, {
	id = null,
	name = null,
	time = null,
	category = null,
	measurement = null,
	class = null,
	groupname = null}).

-record (monitorlog_value, {
	id = null,
	monitorlog = null,
	svalue = null,
	stype = null,
	name = null,
	unit = null}).

-record (operationlog, {
	id = null,
	create_time = null,
	record_state = null,
	ip = null,
	operate_time = null,
	operate_type = null,
	operate_objname = null,
	operate_objinfo = null,
	description = null,
	user_name = null}).

-record (schedule, {
	id = null,
	name = null,
	stype = null,
	sunday = null,
	monday = null,
	tuesday = null,
	wednesday = null,
	thursday = null,
	friday = null,
	saturday = null}).

-record (user_spl, {
	id = null,
	name = null,
	password = null,
	ldapserver = null,
	ldapsecurity = null,
	title = null,
	description = null,
	disabled = null,
	cpe = null,
	nt = null,
	unix = null}).

-record (user_info_right, {
	id = null,
	user_spl = null,
	subgroup = null,
	rights = null}).

-record (weekday, {
	id = null,
	day = null,
	time_start = null,
	time_end = null,
	enabled = null}).

