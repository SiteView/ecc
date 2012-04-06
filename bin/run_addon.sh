#!/bin/sh
#
#
#
################################################################################
#                                                                              #
#                       User customizable settings                             #
#                                                                              #
#                                                                              #
################################################################################
#
#  Manual installation README:
#   If installing the addon manually, you have to modify this script so that
#   the addon will be called with proper parameters.
#
#   Step 1: enter path variables.
#   
#
#   The path variables are used to construct the LD_LIBRARY_PATH and the 
#   SHLIB_PATH environment variables. Instead of setting these (very important)
#   environment variables manually, you should provide location of every
#   important subsystem. The variables will be set for you.
#

#   Name		Description			Example
#   ------------------- ------------------------------- ---------------------
#   OVO_DIR		OVO libraries 			/opt/OV/lib
#   INSTALLATION_DIR	Where the Addon is installed	/opt/topaz/hp_ovo_addon
#
#
#   Following variables can be provided by the dependency info that is stored
#   inside the OVO_DIR libraries. Do not define these unless you have to.
#
#   Name		Description			Example
#   ------------------- ------------------------------- ---------------------
#   JAVA_DIR		Where the Java libraries are 	/usr/java1.1/lib	
#   ORACLE_DIR		Where the Oracle client libs 
#
#
#  Step 2. Set the hostname/port for the sitescope machine.
# 
#  Set the value of the SIS_HOST variable to the IP address of the sitescope machine.
#
#  If the TCP port was changed from the default value of 9000 to something else, change
#  the PORT variable according to the sitescope monitor settings.
#
#  Note: you should set the IP address in dots and numbers format, not the symbolic
#     name. Use of symbolic name depends on properly configured DNS/YP systems, which
#     are not always installed on the production servers.
#
#  Step 3. Test the run_addon.sh script.
#  Run the script. If everyting is ok, you will see lines: 
# 
# ./run_addon.sh
#     Starting addon as background process
#     Started successfully
#  
#
#  Appendix: Troubleshooting
#  
#  The log files can provide you with a lot of useful information about the addon process.
#  
#
#  If something is wrong, first thing you should do is to locate the log file that the 
#  Addon writes as it operates. By default the log file(s) will be written in the
#  /tmp/hpovo_ito_addon ; you can override this behaviour by setting the LOG_DIR variable
#  below. 
# 
#  Note: The common shortcuts for seeing the addon log are:
#		
#		 cat `ls -rt /tmp/hpovo_ito_addon/hpovo_ito_addon*|tail -1` 
#	and 
#		 tail -f `ls -rt /tmp/hpovo_ito_addon/hpovo_ito_addon*|tail -1`
#
#  1. If the log file contains line similar to:
#     "Can not connect to remote machine 192.168.81.52 port 9000" (or other IP),
#     then either the sitescop host IP in the run_addon.sh script is wrong, or there is no sitescope monitor 
#     running  on the IP/port combination. Verify the IP address.
#
#  2. If the log file complains about unresolved symbols, then there is an error with 
#     the LD_LIBRARY_PATH/SHLIB_PATH (locations of the dynamic libraries). Verify that the OVO_DIR
#     path is correct; i.e. points to location of HP OVO _libraries_. Assuming that the OVO is installed
#     under /opt/OV, the correct value for the OVO_DIR would be /opt/OV/lib and not /opt/OV
#  
#  3. If the log file complains that the tp_hpovo_addon is not found, verify that the 
#     INSTALLATION_DIR is correct. The INSTALLATION_DIR should point to the location of the
#     addon binary.
# 
#
#  Debugging the dynamic path problems. 
# 
#  Uncomment the line with RUN_ADDON_DEBUG_MODE=true. Run the script. 
#  The addon will not be executed. Instead, the environment variables 
#  will be dumped on the screen and simple sanity test will be executed. 
#  You will see something like:
#
#        /usr/lib/libxti.2 =>    /usr/lib/libxti.2
#        /usr/lib/libnsl.1 =>    /usr/lib/libnsl.1
#  
#  Make sure that the sanity test will not issue errors like:
#   "Can't find path for shared library: libopcsv_r.sl"
#  
#  If the output contains such lines, there is a problem with OVO_DIR variable. 


# sitescope host name or IP address (No default)
SIS_HOST=

# sitescope monitor port (default 9000.) IF you change the port, update sitescope monitor configuration 
PORT=9000

# additional fields to be sent to topaz. list of OPCDATA fields to be extracted from incoming
# alerts and sent to topaz. 
ADDITIONAL_FIELDS="OPCDATA_SEVERITY OPCDATA_RECEIVE_TIME OPCDATA_AACTION_ANNOTATE OPCDATA_AACTION_STATUS OPCDATA_UNMATCHED OPCDATA_APPLICATION OPCDATA_GROUP OPCDATA_MSGTEXT OPCDATA_ORIGMSGTEXT OPCDATA_NODENAME OPCDATA_OBJECT OPCDATA_SERVICE_NAME OPCDATA_MSG_KEY_RELATION OPCDATA_ACKNOWLEDGE_OP OPCDATA_MSG_LOG_ONLY "

# Maximal time period (in seconds) between two sequential heatbeat
# reports. Default is 45 seconds
HEARTBEAT_TIMEOUT=45

# Location of this script file (default is /opt/topaz/hp_ovo_addon)
INSTALLATION_DIR=/opt/topaz/hp_ovo_addon/

# Location of HP OVO server software.
OVO_DIR=/opt/OV/lib

# Location of Java 1.1 software (default is /usr/java1.1/lib)
JAVA_DIR=/usr/java1.1/lib

# Location of Oracle software
ORACLE_DIR=

# Debug level.
# Possible debug levels are: DEBUG/INFO/WARNING/ERROR.
# Default is ERROR
DEBUG_LEVEL=ERROR


# Log directory
# Default is /tmp/hpovo_ito_adddon
LOG_DIR=/tmp/hpovo_ito_addon

################################################################################
#                                                                              #
#        End of user customizable settings. Do not change lines below!!!       #
#                                                                              #
#                                                                              #
################################################################################

# uncomment this line to enable run_addon.sh debug
#RUN_ADDON_DEBUG_MODE=true
PATH=/usr/bin
READ_QUEUE_NAME=TopazEms
READ_QUEUE_SIZE=0

LD_LIBRARY_PATH=/usr/lib/sparcv9:/lib:${OVO_DIR}:${JAVA_DIR}:${ORACLE_DIR}:/usr/lib
SHOULD_B_UP_FILE_NAME=${INSTALLATION_DIR}/.topaz_addon_should_be_up
# SHLIB_PATH is HP-UX synonym of the LD_LIBRARY_PATH. 
SHLIB_PATH=${LD_LIBRARY_PATH}
# export both variables, so that the script will be portable to HP-UX
export LD_LIBRARY_PATH SHLIB_PATH


exists_but_not_dir() {
	for type in f c b p ; do
		test -${type} ${1} > /dev/null 2>&1 && return 0 
	done
	return 1 
}




should_be_up_file_exists() {
	if [ ! -f ${SHOULD_B_UP_FILE_NAME} ] ; then
		return 1
	else
		return 0
	fi
}

run_addon() {
	if [ ! -d ${LOG_DIR} ] ; then
		while exists_but_not_dir ${LOG_DIR} ; do 
			LOG_DIR="${LOG_DIR}_"
		done
	fi
	
	mkdir -p ${LOG_DIR} >/dev/null 2>&1
	
	DATE_FMT="hpovo_ito_addon_%Y-%b-%d--%T.log"
	LOG_FILE=`date "+${DATE_FMT}" 2>/dev/null`
	[ "x${LOG_FILE}" = "x" ] &&LOG_FILE="hpovo_ito_addon_latest.log"
	
	NORMALIZED_DEBUG_LEVEL=`echo ${DEBUG_LEVEL:-ERROR}| tr '[:lower:]' '[:upper:]' 2>/dev/null`
	case ${NORMALIZED_DEBUG_LEVEL} in 
		"DEBUG")	DBG_LVL=0 ;;
		"INFO") 	DBG_LVL=1 ;;
		"WARNING")	DBG_LVL=2 ;;
		"ERROR")	DBG_LVL=3 ;;
		*)		DBG_LVL=3 ;;
	esac
	
	LOG_PATH=${LOG_DIR}/${LOG_FILE}
	##### DEBUG { ####
	if ${RUN_ADDON_DEBUG_MODE:-false} ; then
		echo "==== ADDON DEBUG MODE - DUMPING ENVIRONMENT VARIABLES ===="
		VAR_LIST="DEBUG_LEVEL NORMALIZED_DEBUG_LEVEL DBG_LVL LD_LIBRARY_PATH HEARTBEAT_TIMEOUT OVO_DIR ORACLE_DIR JAVA_DIR READ_QUEUE_NAME SIS_HOST PORT"
		for var in  ${VAR_LIST} ; do
			eval echo "${var} is \"\$${var}\""
		done
		echo "==== ADDON DEBUG MODE - SHOWING DYNAMIC Linker Dependencies"
		PATH=${PATH}:/usr/ccs/bin
		ldd ${INSTALLATION_DIR}/tp_hpovo_addon
		echo "==== ADDON DEBUG MODE - EXITING ===="
		exit 0 
	fi		
	##### } ##########
	
	#TROUBLESHOOTING_FLAGS="-f"
	echo "Log started on  `date`" > ${LOG_PATH}
	ADDON_STR="addon as background process"
	if [ -n "${TROUBLESHOOTING_FLAGS}" ] ; then
		ADDON_STR="addon with troubleshooting flags ${TROUBLESHOOTING_FLAGS}"
	fi
	echo "Starting ${ADDON_STR}"
	SUCCESS=true
	
	for field in ${ADDITIONAL_FIELDS} ; do 
		PROCCESSED_LIST="${PROCCESSED_LIST} -n${field}" 
	done
	
	${INSTALLATION_DIR}/tp_hpovo_addon 		\
		${TROUBLESHOOTING_FLAGS} 		\
		-r${READ_QUEUE_NAME} 			\
		-t${HEARTBEAT_TIMEOUT} 			\
		-d${DBG_LVL} 				\
		-s${READ_QUEUE_SIZE} 			\
		${PROCCESSED_LIST}			\
		${SIS_HOST}:${PORT} > ${LOG_PATH} 2>&1 || SUCCESS=false
	
	${SUCCESS} && sleep 2
	${SUCCESS} && ps -aef | grep tp_hpovo_addon >/dev/null 2>&1 || SUCCESS=false
	${SUCCESS} && grep -v ERROR ${LOG_PATH} >/dev/null 2>&1 || SUCCESS=false
	${SUCCESS} && grep Exiting ${LOG_PATH} >/dev/null 2>&1 && SUCCESS=false
	if ${SUCCESS} ; then
		echo "Started successfully"
	else 
		echo "Error starting ${ADDON_STR}" 
		tail -1  ${LOG_PATH}
	fi
}

# create the file
touch ${SHOULD_B_UP_FILE_NAME}
while should_be_up_file_exists ; do
	# get the pid list
	PID_LIST=`ps -ef|grep tp_hpovo_addon|grep -v grep|awk '{print $2}'`
	PROCESS_EXISTS=1
	for PID in ${PID_LIST} ; do
		PROCESS_EXISTS=0
	done

	if [ ${PROCESS_EXISTS} -ne 0 ] ; then
		#respawn process
		(run_addon)&
	fi
	sleep ${HEARTBEAT_TIMEOUT}
done
