#!/usr/bin/ksh
# 获取主机系统配置
OS=`lsb_release -i`
OSNAME=${OS#*:}
TRIMOS=${OSNAME#"${OSNAME%%[![:space:]]*}"}

CmdLists={"/bin/df -k","/bin/df -k /dev/mapper/VolGroup00-LogVol00 ","/usr/bin/free -b"}

xml="ssh.xml"

echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" > "$xml"
echo "<feed>" >> "$xml"

echo "Current OS is ${TRIMOS}."
echo "<OS>${TRIMOS}</OS>"  >> "$xml"


#redhat
for i in {"/bin/df -k",\
"/bin/df -k /dev/mapper/VolGroup00-LogVol00 ",\
"/usr/bin/free -b","/usr/bin/vmstat -n 3 2",\
"/usr/bin/top n 1 d 2 b -p 1",\
"/bin/ps -ef",\
"/bin/ps -e -o vsz=MEMSIZE -o args=COMMAND",\
"echo ssDateStart; /bin/date -u +\"%m/%d/%Y %H:%M:%S\"; /bin/date +\"%m/%d/%Y %H:%M:%S\"",\
"/bin/ls  --full-time  -l /etc  | /bin/grep -v \"^total\" | /bin/grep -v \"^d\" | /bin/sed 's/  */!/g' | /bin/cut -d! -f5\,6\,7\,8\,9"}
do
   echo "$i runing."
   echo "<ssh><key>$i</key><result>"  >> "$xml"
   echo "$i"  > run.sh
   chmod +x run.sh
   bash ./run.sh  >> "$xml"
   echo "</result></ssh>"  >> "$xml" 
done

#aix
for i in {"/usr/bin/df -k",\
"/usr/bin/df -k /dev/hd4 ",\
"/usr/sbin/lsps -s",\
"/usr/bin/vmstat 3 2",\
"/usr/bin/top n 1 d 2 b -p 1",\
"/bin/ps -ef",\
"/bin/ps -e -o vsz=MEMSIZE -o args=COMMAND",\
"/usr/bin/date -u +\"%m/%d/%Y %H:%M:%S\"; /usr/bin/date +\"%m/%d/%Y %H:%M:%S\"",\
"/bin/ls  --full-time  -l /etc  | /bin/grep -v \"^total\" | /bin/grep -v \"^d\" | /bin/sed 's/  */!/g' | /bin/cut -d! -f5\,6\,7\,8\,9"}
do
   echo "$i runing."
   echo "<ssh><key>$i</key><result>"  >> "$xml"
   echo "$i"  > run.sh
   chmod +x run.sh
   bash ./run.sh  >> "$xml"
   echo "</result></ssh>"  >> "$xml" 
done

#hp-ux
for i in {"/usr/bin/df -kP",\
"/usr/bin/df -kP /dev/vgbak/lvbak",\
"/usr/sbin/swapinfo -ta",\
"/usr/bin/vmstat -s",\
"/usr/sbin/sar 3",\
"/bin/ps -ef",\
"/bin/ps -e -o vsz=MEMSIZE -o args=COMMAND",\
"/usr/bin/date -u +\"%m/%d/%Y %H:%M:%S\"; /usr/bin/date +\"%m/%d/%Y %H:%M:%S\"",\
"/bin/ls  --full-time  -l /etc  | /bin/grep -v \"^total\" | /bin/grep -v \"^d\" | /bin/sed 's/  */!/g' | /bin/cut -d! -f5\,6\,7\,8\,9"}
do
   echo "$i runing."
   echo "<ssh><key>$i</key><result>"  >> "$xml"
   echo "$i"  > run.sh
   chmod +x run.sh
   bash ./run.sh  >> "$xml"
   echo "</result></ssh>"  >> "$xml" 
done


#sunsolaris
for i in {"/usr/bin/df -k",\
"/usr/bin/df -k /dev/vgbak/lvbak",\
"/usr/sbin/swap -s",\
"/usr/bin/mpstat 3 1",\
"/usr/sbin/sar 3",\
"/bin/ps -ef",\
"/bin/ps -e -o vsz=MEMSIZE -o args=COMMAND",\
"echo ssDateStart; /usr/bin/date -u +\"%m/%d/%Y %H:%M:%S\"; /usr/bin/date +\"%m/%d/%Y %H:%M:%S\"",\
"/bin/ls  --full-time  -l /etc  | /bin/grep -v \"^total\" | /bin/grep -v \"^d\" | /bin/sed 's/  */!/g' | /bin/cut -d! -f5\,6\,7\,8\,9"}
do
   echo "$i runing."
   echo "<ssh><key>$i</key><result>"  >> "$xml"
   echo "$i"  > run.sh
   chmod +x run.sh
   bash ./run.sh  >> "$xml"
   echo "</result></ssh>"  >> "$xml" 
done


echo "</feed>" >> "$xml"



