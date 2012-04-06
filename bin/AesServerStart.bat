@echo off

echo Starting Aes Server...

echo off
set AES_SCRIPT_DIR=dlls
set AES_SCRIPT_FILE=bam_status_scripts.dll
echo on

REM more parameters for aes_viewer :
REM -min_filters <number> : The minimal number of nodes in the tree (see aes_viewer.cpp for the tree's structure)
REM -tree_factor <number> : The child/parent ratio in that tree
REM -test_name "name" name of the test to run (look in aes_viewer.cpp)
REM -test_duration <number> seconds to run before terminating

%TOPAZ_HOME%\bin\aes_server.exe -drv_running_mode server -extra_ext aes -extra_ext sample_publisher -dc_site_url http://localhost/topaz -drv_log_file %TOPAZ_HOME%\log\aes.log -out %TOPAZ_HOME%\bin\aes_scripts\compile_area -c_script_file %TOPAZ_HOME%\bin\aes_scripts\%AES_SCRIPT_DIR%\%AES_SCRIPT_FILE%

