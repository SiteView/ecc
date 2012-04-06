@echo off
cd %1
tss_dispatcher -drv_running_mode server -extra_ext tss_dispatcher -out ..\\tss_log -drv_log_file ..\\tss_log\\tss_disp_mdrv.log -ml_use_io_thread true -sitescope_port %2