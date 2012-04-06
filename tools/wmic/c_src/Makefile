.SUFFIXES: .o .c 
CC=gcc


SHARED_LIBADD = -Wl,-rpath,/usr/local/lib -L/usr/local/lib -Wl,-rpath,/usr/local/lib -L/usr/local/lib  


ERL_INCLUDE = -I/usr/lib/erlang/usr/include \
	      -I/usr/lib/erlang/lib/erl_interface-3.6.4/include
	      
ERL_LIBS    = -L/usr/lib/erlang/usr/lib \
              -lerts \
	      -L/usr/lib/erlang/lib/erl_interface-3.6.4/lib \
              -lei \
              -lerl_interface	     
	      
WMIC_INCLUDE = -g -I../deps/wmi-1.3.13/Samba/source \
                  -I../deps/wmi-1.3.13/Samba/source/include \
		  -I../deps/wmi-1.3.13/Samba/source/lib \
		  -I../deps/wmi-1.3.13/Samba/source/lib/replace \
		  -D_SAMBA_BUILD_=4 -DHAVE_CONFIG_H  \
		  -I/usr/include/python2.4

WMIC_LIBS     =  -Wl,--export-dynamic \
		-L../deps/wmi-1.3.13/Samba/source/bin/static -lpopt_samba \
		-lwmi \
		-lrpc_ndr_winreg \
		-lrpc_ndr_lsa \
		-lrpc_ndr_samr \
		-ldcom \
		-lcom \
		-ldcom_proxy_dcom \
		-lndr_table \
		-lndr_nfs4acl \
		-lndr_frsapi \
		-lndr_frsrpc \
		-lndr_notify \
		-lndr_sasl_helpers \
		-lndr_opendb \
		-lndr_winstation \
		-lndr_dnsserver \
		-lndr_winsrepl \
		-lndr_drsblobs \
		-lndr_rot \
		-lndr_xattr \
		-lndr_keysvc \
		-lndr_trkwks \
		-lndr_ntsvcs \
		-lndr_scerpc \
		-lndr_w32time \
		-lndr_browser \
		-lndr_wzcsvc \
		-lndr_protected_storage \
		-lndr_mgmt \
		-lndr_winreg \
		-lndr_initshutdown \
		-lndr_wins \
		-lndr_msgsvc \
		-lndr_dssetup \
		-lndr_dbgidl \
		-lndr_eventlog \
		-lndr_atsvc \
		-lndr_wkssvc \
		-lndr_srvsvc \
		-lndr_spoolss \
		-lndr_spoolss_buf \
		-lndr_unixinfo \
		-lndr_policyagent \
		-lndr_drsuapi \
		-lndr_compression \
		-lcompression \
		-lndr_dfs \
		-lndr_efs \
		-lndr_dsbackup \
		-lndr_echo \
		-lndr_audiosrv \
		-lrpc_ndr_remact \
		-lndr_remact \
		-lndr_dcom \
		-lrpc_ndr_oxidresolver \
		-lndr_oxidresolver \
		-lndr_orpc \
		-ldcerpc \
		-lrpc_ndr_netlogon \
		-lrpc_ndr_epmapper \
		-lndr_epmapper \
		-lndr_dcerpc \
		-lcli_smb \
		-lcli_finddcs \
		-lcli_smb2 \
		-lcli_dgram \
		-lcli_smb_composite \
		-lcli_raw \
		-lgensec \
		-lndr_schannel \
		-lauth \
		-lpam_errors \
		-lpam \
		-lcrypt \
		-lwinbind-client \
		-lmessaging \
		-lunix_privs \
		-lndr_irpc \
		-lndr_winbind \
		-lsasl2 \
		-lpopt_credentials \
		-lcmdline_credentials \
		-lcredentials \
		-lkerberos \
		-lauth_sam \
		-lndr_krb5pac \
		-lndr_netlogon \
		-lsecrets \
		-lutil_tdb \
		-ldb_wrap \
		-lldbsamba \
		-lsamdb \
		-lldb \
		-lcli_ldap \
		-lgensec_socket \
		-lasn1_util \
		-lndr_samr \
		-lndr_lsa \
		-lcli_resolve \
		-lcli_nbt \
		-lndr_nbt \
		-lndr_security \
		-lsecurity \
		-lndr_svcctl \
		-lndr_nbt_buf \
		-lpacket \
		-ltls \
		-lsamba-socket \
		-lcli_composite \
		-levents \
		-lheimdal_hdb_keys \
		-lndr_misc \
		-lndr \
		-lsamba-config \
		-lsamba-errors \
		-ltdb \
		-lcli_auth \
		-lmsrpc_parse \
		-lschanneldb \
		-lcredentials_krb5 \
		-lheimdal_gssapi \
		-lheimdal_krb5 \
		-lheimdal_glue \
		-lnetif \
		-lsamba-util \
		-lcharset \
		-lsamba-config \
		-lsamba-util \
		-ldynconfig \
		-liconv \
		-L/usr/lib \
		-lsocket_wrapper \
		-lcrypto \
		-ltalloc \
		-lheimdal_asn1 \
		-lheimdal_com_err \
		-lheimdal_des \
		-lheimdal_roken \
		-lresolv \
		-lheimdal_roken_getprogname \
		-lpopt_samba \
		-lpopt \
		-lreplace \
		-ldl

TARGET_LIB = wmic_drv.so

ALL: $(TARGET_LIB)

.c.o:
	$(CC) $(CFLAGS) $(ERL_INCLUDE) $(WMIC_INCLUDE) -c $<


wmic_drv.so: wmic_drv.o wmic.o
	$(CC) -o $@ $^ $(ERL_LIBS) $(WMIC_LIBS) $(SHARED_LIBADD)  -fpic -shared
	cp wmic_drv.so ../priv/lib 
	#mv wmic_drv.so ../../priv/lib 
	rm -rf *.o 

clean:
	rm -rf *.o *.so
