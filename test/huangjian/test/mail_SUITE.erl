-module(mail_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%%--------------------------------------------------------------------
%% Test server callback functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> DefaultData
%% DefaultData: [tuple()]  
%% Description: Require variables and set default values for the suite
%%--------------------------------------------------------------------
suite() -> [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config: [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config: [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.
%%--------------------------------------------------------------------
%% Function: all() -> TestCases
%% TestCases: [Case] 
%% Case: atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------      
all() -> 
    [{sequence, send}, {sequence, receive_pop3}, {sequence, receive_imap4}, {sequence, rec_send}].

%% Function: sequences() -> Sequences 
%%Sequences = [{SeqName,Testcases}]
%%SeqName = atom()
%%Testcases = [atom()]
%% Description: Returns a list of sequenced test cases in this test suite
sequences() ->
	[{send, [correct_send_no_attach, correct_send_have_attach,
			error_send_stmp_server, error_send_smtp_mail,
			error_send_smtp_user, error_send_smtp_password,
			error_send_smtp_attach]},
	 {receive_pop3, [correct_pop3_no_match, correct_pop3_have_match,
			error_pop3_pop3_server, error_pop3_pop3_user, 
			error_pop3_pop3_password, error_pop3_pop3_match]},
	 {receive_imap4, [correct_imap4_no_match, correct_imap4_have_match,
			error_imap4_imap4_server, error_imap4_imap4_user, 
			error_imap4_imap4_password, error_imap4_imap4_match]},
	 {rec_send, [correct_rec_send, error_send_correct_rec,
			error_rec_correct_send]}].
	
correct_send_no_attach(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "send"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "smtp"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_send_have_attach(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "send"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "smtp"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, "f:\\hj.ini"),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
error_send_stmp_server(_Config) -> 
	P = mail_monitor:new(),
	P:set_property(action, "send"),
	P:set_property(smtpServer, "mail.dragoflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "smtp"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_send_smtp_mail(_Config) ->
	P = mail_monitor:new(),
	P:set_property(action, "send"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huan@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "smtp"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_send_smtp_user(_Config) ->
	P = mail_monitor:new(),
	P:set_property(action, "send"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "smtp"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huan"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_send_smtp_password(_Config) ->
	P = mail_monitor:new(),
	P:set_property(action, "send"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "smtp"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcomin"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_send_smtp_attach(_Config) ->
	P = mail_monitor:new(),
	P:set_property(action, "send"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "smtp"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, "c:\\1.txt"),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

correct_pop3_no_match(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "pop3"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_pop3_have_match(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "pop3"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, "HJ"),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
error_pop3_pop3_server(_Config) ->	 
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "pop3"),
	P:set_property(pop3Server, "mail.dragonflo.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.


error_pop3_pop3_user(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "pop3"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huan"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_pop3_pop3_password(_Config) ->
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "pop3"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcomin"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_pop3_pop3_match(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "pop3"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, "LH"),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

correct_imap4_no_match(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "imap4"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
correct_imap4_have_match(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "imap4"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, "HJ"),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
		
error_imap4_imap4_server(_Config) ->	 
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "imap4"),
	P:set_property(pop3Server, "mail.dragonflo.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.


error_imap4_imap4_user(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "imap4"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huan"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_imap4_imap4_password(_Config) ->
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "imap4"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcomin"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_imap4_imap4_match(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "receive"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "imap4"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, "LH"),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
			
correct_rec_send(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "sendrecv"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "imap4"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.

error_send_correct_rec(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "sendrecv"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "imap4"),
	P:set_property(pop3Server, "mail.dragonflow.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huan"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
error_rec_correct_send(_Config) ->	
	P = mail_monitor:new(),
	P:set_property(action, "sendrecv"),
	P:set_property(smtpServer, "mail.dragonflow.com"),
	P:set_property(sendFromAddr, "jian.huang@dragonflow.com"),
	P:set_property(sendToAddr, "lei.lin@dragonflow.com"),
	P:set_property(receivingProtocol, "imap4"),
	P:set_property(pop3Server, "mail.dragonflo.com"),
	P:set_property(pop3Account, "jian.huang"),
	P:set_property(pop3Password, "iamcoming"),
	P:set_property(contentMatch, ""),
	P:set_property(timeout, 30),
	P:set_property(checkEvery, ""),
	P:set_property(attachment, ""),
	P:set_property(smtpUser, "jian.huang"),
	P:set_property(smtpPassword, "iamcoming"),
	
	P:update(),
	
	{ok, {state_string, Good}} = P:get_attribute(state_string),
	
	{comment, Good}.
	
			