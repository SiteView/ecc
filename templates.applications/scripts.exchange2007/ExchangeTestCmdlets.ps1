#
# Synopsis: ExchangeTestCmdlets.ps1 will run a test cmdlets collection according to the server roles installed on the Exchage Server and selected counters.
#           The cmdlets are:
#	    1. Test-MapiConnectivity is used for verifying server functionality by logging on to the mailbox that specified by the user.
#       2. Test-ExchangeSearch is for testing that search is currently enabled and is indexing new e-mail messages in a timely manner.
#       3. Test-MailFlow is used for diagnosing whether mail can be successfully sent from and delivered to the System
#              Mailbox mailbox on a computer that has the Mailbox server role installed.
#       4. Test-OwaConnectivity is used for verifying that Microsoft Office Outlook Web Access is running as expected.
#       5. Test-WebServicesConnectivity is used for verifying the functionality of Outlook Anywhere on a Microsoft Exchange Server 2007 computer.
#
# a) By default the script gets 8 arguments - running all the cmdlets:
#       Usage: .\ExchangeTestCmdlets.ps1 exchangeServer user domain.com $true $true $true $true $true
#
# b) Here the scripts gets 13 arguments.
# The five additional boolean arguments represent whether to run the cmdlet or not: "$true" means "run", "$false" means "don't run".
#       Usage: .\ExchangeTestCmdlets.ps1 exchangeServer user domain.com $true $true $true $true $true $true $true $false $true $true
#

$argsLength = $args.length
$usage = "Missing arguments. " + [system.environment]::newline + "Usage:" + [system.environment]::newline + "1.   &'ExchangeTestCmdlets.ps1' <server> <account> <domain> <isHubTransportServer> <isClientAccessServer> <isEdgeServer> <isMailboxServer> <isUnifiedMessagingServer>" + [system.environment]::newline + "For Example: &'ExchangeTestCmdlets.ps1' exchangeServer user domain.com $true $true $true $true $true" + [system.environment]::newline + "2.  &'ExchangeTestCmdlets.ps1' <server> <account> <domain> <isHubTransportServer> <isClientAccessServer> <isEdgeServer> <isMailboxServer> <isUnifiedMessagingServer> <isTestMapiConnectivity> <isTestMailFlow> <isTestExchangeSearch> <isTestOwaConnectivity> <isTestWebServicesConnectivity>" + [system.environment]::newline + "For Example: &'ExchangeTestCmdlets.ps1' exchangeServer user domain.com $true $true $true $true $true $true $true $true $true $true"

if (!($argsLength -eq 13) -and !($argsLength -eq 8)){
	$usage
	break
}


$exchangeServer = $args[0]
$account = $args[1]
$domain = $args[2]
$isHubTransportServer = $args[3]
$isClientAccessServer = $args[4]
$isEdgeServer = $args[5]
$isMailboxServer = $args[6]
$isUnifiedMessagingServer = $args[7]

if ($argsLength -eq 8){
    $isTestMapiConnectivity = $true
    $isTestMailFlow = $true
    $isTestExchangeSearch = $true
    $isTestOwaConnectivity = $true
    $isTestWebServicesConnectivity = $true
} else {
    $isTestMapiConnectivity = $args[8]
    $isTestMailFlow = $args[9]
    $isTestExchangeSearch = $args[10]
    $isTestOwaConnectivity = $args[11]
    $isTestWebServicesConnectivity = $args[12]
}


if ($isMailboxServer){

    if ($isTestMapiConnectivity){
	    "--- TEST-MAPICONNECTIVITY_OUTPUT ---"
	    Test-MapiConnectivity -server $exchangeServer | Format-List
	}

    if ($isTestExchangeSearch){
	    "--- TEST-EXCHANGESEARCH_OUTPUT ---"
	    Test-ExchangeSearch $account@$domain | Format-List
	}

}


if ($isMailboxServer -and $isHubTransportServer){

    if ($isTestMailFlow){
	    "--- TEST-MAILFLOW_OUTPUT ---"
	    Test-MailFlow $exchangeServer | Format-List
	}

}
	
if ($isClientAccessServer){

    if ($isTestOwaConnectivity){
	    "--- TEST-OWACONNECTIVITY_OUTPUT ---"
	    Test-OwaConnectivity -ClientAccessServer:$exchangeServer -AllowUnsecureAccess | Format-List
	}
	
	if($isTestWebServicesConnectivity){
	    "--- TEST-WEBSERVICESCONNECTIVITY_OUTPUT ---"
	    Test-WebServicesConnectivity -ClientAccessServer $exchangeServer -TrustAnySSLCertificate | format-list
	}

}
