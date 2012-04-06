#
# Synopsis: GetExchangeServer.ps1 will run Get-ExchangeServer cmdlet.
# Get-ExchangeServer cmdlet is used for obtaining the attributes of a specified server.
#
# a) by default the script gets as an argument the exchange server
#       Usage: .\GetExchangeServer.ps1 exchangeServer
#

$argsLength = $args.length
$usage = "You must supply a server name." + [system.environment]::newline + "For Example: &'GetExchangeServer.ps1' exchangeServer"

if (!($argsLength -eq 1)){
	$usage
	break;
	}

$exchangeServer = $args[0]


"--- GET-EXCHANGESERVER_OUTPUT ---"
Get-ExchangeServer $exchangeServer | Format-List