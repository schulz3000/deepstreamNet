@rpc
Feature: RPC Timeouts
	Remote Procedure Calls are deepstream's concept of request-response 
	communication. This requires a client that makes the RPC 
	(requestor or receiver) and another client that answers it (provider).

	Whenever a provide or unprovide event does not recieve an 
	acknolowdgement from the server the client should emit an ack timeout 
	error so that the client can attempt to retry.

Scenario: The client is connected
	Given the test server is ready
		And the client is initialised
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

# Making

Scenario: The client makes an RPC
	When the client requests RPC "toUppercase" with data "abc"
	Then the last message the server recieved is P|REQ|toUppercase|<UID>|Sabc+

Scenario: The client receives a timeout
	When some time passes
	Then the client recieves an error RPC callback for "toUppercase" with the message "RESPONSE_TIMEOUT"

# Providing

Scenario: The client provides a RPC
	When the client provides a RPC called "toUppercase"
	Then the last message the server recieved is P|S|toUppercase+

@timeout
Scenario: The server does not respond in time with an ACK
	When some time passes
	Then the client throws a "ACK_TIMEOUT" error with message "No ACK message received in time for toUppercase"

# Unproviding

Scenario: The client stops providing a RPC
	When the client stops providing a RPC called "toUppercase"
	Then the last message the server recieved is P|US|toUppercase+

@timeout
Scenario: The server does not respond in time with an ACK
	When some time passes
	Then the client throws a "ACK_TIMEOUT" error with message "No ACK message received in time for toUppercase"
	