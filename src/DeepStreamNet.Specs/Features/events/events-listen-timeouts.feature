@events
Feature: Event Listen Timeouts
	Whenever a listen subscribe or unsubscribe event does not recieve 
	an acknolowdgement from the server the client should emit an ack 
	timeout error so that the client can attempt to retry.

Scenario: The client is connected
	Given the test server is ready
		And the client is initialised
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

Scenario: The client listens to eventPrefix
	When the client listens to events matching "eventPrefix/.*"
	Then the last message the server recieved is E|L|eventPrefix/.*+

@timeout
Scenario: The server does not respond in time with an ACK
	When some time passes
	Then the client throws a "ACK_TIMEOUT" error with message "No ACK message received in time for eventPrefix/.*"

Scenario: The client unlistens to eventPrefix
	When the client unlistens to events matching "eventPrefix/.*"
	Then the last message the server recieved is E|UL|eventPrefix/.*+

@timeout
Scenario: The server does not respond in time with an ACK
	When some time passes
	Then the client throws a "ACK_TIMEOUT" error with message "No ACK message received in time for eventPrefix/.*"