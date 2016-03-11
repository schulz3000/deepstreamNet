@records
Feature: Record Timeouts
	Records have multiple different actions they can be made, and each
	of those actions have their own timeouts.
	
	These timeouts allow you to notify the user if an error has 
	occured due to connection issues and gives you granular events to 
	allow different reactions to different situations.

	The timeouts are:

	ACK_TIMEOUT 
		If the user does not recieve a timeout when initially 
		requesting a record or discarding it.

	RESPONSE_TIMEOUT
		If the user does not recieve the data in a timely fashion.

	DELETE_TIMEOUT
		This is when the backend does not respond when a record is deleted.
	
	CACHE_RETRIEVAL_TIMEOUT 
	STORAGE_RETRIEVAL_TIMEOUT
		This is a specific backend issue and does not need to be 
		exposed to the user, but helps with logging issues.

Scenario: The client is connected
	Given the test server is ready
		And the client is initialised
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

@timeout
Scenario: The server does not respond in time with an ACK
	When the client creates a record named "unhappyRecord"
		And some time passes
	Then the client throws a "ACK_TIMEOUT" error with message "unhappyRecord"

@timeout
Scenario: The server does not recieve initial record data in time
 	When the server sends the message R|A|S|unhappyRecord+
 		And some time passes
 	# TODO: Should this use another error code?
 	Then the client throws a "RESPONSE_TIMEOUT" error with message "unhappyRecord"

Scenario: The server then recieves the initial record data
	When the server sends the message R|R|unhappyRecord|100|{"reasons":["Because."]}+

Scenario: The client sends an partial update
	When the client sets the record "unhappyRecord" to {"reasons":["Just Because."]}
	Then the last message the server recieved is R|U|unhappyRecord|101|{"reasons":["Just Because."]}+

@timeout
Scenario: The server send a cache retrieval timeout
 	When the server sends the message R|E|CACHE_RETRIEVAL_TIMEOUT|unhappyRecord+
 	Then the client throws a "CACHE_RETRIEVAL_TIMEOUT" error with message "unhappyRecord"
 
 @timeout
Scenario: The server send a storage retrieval timeout
 	When the server sends the message R|E|STORAGE_RETRIEVAL_TIMEOUT|unhappyRecord+
 	Then the client throws a "STORAGE_RETRIEVAL_TIMEOUT" error with message "unhappyRecord"

Scenario: The client discards record
	When the client discards the record named "unhappyRecord"
	Then the last message the server recieved is R|US|unhappyRecord+

@timeout
Scenario: The server does not respond in time with an ACK
	When some time passes
	Then the client throws a "ACK_TIMEOUT" error with message "unhappyRecord"

Scenario: The client deletes the record
	Given the client creates a record named "unhappyRecord"
		And the server sends the message R|A|S|unhappyRecord+
		And the server sends the message R|R|unhappyRecord|100|{"reasons":["Because."]}+
	When the client deletes the record named "unhappyRecord"
	Then the last message the server recieved is R|D|unhappyRecord+

@timeout
Scenario: The server does not recieve an ack
	When some time passes
	Then the client throws a "DELETE_TIMEOUT" error with message "unhappyRecord"