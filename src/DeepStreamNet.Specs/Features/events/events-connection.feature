@events
Feature: Events Connectivity
	Events subscriptions must be resent to the server after connection 
	issues to guarantee it continues recieving them correctly. This 
	applies to both subscribing to events and listening to event 
	subscriptions.

Scenario: The client is connected
	Given the test server is ready
		And the client is initialised
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

Scenario: The client subscribes to an event
	Given the client subscribes to an event named "test1"
	Then the last message the server recieved is E|S|test1+

Scenario: The server sends an ACK message for test1
	Given the server sends the message E|A|S|test1+

Scenario: The client listens to eventPrefix
	When the client listens to events matching "eventPrefix/.*"
	Then the last message the server recieved is E|L|eventPrefix/.*+

Scenario: The server sends an ACK message for test1
	Given the server sends the message E|A|L|eventPrefix/.*+

Scenario: The client loses its connection to the server
	When the connection to the server is lost
	Given two seconds later
	Then the client throws a "connectionError" error with message "Can't connect! Deepstream server unreachable on localhost:7777"
		And the clients connection state is "RECONNECTING"

Scenario: The client publishes an event
	When the client publishes an event named "test1" with data "yetAnotherValue"
	Then the server did not recieve any messages
	
Scenario: The client reconnects to the server
	When the connection to the server is reestablished
	Then the clients connection state is "AUTHENTICATING"

Scenario: The client is connected
	Given the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+
	Then the clients connection state is "OPEN"

Scenario: The client resends the event subscription
	Then the server received the message E|S|test1+

Scenario: The client resends the event listen
	Then the server received the message E|L|eventPrefix/.*+

Scenario: The client sends offline events
	Then the server received the message E|EVT|test1|SyetAnotherValue+
