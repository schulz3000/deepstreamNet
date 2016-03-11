@events
Feature: Events
	Events are deepstream's publish-subscribe pattern. Everytime a 
	client subscribes to or unsubscribes from an event the server 
	replies with an acknowledgment message

Scenario: The client is connected
	Given the test server is ready
		And the client is initialised
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

# Happy Path
Scenario: The client subscribes to an event
	Given the client subscribes to an event named "test1"
	Then the server received the message E|S|test1+ 

Scenario: The server sends an ACK message for test1
	Given the server sends the message E|A|S|test1+

Scenario: The client receives an event
	When the server sends the message E|EVT|test1|SsomeValue+
	Then the client received the event "test1" with data "someValue"

Scenario: The client receives another event
	When the server sends the message E|EVT|test1|SanotherValue+
	Then the client received the event "test1" with data "anotherValue"

Scenario: The client publishes an event
	When the client publishes an event named "test1" with data "yetAnotherValue"
	Then the server received the message E|EVT|test1|SyetAnotherValue+

Scenario: The client unsubscribes from an event
	When the client unsubscribes from an event named "test1"
	Then the server received the message E|US|test1+

Scenario: The server sends an ACK message for test1 unsubscribe
	Given the server sends the message E|A|US|test1+