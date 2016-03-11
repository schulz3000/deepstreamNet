@events
Feature: Events Misc
	This feature covers other possible scenarios that can occur, such as 
	attempting to unsubscribe to an event that you haven't subscribed to.

Scenario: The client is connected
	Given the test server is ready
		And the client is initialised
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

Scenario: The client attempts to subscribe to the same event multiple times
		  only triggers a single subscribe message to the server and the incoming events should
		  be multiplexed on the client

	Given the server resets its message count
	When the client subscribes to an event named "test2"
		And the server sends the message E|A|S|test2+		
		And the client subscribes to an event named "test2"
	Then the server received the message E|S|test2+
		And the server has received 1 messages

#TODO: This does not clear the timeout, which means the following test throws an error
#Scenario: The client tries to unsubscribe from an event it wasn't previously subscribed to
#	When the client unsubscribes from an event named "test3"
#		And the server sends the message E|E|NOT_SUBSCRIBED|test3+
#	Then the client throws a "NOT_SUBSCRIBED" error with message "test3"

#TODO Eventhandler needs to pass the client to convert typed
#Scenario: The client receives a message with typed data, but an unknown type
	#When the server sends the message E|EVT|someEvent|QXX+
	#Then the client throws a MESSAGE_PARSE_ERROR error with message Unknown type XXX