@records
Feature: Record
	Records are arbitrary JSON structures that can be manipulated and 
	subscribed to. Every change to a record is synced accross all 
	subscribed clients. 

	The actions that can be performed on a record are:
	- Request a record to recieve the current state and subcribe
	- Send or Recieve updates to the record
	- Discard the record when no longer needed on the client
	- Delete the record if no longer needed in the system

Scenario: The client is connected
	Given the test server is ready
		And the client is initialised
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

Scenario: The client creates a record
	Given the client creates a record named "happyRecord"
	Then the last message the server recieved is R|CR|happyRecord+

Scenario: The server sends a read ACK message for happyRecord
	Given the server sends the message R|A|S|happyRecord+

Scenario: The client receives the initial record data
	When the server sends the message R|R|happyRecord|100|{"name":"John", "pets": [{"name":"Ruffles", "type":"dog","age":2}]}+
	Then the client record "happyRecord" data is {"name":"John", "pets": [{"name":"Ruffles", "type":"dog","age":2}]}

Scenario: The client receives an partial update
	When the server sends the message R|P|happyRecord|101|pets.0.age|N3+
	Then the client record "happyRecord" data is {"name":"John", "pets": [{"name":"Ruffles", "type":"dog","age":3}]}

Scenario: The client receives a full update
	When the server sends the message R|U|happyRecord|102|{"name":"Smith", "pets": [{"name":"Ruffus", "type":"dog","age":4}]}+
	Then the client record "happyRecord" data is {"name":"Smith", "pets": [{"name":"Ruffus", "type":"dog","age":4}]}

Scenario: The client sends an partial update
	When the client sets the record "happyRecord" "pets.0.name" to "Max"
	Then the last message the server recieved is R|P|happyRecord|103|pets.0.name|SMax+

Scenario: The client receives a full update
	When the client sets the record "happyRecord" to {"name":"Smith","pets":[{"name":"Ruffus","type":"dog","age":5}]}
	Then the last message the server recieved is R|U|happyRecord|104|{"name":"Smith","pets":[{"name":"Ruffus","type":"dog","age":5}]}+

Scenario: The client discards the record
	When the client discards the record named "happyRecord"
	Then the last message the server recieved is R|US|happyRecord+

Scenario: The server responds with a discard ACK
	When the server sends the message R|A|US|happyRecord+

Scenario: The client deletes the record
	Given the client creates a record named "happyRecord"
		And the server sends the message R|A|S|happyRecord+
		And the server sends the message R|R|happyRecord|100|{"name":"John", "pets": [{"name":"Ruffles", "type":"dog","age":2}]}+	
		And the client deletes the record named "happyRecord"
	Then the last message the server recieved is R|D|happyRecord+

Scenario: The server responds with a delete ACK
	When the server sends the message R|A|D|happyRecord+