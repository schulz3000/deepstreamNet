@records
Feature: Record Subscription
	Record subscriptions is done purely on the client side. Whenever 
	an update is recieved for a record, you should find out if anyone 
	is interested and then notify them. When implementing, please keep
	in mind the difference between an update and a patch.

	Updates mean you should fully discard the data that you had, and 
	replace it with the data recieved. 

	Patch means you should apply the diff onto the data you currently 
	have.

	From the clients callback perspective there isn't really a 
	difference when recieving updates or patches, since the 
	new data is compared against the old and only the differences 
	are provided to the callback

Scenario: The client is connected
	Given the test server is ready
		And the client is initialised
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

Scenario: The client creates a record
	When the client creates a record named "subscribeRecord"
	Then the last message the server recieved is R|CR|subscribeRecord+

Scenario: The server responds with an ack and the initial read
	When the server sends the message R|A|S|subscribeRecord+
		And the server sends the message R|R|subscribeRecord|124|{"name":"Smith","pets":[{"name":"Ruffus","type":"dog","age":0}]}+

# Full Subscribe

Scenario: The client subscribes to subscribeRecord
	When the client subscribes to the entire record "subscribeRecord" changes
	Then the client will not be notified of the record change

Scenario: The client record subscribeRecord receives updated data it will notify subscribers
	When the server sends the message R|U|subscribeRecord|125|{"name":"Smith","pets":[{"name":"Ruffus","type":"dog","age":1}]}+
	Then the client will be notified of the record change

Scenario: The client record subscribeRecord receives partial data it will notify subscribers
	When the server sends the message R|P|subscribeRecord|126|pets.0.name|SRuffusTheSecond+
	Then the client will be notified of the partial record change

Scenario: The client will no longer get notified after it unsubscribes
	Given the client unsubscribes to the entire record "subscribeRecord" changes
	When the server sends the message R|U|subscribeRecord|127|{"name":"Smith","pets":[{"name":"Ruffus","type":"dog","age":1}]}+
	Then the client will not be notified of the record change

# Path Susbcribe

Scenario: The client subscribes subscribeRecord to the path pets.0
	When the client subscribes to "pets.0.age" for the record "subscribeRecord"
	Then the client will not be notified of the record change

Scenario: The client receives an partial update unrelated to the path subscribed to
	When the server sends the message R|P|subscribeRecord|128|name|SJohn Smith+
	Then the client will not be notified of the record change

Scenario: The client receives an full update where the pets age hasn't changed
	When the server sends the message R|U|subscribeRecord|129|{"name":"John Smith", "age": 21, "pets": [{"name":"Ruffus", "type":"dog","age":1}]}+
	Then the client will not be notified of the record change

Scenario: The client receives an partial update related to the path subscribed to
	When the server sends the message R|P|subscribeRecord|130|pets.0.age|N4+
	Then the client will be notified of the record change

Scenario: The client receives an full update where the pets has changed
	When the server sends the message R|U|subscribeRecord|131|{"name":"John Smith", "age": 21, "pets": [{"name":"Ruffus", "type":"dog","age":5}]}+
	Then the client will be notified of the second record change

Scenario: The client will no longer get notified after it unsubscribes to the path
	Given the client unsubscribes to "pets.0.age" for the record "subscribeRecord"
	When the server sends the message R|P|subscribeRecord|132|pets.0.age|N6+
	Then the client will not be notified of the record change