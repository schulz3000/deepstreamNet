@records
Feature: Record Conflicts
	Record conflicts occur most often when the client disconnects, 
	does offline work and then reconnects resulting in a merge issue 
	with other potential edits, but can also occur in rare conditions 
	as part of a race condition. If a conflict does occur, the client 
	should recieve a VERSION_EXISTS error.

	Please note this will change as part of the MERGE epic planned, 
	and users will be notified instead that the write failed and be 
	allowed to effectively merge/rebase it as needed. When 
	implementing please let the core team know so we can collabarate.

Scenario: The client is connected
	Given the test server is ready
		And the client is initialised
		And the client logs in with username "XXX" and password "YYY"
		And the server sends the message A|A+

Scenario: The server requests a record
	Given the client creates a record named "mergeRecord"

Scenario: The server responds with ack and read
	When the server sends the message R|A|S|mergeRecord+
	And the server sends the message R|R|mergeRecord|100|{"key":"value1"}+

 Scenario: The client recieves an out of sync update
 	When the server sends the message R|U|mergeRecord|102|{"key":"value3"}+
 	Then the client throws a "VERSION_EXISTS" error with message "mergeRecord"
 
 Scenario: The client sends an partial update
 	When the client sets the record "mergeRecord" "key" to "value4"
 
 Scenario: The client recieves an error saying version already exists
 	When the server sends the message R|E|VERSION_EXISTS|mergeRecord|102+
 	Then the client throws a "VERSION_EXISTS" error with message "mergeRecord"