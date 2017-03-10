DeepStreamNet
=============

dotnet Client for [deepstream.io](https://deepstream.io)

##Builds
[![Build status](https://ci.appveyor.com/api/projects/status/aj8op4emvlivn7jx/branch/develop?svg=true)](https://ci.appveyor.com/project/schulz3000/deepstreamnet/branch/develop) [![Build Status](https://travis-ci.org/schulz3000/deepstreamNet.svg?branch=develop)](https://travis-ci.org/schulz3000/deepstreamNet)

##NuGet
[![deepstreamNet](https://img.shields.io/nuget/v/deepstreamNet.svg?style=flat)](https://www.nuget.org/packages/deepstreamNet)

##Frameworks
DotNet >= 4.0
NetStandard >= 1.5

##Usage

```csharp
var client = new DeepStreamClient("localhost", 6020, "deepstream");

if(await client.LoginAsync())
{
 //Login was successful   
}

//Alternative with credentials
await client.LoginAsync("Username", "Password");

// Close Connection to deepstream.io endpoint
client.Dispose();
```

###Events

```csharp
// Subscribe to Event 'test'
var eventSubscription = await client.Events.SubscribeAsync("test", x => { Console.WriteLine(x); });

// Send 'Hello' to all Subscribers of Event 'test'
client.Publish("test", "Hello");

// Send number '42' to all Subscribers of Event 'test'
client.Publish("test", 42);

// Send object '{Property1="Hello", Property2=42}' to all Subscribers of Event 'test'
client.Publish("test", new {Property1="Hello", Property2=42});

// Unsubscribe from Event 'test'
await eventSubscription.DisposeAsync();

// Listen to events
var listener = await client.Events.ListenAsync("^test/.*", (eventName, isSubscribed, response) =>
{
   if (isSubscribed)
   {
      if (/* if you want to provide */)
      {
         response.Accept();
         client.Events.Publish(eventName, "Hello World");
         // start publishing data via client.Events.Publish(eventName, /* data */)
      }
      else
      {
          response.Reject(); // let deepstream ask another provider
      }
    }
    else
    {
        // stop publishing data
    }
});

// Unlisten
await listener.DisposeAsync();
```

###Records

```csharp
var record = await client.Records.GetRecordAsync("test");

record["FirstName"] = "John";
record["Age"] = 28;

// Save all changes
await client.Records.SaveAsync(record);

// Discard all changes
await client.Records.DiscardAsync(record);

// Delete Records
await client.Records.DeleteAsync(record);
```

###Dynamic Records

```csharp
dynamic record = await client.Records.GetRecordAsync("test");

record.FirstName = "Jane";
record.Age = 21;

// Save all changes
await client.Records.SaveAsync(record);
```

###RPC Request

```csharp
//Request RemoteProcedure 'toUpperCase' with argument 'abc'
var result = await client.Rpcs.MakeRequest<string,string>("toUpperCase", "abc");
//result == "ABC"
```

###RPC Provider

```csharp
//Define Method for RemoteProcedure
void ToUpperCase(string input, IRpcResponse<string> response)
{
    if (string.IsNullOrEmpty(input))
        response.Error("input must not be empty");

    if (input == "ABC")
        response.Reject();

    response.Send(input.ToUpper());
}

//Register RemoteProcedure 'toUpperCase' with InputArgs as string and Result as string
var proc = await client.Rpcs.RegisterProviderAsync<string, string>("toUpperCase", ToUpperCase);

//alternative with anonymous Method as RemoteProcedure
var proc = await client.Rpcs.RegisterProviderAsync<string, string>("toUpperCase", (input,response)=> response.Send(input.ToUpper()));


//Define async Method for RemoteProcedure
async Task ToUpperCaseAsync(string input, IRpcResponse<string> response)
{
    if (string.IsNullOrEmpty(input))
        response.Error("input must not be empty");

    if (input == "ABC")
        response.Reject();

    await Task.Delay(500);

    response.Send(input.ToUpper());
}

//Register async RemoteProcedure 'toUpperCase' with InputArgs as string and Result as string
var proc = await client.Rpcs.RegisterProviderAsync<string, string>("toUpperCase", ToUpperCaseAsync);

//alternative with anonymous Method as RemoteProcedure
var proc = await client.Rpcs.RegisterProviderAsync<string, string>("toUpperCase", async (input,response)=> {
    await Task.Delay(500);
    response.Send(input.ToUpper();
    }));

//Unregister RemoteProcedure
await proc.DisposeAsync(); 
```