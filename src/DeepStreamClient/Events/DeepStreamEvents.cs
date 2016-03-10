using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;
using Jil;

namespace DeepStreamNet
{
    class DeepStreamEvents : DeepStreamBase,IDeepStreamEvents
    {
        readonly Dictionary<string, int> eventsDict = new Dictionary<string, int>();
        readonly Dictionary<string, int> listenerDict = new Dictionary<string, int>();

        public DeepStreamEvents(Connection con)
            : base(con)
        {
        }

        public Task PublishAsync<T>(string eventName, T data)
        {
            ThrowIfNotLoggedIn();

            if (string.IsNullOrWhiteSpace(eventName))
                throw new ArgumentNullException(nameof(eventName));

            var sendData = string.Empty;

            if (data is string)
            {
                sendData = Constants.Types.STRING + data.ToString();
            }
            else if (data is bool)
            {
                if (bool.Parse(data.ToString()))
                    sendData = Constants.Types.TRUE.ToString();
                else
                    sendData = Constants.Types.FALSE.ToString();
            }
            else if (data is int || data is double || data is float || data is decimal)
            {
                sendData = Constants.Types.NUMBER + data.ToString().Replace(',', '.');
            }
            else if ((data as object) == null)
            {
                sendData = Constants.Types.NULL.ToString();
            }
            else
            {
                try
                {
                    sendData = Constants.Types.OBJECT + JSON.Serialize(data, Options.ISO8601);
                }
                catch
                {
                    sendData = Constants.Types.UNDEFINED.ToString();
                }
            }

            var command = Utils.BuildCommand(Topic.EVENT,Action.EVENT, eventName, sendData);
            return Connection.SendAsync(command);
        }

        public async Task<IDisposable> Subscribe(string eventName, Action<object> data)
        {
            ThrowIfNotLoggedIn();

            EventHandler<EventReceivedArgs> handler = (s, e) =>
            {
                if (string.Equals(e.EventName, eventName, StringComparison.Ordinal))
                    data(e.Data);
            };

            Connection.EventReceived += handler;

            if (!eventsDict.ContainsKey(eventName))
            {
                await Subscribe(eventName);
                eventsDict.Add(eventName, 0);
            }
            else
            {
                eventsDict[eventName]++;
            }

            return new DisposableAction(async () =>
            {
                eventsDict[eventName]--;
                Connection.EventReceived -= handler;
                if (eventsDict[eventName] == 0)
                {
                    await UnSubscribe(eventName);
                }
            });
        }

        public async Task<IDisposable> Listen(string pattern)
        {
            if (string.IsNullOrWhiteSpace(pattern))
                throw new ArgumentNullException(nameof(pattern));

            ThrowIfNotLoggedIn();

            EventHandler<EventListenerChangedArgs> handler = (s, e) =>
            {
                if (string.Equals(e.Pattern, pattern, StringComparison.Ordinal) && listenerDict.ContainsKey(pattern))
                    listenerDict[pattern] += (int)e.EventListenerState;
            };

            if (!listenerDict.ContainsKey(pattern))
            {
                if (await Connection.SendWithAckAsync(Topic.EVENT, Action.LISTEN, Action.LISTEN, pattern))
                {
                    listenerDict.Add(pattern, 0);
                }
            }

            Connection.EventListenerChanged += handler;

            return new DisposableAction(async()=> {
                
                if (await  Connection.SendWithAckAsync(Topic.EVENT,Action.UNLISTEN,Action.UNLISTEN,pattern))
                {
                    Connection.EventListenerChanged -= handler;
                    listenerDict.Remove(pattern);
                }                
            });

        }
                
        async Task Subscribe(string eventName)
        {
            if (string.IsNullOrWhiteSpace(eventName))
                throw new ArgumentNullException(nameof(eventName));

            var result = await Connection.SendWithAckAsync(Topic.EVENT, Action.SUBSCRIBE, Action.SUBSCRIBE, eventName).ConfigureAwait(false);

            if (!result)
                throw new InvalidOperationException("ACK_TIMEOUT");
        }

        async Task UnSubscribe(string eventName)
        {
            if (string.IsNullOrWhiteSpace(eventName))
                throw new ArgumentNullException(nameof(eventName));

            var result = await Connection.SendWithAckAsync(Topic.EVENT, Action.UNSUBSCRIBE, Action.UNSUBSCRIBE, eventName);

            if (!result)
                throw new InvalidOperationException("ACK_TIMEOUT");
        }
    }
}
