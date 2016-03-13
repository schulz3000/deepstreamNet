using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    class DeepStreamEvents : DeepStreamBase, IDeepStreamEvents
    {
        readonly Dictionary<string, int> eventsDict = new Dictionary<string, int>();
        readonly Dictionary<string, int> listenerDict = new Dictionary<string, int>();

        public DeepStreamEvents(Connection con)
            : base(con)
        {
        }

        public Task PublishAsync<T>(string eventName, T data)
        {
            ThrowIfConnectionNotOpened();

            if (string.IsNullOrWhiteSpace(eventName))
                throw new ArgumentNullException(nameof(eventName));

            var sendData = Utils.ConvertAndPrefixData(data);

            var command = Utils.BuildCommand(Topic.EVENT, Action.EVENT, eventName, sendData);
            return Connection.SendAsync(command);
        }

        public async Task<IDisposable> Subscribe(string eventName, Action<object> data)
        {
            ThrowIfConnectionNotOpened();

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

            ThrowIfConnectionNotOpened();

            if (listenerDict.ContainsKey(pattern))
                throw new DeepStreamException("we still listen for " + pattern);

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

            return new DisposableAction(async () =>
            {
                if (await Connection.SendWithAckAsync(Topic.EVENT, Action.UNLISTEN, Action.UNLISTEN, pattern))
                {
                    Connection.EventListenerChanged -= handler;
                    listenerDict.Remove(pattern);
                }
            });
        }

        private async Task Subscribe(string eventName)
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