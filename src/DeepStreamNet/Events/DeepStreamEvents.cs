using DeepStreamNet.Contracts;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    class DeepStreamEvents : DeepStreamBase, IDeepStreamEvents
    {
        readonly Dictionary<string, int> eventsDict = new Dictionary<string, int>();
        readonly Dictionary<string, int> listenerDict = new Dictionary<string, int>();

        public DeepStreamEvents(Connection connection, DeepStreamOptions options)
            : base(connection, options)
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

        public async Task<IAsyncDisposable> Subscribe(string eventName, Action<object> data)
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
                await Subscribe(eventName).ConfigureAwait(false);
                eventsDict.Add(eventName, 0);
            }
            else
            {
                eventsDict[eventName]++;
            }

            return new AsyncDisposableAction(async () =>
            {
                eventsDict[eventName]--;
                Connection.EventReceived -= handler;
                if (eventsDict[eventName] == 0)
                {
                    await UnSubscribe(eventName).ConfigureAwait(false);
                }
            });
        }

        public async Task<IAsyncDisposable> Listen(string pattern)
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
                if (await Connection.SendWithAckAsync(Topic.EVENT, Action.LISTEN, Action.LISTEN, pattern, Options.SubscriptionTimeout))
                {
                    listenerDict.Add(pattern, 0);
                }
            }

            Connection.EventListenerChanged += handler;

            return new AsyncDisposableAction(async () =>
            {
                if (await Connection.SendWithAckAsync(Topic.EVENT, Action.UNLISTEN, Action.UNLISTEN, pattern, Options.SubscriptionTimeout).ConfigureAwait(false))
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

            var result = await Connection.SendWithAckAsync(Topic.EVENT, Action.SUBSCRIBE, Action.SUBSCRIBE, eventName, Options.SubscriptionTimeout).ConfigureAwait(false);

            if (!result)
                throw new DeepStreamException(Constants.Errors.ACK_TIMEOUT);
        }

        async Task UnSubscribe(string eventName)
        {
            if (string.IsNullOrWhiteSpace(eventName))
                throw new ArgumentNullException(nameof(eventName));

            var result = await Connection.SendWithAckAsync(Topic.EVENT, Action.UNSUBSCRIBE, Action.UNSUBSCRIBE, eventName, Options.SubscriptionTimeout).ConfigureAwait(false);

            if (!result)
                throw new DeepStreamException(Constants.Errors.ACK_TIMEOUT);
        }
    }
}