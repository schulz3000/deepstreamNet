using DeepStreamNet.Contracts;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    class DeepStreamEvents : DeepStreamBase, IDeepStreamEvents
    {
        readonly Dictionary<string, int> eventsDict = new Dictionary<string, int>();
        readonly Dictionary<string, Delegate> listenerDict = new Dictionary<string, Delegate>();

        public DeepStreamEvents(Connection connection, DeepStreamOptions options)
            : base(connection, options)
        {
            Connection.EventListenerChanged += Connection_EventListenerChanged;
        }

        async void Connection_EventListenerChanged(object sender, EventListenerChangedEventArgs e)
        {
            if (!listenerDict.ContainsKey(e.Pattern))
                return;

            var listener = listenerDict[e.Pattern];

            var result = listener.DynamicInvoke(e.Name, e.ListenerState == ListenerState.Add, new EventListenerResponse(e.Pattern, e.Name, Connection));
            if (result != null)
                await (Task)result;
        }

        public void Publish<T>(string eventName, T data)
        {
            ThrowIfConnectionNotOpened();

            if (string.IsNullOrWhiteSpace(eventName))
                throw new ArgumentNullException(nameof(eventName));

            var sendData = Utils.ConvertAndPrefixData(data);

            var command = Utils.BuildCommand(Topic.EVENT, Action.EVENT, eventName, sendData);
            Connection.Send(command);
        }

        public async Task<IAsyncDisposable> SubscribeAsync(string eventName, Action<object> data)
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
                await SubscribeAsync(eventName).ConfigureAwait(false);
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
                    await UnSubscribeAsync(eventName).ConfigureAwait(false);
                }
            });
        }

        public Task<IAsyncDisposable> ListenAsync(string pattern, Action<string, bool, IListenerResponse> listener)
        {
            return InnerListenAsync(pattern, listener);
        }

        public Task<IAsyncDisposable> ListenAsync(string pattern, Func<string, bool, IListenerResponse, Task> listener)
        {
            return InnerListenAsync(pattern, listener);
        }

        async Task<IAsyncDisposable> InnerListenAsync(string pattern, Delegate listener)
        {
            if (string.IsNullOrWhiteSpace(pattern))
                throw new ArgumentNullException(nameof(pattern));

            ThrowIfConnectionNotOpened();

            if (listenerDict.ContainsKey(pattern))
                throw new DeepStreamException("we already listen for " + pattern);

            if (!listenerDict.ContainsKey(pattern) && await Connection.SendWithAckAsync(Topic.EVENT, Action.LISTEN, Action.LISTEN, pattern, Options.SubscriptionTimeout).ConfigureAwait(false))
            {
                listenerDict.Add(pattern, listener);
            }

            return new AsyncDisposableAction(async () =>
            {
                if (await Connection.SendWithAckAsync(Topic.EVENT, Action.UNLISTEN, Action.UNLISTEN, pattern, Options.SubscriptionTimeout).ConfigureAwait(false))
                {
                    listenerDict.Remove(pattern);
                }
            });
        }

        async Task SubscribeAsync(string eventName)
        {
            if (string.IsNullOrWhiteSpace(eventName))
                throw new ArgumentNullException(nameof(eventName));

            var result = await Connection.SendWithAckAsync(Topic.EVENT, Action.SUBSCRIBE, Action.SUBSCRIBE, eventName, Options.SubscriptionTimeout).ConfigureAwait(false);

            if (!result)
                throw new DeepStreamException(Constants.Errors.ACK_TIMEOUT);
        }

        async Task UnSubscribeAsync(string eventName)
        {
            if (string.IsNullOrWhiteSpace(eventName))
                throw new ArgumentNullException(nameof(eventName));

            var result = await Connection.SendWithAckAsync(Topic.EVENT, Action.UNSUBSCRIBE, Action.UNSUBSCRIBE, eventName, Options.SubscriptionTimeout).ConfigureAwait(false);

            if (!result)
                throw new DeepStreamException(Constants.Errors.ACK_TIMEOUT);
        }
    }
}