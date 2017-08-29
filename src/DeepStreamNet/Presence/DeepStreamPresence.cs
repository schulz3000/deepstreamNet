using DeepStreamNet.Contracts;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    class DeepStreamPresence : DeepStreamBase, IDeepStreamPresence
    {
        readonly Dictionary<string, Func<string, bool, Task>> listeners = new Dictionary<string, Func<string, bool, Task>>();

        public DeepStreamPresence(Connection connection, DeepStreamOptions options)
            : base(connection, options)
        {
            Connection.PresenceListenerChanged += Connection_PresenceListenerChanged;
        }

        void Connection_PresenceListenerChanged(object sender, PresenceListenerChangedEventArgs e)
        {
            foreach (var item in listeners.Values)
            {
                item(e.Username, e.IsLoggedIn);
            }
        }

        public Task<IEnumerable<string>> GetAllAsync()
        {
            var tcs = new TaskCompletionSource<IEnumerable<string>>();

            EventHandler<PresenceGetAllReceivedArgs> handler = null;
            handler = (s, e) =>
            {
                Connection.PresenceGetAllReceived -= handler;
                tcs.TrySetResult(e.Usernames);
            };

            Connection.PresenceGetAllReceived += handler;

            Connection.Send(Utils.BuildCommand(Topic.PRESENCE, Action.QUERY, Action.QUERY));

            return tcs.Task;
        }

        public Task<IAsyncDisposable> SubscribeAsync(Action<string, bool> listener)
        {
            return InnerSubscribeAsync((username, isLoggedIn) =>
            {
                listener(username, isLoggedIn);
                return Task.FromResult(0);
            });
        }

        public Task<IAsyncDisposable> SubscribeAsync(Func<string, bool, Task> listener)
        {
            return InnerSubscribeAsync(listener);
        }

        async Task<IAsyncDisposable> InnerSubscribeAsync(Func<string, bool, Task> listener)
        {
            if (listener == null)
                throw new ArgumentNullException(nameof(listener));

            var key = Utils.CreateUid();

            if (await Connection.SendWithAckAsync(Topic.PRESENCE, Action.SUBSCRIBE, Action.ACK, Action.SUBSCRIBE.ToString(), Options.SubscriptionTimeout).ConfigureAwait(false))
            {
                listeners.Add(key, listener);
            }

            return new AsyncDisposableAction(async () =>
            {
                if (await Connection.SendWithAckAsync(Topic.PRESENCE, Action.UNSUBSCRIBE, Action.ACK, Action.UNSUBSCRIBE.ToString(), Options.SubscriptionTimeout).ConfigureAwait(false))
                {
                    listeners.Remove(key);
                }
            });
        }
    }
}
