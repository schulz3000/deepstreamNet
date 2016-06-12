using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    class Connection : IDisposable
    {
        readonly TcpClient client;
        readonly CancellationToken cts;

        readonly string Host;
        readonly int Port;

        public ConnectionState State { get; internal set; }

        internal event EventHandler<AcknoledgedArgs> Acknoledged;

        internal event EventHandler<ErrorArgs> Error;

        internal event EventHandler<EventReceivedArgs> EventReceived;

        internal event EventHandler<EventListenerChangedArgs> EventListenerChanged;

        internal event EventHandler<RecordReceivedArgs> RecordReceived;

        internal event EventHandler<RecordUpdatedArgs> RecordUpdated;

        internal event EventHandler<RecordPatchedArgs> RecordPatched;

        internal event EventHandler<RemoteProcedureMessageArgs> PerformRemoteProcedureRequested;

        internal event EventHandler<RemoteProcedureMessageArgs> RemoteProcedureResultReceived;

        public Connection(string host, int port, CancellationToken token)
        {
            if (string.IsNullOrWhiteSpace(host))
                throw new ArgumentNullException(nameof(port));

            if (string.IsNullOrWhiteSpace(host))
                throw new ArgumentNullException(nameof(host));

            Host = host;
            Port = port;
            cts = token;

            State = ConnectionState.NONE;

            client = new TcpClient();
            client.NoDelay = true;
        }

        public Task OpenAsync()
        {
            return client.ConnectAsync(Host, Port);
        }

        public async Task SendAsync(string command)
        {
            var stream = client.GetStream();

            var bytes = Encoding.ASCII.GetBytes(command);
            await stream.WriteAsync(bytes, 0, bytes.Length, cts).ConfigureAwait(false);
            await stream.FlushAsync().ConfigureAwait(false);
        }

        public async Task<bool> SendWithAckAsync(Topic topic, Action action, Action expectedReceivedAction, string identifier, int ackTimeout)
        {
            var tcs = new TaskCompletionSource<bool>();

            EventHandler<AcknoledgedArgs> ackHandler = null;
            EventHandler<ErrorArgs> errorHandler = null;
            var timer = new AckTimer(ackTimeout);

            ackHandler = (s, e) =>
            {
                if (e.Topic == topic && e.Action == expectedReceivedAction && e.Identifier == identifier)
                    tcs.TrySetResult(true);
                else if (e.Topic == Topic.AUTH)
                    tcs.TrySetResult(true);

                if (tcs.Task.IsCompleted)
                {
                    timer.Dispose();
                    Acknoledged -= ackHandler;
                    Error -= errorHandler;
                }
            };

            errorHandler = (s, e) =>
            {
                if (e.Topic == topic && e.Action == Action.ERROR)
                    tcs.TrySetException(new DeepStreamException(e.Error, e.Message));
            };

            timer.Elapsed += (s, e) =>
            {
                Acknoledged -= ackHandler;
                Error -= errorHandler;
                timer.Dispose();
                tcs.TrySetException(new DeepStreamException(Constants.Errors.ACK_TIMEOUT));
            };

            string command = Utils.BuildCommand(topic, action, identifier);

            Acknoledged += ackHandler;
            Error += errorHandler;

            timer.Start();

            await SendAsync(command).ConfigureAwait(false);

            return await tcs.Task.ConfigureAwait(false);
        }

        public void StartMessageLoop()
        {
            Task.Run(MessageLoopAsync, cts);
        }

        public async Task MessageLoopAsync()
        {
            var stream = client.GetStream();
            int result;

            var buffer = new byte[client.ReceiveBufferSize];

            var sb = string.Empty;

            while (!cts.IsCancellationRequested && client.Connected)
            {
                while ((result = await stream.ReadAsync(buffer, 0, buffer.Length, cts).ConfigureAwait(false)) != 0)
                {
                    var enc = sb + Encoding.UTF8.GetString(buffer, 0, result);

                    var groups = enc.Split(Constants.GroupSeperator);

                    for (int i = 0; i < groups.Length - 1; i++)
                    {
                        Notify(groups[i]);
                    }
                    sb = groups[groups.Length - 1];
                }
            }
        }

        void Notify(string value)
        {
            var split = value.Split(Constants.RecordSeperator);

            if (split.Length < 2)
            {
                OnError(Topic.Empty, Action.Empty, Constants.Errors.MESSAGE_PARSE_ERROR, "Insufficiant message parts");
                return;
            }

            var responseAction = new Action(split[1]);
            var action = new Action(split.Length == 2 ? null : split[2]);
            var topic = new Topic(split[0]);

            if (topic == Topic.AUTH)
            {
                if (responseAction == Action.ACK)
                {
                    OnAcknoledged(topic, action, null);
                }
                else if (responseAction == Action.ERROR)
                {
                    OnError(topic, action, split[2], split[3].Substring(1));
                }
                else
                {
                    OnError(topic, action, Constants.Errors.MESSAGE_PARSE_ERROR, "Unknown action " + action);
                }
            }
            else if (topic == Topic.EVENT)
            {
                if (responseAction == Action.ACK)
                {
                    OnAcknoledged(topic, action, split[3]);
                }
                else if (responseAction == Action.EVENT)
                {
                    var convertedDataWithType = Utils.ConvertPrefixedData(split[3]);
                    EventReceived?.Invoke(this, new EventReceivedArgs(split[2], convertedDataWithType.Key, convertedDataWithType.Value));
                }
                else if (responseAction == Action.SUBSCRIPTION_FOR_PATTERN_FOUND)
                {
                    EventListenerChanged?.Invoke(this, new EventListenerChangedArgs(split[2], EventListenerState.Add));
                }
                else if (responseAction == Action.SUBSCRIPTION_FOR_PATTERN_REMOVED)
                {
                    EventListenerChanged?.Invoke(this, new EventListenerChangedArgs(split[2], EventListenerState.Remove));
                }
                else
                {
                    OnError(topic, action, Constants.Errors.MESSAGE_PARSE_ERROR, "Unknown action " + action);
                }
            }
            else if (topic == Topic.RECORD)
            {
                if (responseAction == Action.ACK)
                {
                    OnAcknoledged(topic, action, split[3]);
                }
                else if (responseAction == Action.READ)
                {
                    RecordReceived?.Invoke(this, new RecordReceivedArgs(topic, responseAction, split[2], int.Parse(split[3], CultureInfo.InvariantCulture), JsonConvert.DeserializeObject<Dictionary<string, object>>(split[4])));
                }
                else if (responseAction == Action.UPDATE)
                {
                    RecordUpdated?.Invoke(this, new RecordUpdatedArgs(topic, responseAction, split[2], int.Parse(split[3], CultureInfo.InvariantCulture), JsonConvert.DeserializeObject<Dictionary<string, object>>(split[4])));
                }
                else if (responseAction == Action.PATCH)
                {
                    RecordPatched?.Invoke(this, new RecordPatchedArgs(topic, responseAction, split[2], int.Parse(split[3], CultureInfo.InvariantCulture), split[4], Utils.ConvertPrefixedData(split[5])));
                }
                else
                {
                    OnError(topic, action, Constants.Errors.MESSAGE_PARSE_ERROR, "Unknown action " + action);
                }
            }
            else if (topic == Topic.RPC)
            {
                if (responseAction == Action.ACK)
                {
                    Acknoledged?.Invoke(this, new AcknoledgedWithUidArgs(topic, responseAction, split[2], split[3]));
                }
                //subscriber
                else if (responseAction == Action.RESPONSE)
                {
                    var dataWithType = Utils.ConvertPrefixedData(split[4]);
                    RemoteProcedureResultReceived?.Invoke(this, new RemoteProcedureMessageArgs(topic, responseAction, split[2], split[3], dataWithType.Key, dataWithType.Value));
                }

                //provider
                else if (responseAction == Action.REQUEST)
                {
                    var dataWithType = Utils.ConvertPrefixedData(split[4]);
                    PerformRemoteProcedureRequested?.Invoke(this, new RemoteProcedureMessageArgs(topic, responseAction, split[2], split[3], dataWithType.Key, dataWithType.Value));
                }
                else if (responseAction == Action.ERROR)
                {
                    OnError(topic, responseAction, split[2], split[3]);
                }
            }
            else
            {
                OnError(topic, action, Constants.Errors.MESSAGE_PARSE_ERROR, "Received message for unknown topic " + topic);
            }
        }

        void OnAcknoledged(Topic topic, Action action, string identifier)
        {
            Acknoledged?.Invoke(this, new AcknoledgedArgs(topic, action, identifier));
        }

        void OnError(Topic topic, Action action, string error, string message)
        {
            Error?.Invoke(this, new ErrorArgs(topic, action, error, message));
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        void Dispose(bool disposing)
        {
            if (disposing)
            {
                client.Close();
                (client as IDisposable).Dispose();
            }
        }
    }
}