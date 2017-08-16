using Newtonsoft.Json.Linq;
using SuperSocket.ClientEngine;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Threading.Tasks;
using WebSocket4Net;

namespace DeepStreamNet
{
    class Connection : IDisposable
    {
        WebSocket client;

        public ConnectionState State { get; internal set; }
        public string Endpoint { get; private set; }

        internal event EventHandler<ChallengeEventArgs> ChallengeReceived;

        internal event EventHandler PingReceived;

        internal event EventHandler<AcknoledgedArgs> Acknoledged;

        internal event EventHandler<ErrorArgs> Error;

        internal event EventHandler<EventReceivedArgs> EventReceived;

        internal event EventHandler<EventListenerChangedEventArgs> EventListenerChanged;

        internal event EventHandler<RecordReceivedArgs> RecordReceived;

        internal event EventHandler<RecordUpdatedArgs> RecordUpdated;

        internal event EventHandler<RecordPatchedArgs> RecordPatched;

        internal event EventHandler<HasRecordArgs> HasRecordReceived;

        internal event EventHandler<RecordListenerChangedEventArgs> RecordListenerChanged;

        internal event EventHandler<RemoteProcedureMessageArgs> PerformRemoteProcedureRequested;

        internal event EventHandler<RemoteProcedureMessageArgs> RemoteProcedureResultReceived;

        public Connection(string host, int port, string path, bool useSecureConnection)
        {
            if (port < 1)
                throw new ArgumentOutOfRangeException(nameof(port));

            if (string.IsNullOrWhiteSpace(host))
                throw new ArgumentNullException(nameof(host));

            if (string.IsNullOrWhiteSpace(path))
                throw new ArgumentNullException(nameof(path));

            var protocol = useSecureConnection ? "wss" : "ws";

            Connect($"{protocol}://{host}:{port.ToString()}/{path}");
        }

        internal Connection(string url)
        {
            if (string.IsNullOrWhiteSpace(url))
                throw new ArgumentNullException(nameof(url));

            Connect(url);
        }

        void Connect(string url)
        {
            State = ConnectionState.NONE;

            Endpoint = url;

            client = new WebSocket(Endpoint, userAgent: "deepstreamNet Client");

#if !__IOS__
            client.NoDelay = true;
#endif
        }

        public Task OpenAsync()
        {
            var tcs = new TaskCompletionSource<bool>();

            EventHandler openHandler = null;
            openHandler = (s, e) =>
            {
                client.Opened -= openHandler;
                tcs.SetResult(true);
            };
            client.Opened += openHandler;

            EventHandler<ErrorEventArgs> errorHandler = null;
            errorHandler = (s, e) =>
            {
                client.Error -= errorHandler;
                tcs.TrySetException(e.Exception);
            };
            client.Error += errorHandler;

            client.Open();

            return tcs.Task;
        }

        public void Send(string command)
        {
            client.Send(command);
        }

        public Task<bool> SendWithAckAsync(Topic topic, Action action, Action expectedReceivedAction, string identifier, int ackTimeout, params string[] additionalParams)
        {
            var tcs = new TaskCompletionSource<bool>();

            EventHandler<AcknoledgedArgs> ackHandler = null;
            EventHandler<ErrorArgs> errorHandler = null;
            EventHandler timerHandler = null;
            var timer = new AckTimer(ackTimeout);

            ackHandler = (s, e) =>
            {
                if (e.Topic == topic && e.Action == expectedReceivedAction && e.Identifier == identifier)
                    tcs.TrySetResult(true);
                else if (e.Topic == Topic.AUTH)
                    tcs.TrySetResult(true);

                if (tcs.Task.IsCompleted)
                {
                    timer.Elapsed -= timerHandler;
                    timer.Dispose();
                    Acknoledged -= ackHandler;
                    Error -= errorHandler;
                }
            };

            errorHandler = (s, e) =>
            {
                timer.Elapsed -= timerHandler;
                timer.Dispose();
                Acknoledged -= ackHandler;
                Error -= errorHandler;

                if (e.Topic == topic && e.Action == Action.ERROR)
                    tcs.TrySetException(new DeepStreamException(e.Error, e.Message));
            };

            timerHandler = (s, e) =>
            {
                timer.Elapsed -= timerHandler;
                timer.Dispose();
                Acknoledged -= ackHandler;
                Error -= errorHandler;

                tcs.TrySetException(new DeepStreamException(Constants.Errors.ACK_TIMEOUT));
            };

            var parameter = new List<string>();

            if (additionalParams.Length > 0)
            {
                parameter.Add(identifier);
                parameter.AddRange(additionalParams);
            }
            else
            {
                parameter.Add(identifier);
            }

            var command = Utils.BuildCommand(topic, action, parameter.ToArray());

            timer.Elapsed += timerHandler;
            Acknoledged += ackHandler;
            Error += errorHandler;

            timer.Start();

            Send(command);

            return tcs.Task;
        }

        public void StartMessageLoop()
        {
            client.MessageReceived -= Client_MessageReceived;
            client.MessageReceived += Client_MessageReceived;
        }

        void Client_MessageReceived(object sender, MessageReceivedEventArgs e)
        {
            var groups = e.Message.Split(Constants.GroupSeperator);

            for (int i = 0; i < groups.Length - 1; i++)
            {
                Notify(groups[i]);
            }
        }

        void Notify(string value)
        {
            Console.WriteLine(value);
            var split = value.Split(Constants.RecordSeperator);

            if (split.Length < 2)
            {
                OnError(Topic.Empty, Action.Empty, Constants.Errors.MESSAGE_PARSE_ERROR, "Insufficiant message parts");
                return;
            }

            var responseAction = new Action(split[1]);
            var action = new Action(split.Length == 2 ? null : split[2]);
            var topic = new Topic(split[0]);

            if (topic == Topic.CONNECTION)
            {
                if (responseAction == Action.CHALLENGE)
                    ChallengeReceived?.Invoke(this, new ChallengeEventArgs(topic, responseAction));
                else if (responseAction == Action.PING)
                    PingReceived?.Invoke(this, EventArgs.Empty);
                else if (responseAction == Action.REDIRECT)
                    ChallengeReceived?.Invoke(this, new RedirectionEventArgs(topic, responseAction, split[2]));
                else if (responseAction == Action.ACK)
                    ChallengeReceived?.Invoke(this, new ChallengeEventArgs(topic, responseAction));
                else if (responseAction == Action.REJECTION)
                    OnError(topic, action, "Connection rejected", "Connection rejected");
            }
            else if (topic == Topic.AUTH)
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
                    OnError(topic, action, Constants.Errors.MESSAGE_PARSE_ERROR, "Unknown action " + action.ToString());
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
                    EventReceived?.Invoke(this, new EventReceivedArgs(split[2], convertedDataWithType.Key, convertedDataWithType.Value.ToObject(convertedDataWithType.Key)));
                }
                else if (responseAction == Action.SUBSCRIPTION_FOR_PATTERN_FOUND)
                {
                    EventListenerChanged?.Invoke(this, new EventListenerChangedEventArgs(split[2], split[3], ListenerState.Add));
                }
                else if (responseAction == Action.SUBSCRIPTION_FOR_PATTERN_REMOVED)
                {
                    EventListenerChanged?.Invoke(this, new EventListenerChangedEventArgs(split[2], split[3], ListenerState.Remove));
                }
                else
                {
                    OnError(topic, action, Constants.Errors.MESSAGE_PARSE_ERROR, "Unknown action " + action.ToString());
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
                    RecordReceived?.Invoke(this, new RecordReceivedArgs(topic, responseAction, split[2], int.Parse(split[3], CultureInfo.InvariantCulture), JToken.Parse(split[4])));
                }
                else if (responseAction == Action.UPDATE)
                {
                    RecordUpdated?.Invoke(this, new RecordUpdatedArgs(topic, responseAction, split[2], int.Parse(split[3], CultureInfo.InvariantCulture), JToken.Parse(split[4])));
                }
                else if (responseAction == Action.PATCH)
                {
                    RecordPatched?.Invoke(this, new RecordPatchedArgs(topic, responseAction, split[2], int.Parse(split[3], CultureInfo.InvariantCulture), split[4], Utils.ConvertPrefixedData(split[5]).Value));
                }
                else if (responseAction == Action.HAS)
                {
                    HasRecordReceived?.Invoke(this, new HasRecordArgs(topic, responseAction, split[2], Constants.Types.TRUE.ToString().Equals(split[3], StringComparison.Ordinal)));
                }
                else if (responseAction == Action.SUBSCRIPTION_FOR_PATTERN_FOUND)
                {
                    RecordListenerChanged?.Invoke(this, new RecordListenerChangedEventArgs(split[2], split[3], ListenerState.Add));
                }
                else if (responseAction == Action.SUBSCRIPTION_FOR_PATTERN_REMOVED)
                {
                    RecordListenerChanged?.Invoke(this, new RecordListenerChangedEventArgs(split[2], split[3], ListenerState.Remove));
                }
                else if (responseAction == Action.WRITE_ACKNOWLEDGEMENT)
                {
                    if ("L".Equals(split[4], StringComparison.Ordinal))
                    {
                        OnAcknoledged(topic, responseAction, split[2]);
                    }
                    else
                    {
                        OnError(topic, responseAction, "Record " + split[2], split[4].Substring(1, split[4].Length - 2));
                    }
                }
                else
                {
                    OnError(topic, action, Constants.Errors.MESSAGE_PARSE_ERROR, "Unknown action " + action.ToString());
                }
            }
            else if (topic == Topic.RPC)
            {
                if (responseAction == Action.ACK)
                {
                    var subAction = new Action(split[2]);

                    if (subAction == Action.SUBSCRIBE || subAction == Action.UNSUBSCRIBE)
                        Acknoledged?.Invoke(this, new RpcAcknoledgedArgs(topic, responseAction, subAction, split[3]));
                    else if (subAction == Action.REQUEST)
                        Acknoledged?.Invoke(this, new AcknoledgedWithUidArgs(topic, responseAction, split[3], split[4]));
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
                else
                {
                    OnError(topic, action, Constants.Errors.MESSAGE_PARSE_ERROR, "Unknown action " + action.ToString());
                }
            }
            else
            {
                OnError(topic, action, Constants.Errors.MESSAGE_PARSE_ERROR, "Received message for unknown topic " + topic.ToString());
            }
        }

        void OnAcknoledged(Topic topic, Action action, string identifier) => Acknoledged?.Invoke(this, new AcknoledgedArgs(topic, action, identifier));

        void OnError(Topic topic, Action action, string error, string message) => Error?.Invoke(this, new ErrorArgs(topic, action, error, message));

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        void Dispose(bool disposing)
        {
            if (disposing)
            {
                client.MessageReceived -= Client_MessageReceived;

#if NET40 || NET45 || NET451
                client.Close();
                (client as IDisposable)?.Dispose();
#else
                client.Dispose();
#endif
            }
        }
    }
}