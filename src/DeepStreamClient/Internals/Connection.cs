using System;
using System.Collections.Generic;
using System.Globalization;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Jil;

namespace DeepStreamNet
{
    class Connection : IDisposable
    {
        readonly TcpClient client;
        readonly CancellationToken cts;

        readonly string Host;
        readonly int Port;

        internal event EventHandler<AcknoledgedArgs> Acknoledged;

        internal event EventHandler<EventReceivedArgs> EventReceived;
        internal event EventHandler<EventListenerChangedArgs> EventListenerChanged;

        internal event EventHandler<RecordReceivedArgs> RecordReceived;
        internal event EventHandler<RecordUpdatedArgs> RecordUpdated;
        internal event EventHandler<RecordPatchedArgs> RecordPatched;

        public bool IsLoggedIn { get; set; }
        
        public Connection(string host, int port, CancellationToken token)
        {
            if (string.IsNullOrWhiteSpace(host))
                throw new ArgumentNullException(nameof(port));

            Host = host;
            Port = port;
            cts = token;
            
            client = new TcpClient();
            client.NoDelay = true;
        }

        public Task Open()
        {
            return client.ConnectAsync(Host, Port);
        }

        public async Task SendAsync(string command)
        {
            var stream = client.GetStream();
            
            var bytes = Encoding.ASCII.GetBytes(command);
            await stream.WriteAsync(bytes, 0, bytes.Length, cts);
            await stream.FlushAsync();            
        }

        public async Task<bool> SendWithAckAsync(Topic topic, Action action, Action expectedReceivedAction, string identifier)
        {
            var tcs = new TaskCompletionSource<bool>();

            EventHandler<AcknoledgedArgs> ackHandler = null;
            var timer = new System.Timers.Timer(1000);

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
                }
            };

            timer.Elapsed += (s, e) =>
            {
                Acknoledged -= ackHandler;
                timer.Dispose();
                tcs.TrySetException(new DeepStreamException("ACK Timeout"));
            };

            string command = Utils.BuildCommand(topic, action, identifier);

            Acknoledged += ackHandler;

            timer.Start();

            await SendAsync(command); 
            
            return await tcs.Task;
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
                while ((result = await stream.ReadAsync(buffer, 0, buffer.Length, cts)) != 0)
                {
                    var enc = sb + Encoding.UTF8.GetString(buffer,0,result);

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
                return;

            var responseAction = new Action(split[1]);
            var action = new Action(split.Length == 2 ? null : split[2]);
            var topic = new Topic(split[0]);
            
            if (responseAction == Action.ACK)
            {
                Acknoledged?.Invoke(this, new AcknoledgedArgs(topic, action, split.Length > 2 ? split[3] : null));
            }
            else if (topic == Topic.EVENT)
            {
                if (responseAction == Action.EVENT)
                {
                    var convertedDataWithType = ConvertPrefixedData(split[3]);
                    EventReceived?.Invoke(this, new EventReceivedArgs(split[2], convertedDataWithType.Key, convertedDataWithType.Value));
                }else if(responseAction == Action.SUBSCRIPTION_FOR_PATTERN_FOUND)
                {
                    EventListenerChanged?.Invoke(this, new EventListenerChangedArgs(split[2],EventListenerState.Add));
                }else if(responseAction == Action.SUBSCRIPTION_FOR_PATTERN_REMOVED)                
                {
                    EventListenerChanged?.Invoke(this, new EventListenerChangedArgs(split[2], EventListenerState.Remove));
                }
            }
            else if (topic == Topic.RECORD)
            {
                if (responseAction == Action.READ)
                {
                    RecordReceived?.Invoke(this, new RecordReceivedArgs(topic, responseAction, split[2], int.Parse(split[3], CultureInfo.InvariantCulture), JSON.Deserialize<Dictionary<string, object>>(split[4])));
                }
                else if (responseAction == Action.UPDATE)
                {
                    RecordUpdated?.Invoke(this, new RecordUpdatedArgs(topic, responseAction, split[2], int.Parse(split[3], CultureInfo.InvariantCulture), JSON.Deserialize<Dictionary<string, object>>(split[4])));
                }
                else if (responseAction == Action.PATCH)
                {
                    RecordPatched?.Invoke(this, new RecordPatchedArgs(topic, responseAction, split[2], int.Parse(split[3], CultureInfo.InvariantCulture), split[4], ConvertPrefixedData(split[5])));
                }
            }

        }

        static KeyValuePair<Type, object> ConvertPrefixedData(string dataWithTypePrefix)
        {
            var evtData = dataWithTypePrefix.Substring(1);

            switch (dataWithTypePrefix[0])
            {
                case Constants.Types.STRING:
                    return new KeyValuePair<Type, object>(typeof(string), evtData);
                case Constants.Types.NUMBER:
                    return new KeyValuePair<Type, object>(typeof(double), double.Parse(evtData, CultureInfo.InvariantCulture));
                case Constants.Types.TRUE:
                    return new KeyValuePair<Type, object>(typeof(bool), true);
                case Constants.Types.FALSE:
                    return new KeyValuePair<Type, object>(typeof(bool), false);
                case Constants.Types.NULL:
                    return new KeyValuePair<Type, object>(typeof(object), null);
                case Constants.Types.OBJECT:
                    return new KeyValuePair<Type, object>(typeof(object), JSON.DeserializeDynamic(evtData, Options.RFC1123));
                default:
                    return new KeyValuePair<Type, object>(typeof(string), evtData);
            }
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
