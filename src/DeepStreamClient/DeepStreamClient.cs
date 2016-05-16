using System;
using System.Threading;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;
using Newtonsoft.Json;

namespace DeepStreamNet
{
    /// <summary>
    /// DeepStreamClient
    /// </summary>
    public class DeepStreamClient : IDisposable
    {
        readonly Connection connection;

        readonly CancellationTokenSource cts = new CancellationTokenSource();

        /// <summary>
        /// DeepStreamEvents
        /// </summary>
        public IDeepStreamEvents Events { get; }
        /// <summary>
        /// DeepStreamRecords
        /// </summary>
        public IDeepStreamRecords Records { get; }
        /// <summary>
        /// DeepStreamRemoteProcedures
        /// </summary>
        public IDeepStreamRemoteProcedureCalls Rpcs { get; }

        /// <summary>
        /// DeepStreamClient for connecting to deepstream.io server
        /// </summary>
        /// <param name="host">deepstream.io endpoint address or ip</param>
        /// <param name="port">deeptstream.io endpoint port</param>
        /// <param name="options" cref="DeepStreamOptions">set options other then default</param>
        public DeepStreamClient(string host, int port, DeepStreamOptions options)
        {
            connection = new Connection(host, port, cts.Token);
            Events = new DeepStreamEvents(connection, options);
            Records = new DeepStreamRecords(connection, options);
            Rpcs = new DeepStreamRemoteProcedureCalls(connection, options);
        }


        /// <summary>
        /// DeepStreamClient for connecting to deepstream.io server
        /// </summary>
        /// <param name="host">deepstream.io endpoint address or ip</param>
        /// <param name="port">deeptstream.io endpoint port</param>
        public DeepStreamClient(string host, int port)
            : this(host, port, new DeepStreamOptions())
        {

        }

        /// <summary>
        /// Anonymous Login to deepstream.io server
        /// </summary>
        /// <returns>true if login was successful otherwise false</returns>
        public Task<bool> LoginAsync()
        {
            return LoginAsync(null, null);
        }

        /// <summary>
        /// Login to deepstream.io server
        /// </summary>
        /// <param name="userName">Username for authentication on deepstream.io server</param>
        /// <param name="password">Password for authentication on deepstream.io server</param>
        /// <returns>true if login was successful otherwise false</returns>
        public async Task<bool> LoginAsync(string userName, string password)
        {
            var tcs = new TaskCompletionSource<bool>();

            string credentials = "{}";
            if (!string.IsNullOrWhiteSpace(userName) && !string.IsNullOrWhiteSpace(password))
                credentials = JsonConvert.SerializeObject(new { username = userName, password });

            await connection.Open();
            connection.State = ConnectionState.AWAITING_AUTHENTICATION;

            connection.StartMessageLoop();

            EventHandler<ErrorArgs> errorHandler = null;

            errorHandler = (s, e) =>
            {
                if (e.Topic != Topic.AUTH && e.Action != Action.ERROR)
                    return;

                connection.Error -= errorHandler;
                connection.State = ConnectionState.AWAITING_AUTHENTICATION;

                tcs.TrySetException(new DeepStreamException(e.Error,e.Message));
            };

            connection.Error += errorHandler;

            connection.State = ConnectionState.AUTHENTICATING;
            var result = await connection.SendWithAckAsync(Topic.AUTH, Action.REQUEST, Action.Empty, credentials, 1000);

            if (result)
            {
                connection.State = ConnectionState.OPEN;
            }

            return result;
        }

        /// <summary>
        /// Closing connection to deepstream.io server
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        void Dispose(bool disposing)
        {
            if (disposing)
            {
                cts.Dispose();

                connection.Dispose();
            }
        }
    }
}