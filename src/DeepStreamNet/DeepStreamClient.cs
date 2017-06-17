using DeepStreamNet.Contracts;
using Newtonsoft.Json;
using System;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    /// <summary>
    /// DeepStreamClient
    /// </summary>
    public sealed class DeepStreamClient : IDisposable
    {
        Connection connection;
        /// <summary>
        /// inner Connection
        /// </summary>
        Connection Connection { get { return connection; } }

        IDeepStreamEvents events;
        /// <summary>
        /// DeepStreamEvents
        /// </summary>
        public IDeepStreamEvents Events
        {
            get
            {
                if (events == null)
                    throw new DeepStreamException("not initialized", "please login first");
                return events;
            }
        }

        IDeepStreamRecords records;
        /// <summary>
        /// DeepStreamRecords
        /// </summary>
        public IDeepStreamRecords Records
        {
            get
            {
                if (records == null)
                    throw new DeepStreamException("not initialized", "please login first");
                return records;
            }
        }

        IDeepStreamRemoteProcedureCalls rpcs;
        /// <summary>
        /// DeepStreamRemoteProcedures
        /// </summary>
        public IDeepStreamRemoteProcedureCalls Rpcs
        {
            get
            {
                if (rpcs == null)
                    throw new DeepStreamException("not initialized", "please login first");
                return rpcs;
            }
        }

        readonly DeepStreamOptions Options;

        /// <summary>
        /// DeepStreamClient for connecting to deepstream.io server
        /// </summary>
        /// <param name="host">deepstream.io endpoint address or ip</param>
        /// <param name="port">deeptstream.io endpoint port</param>
        /// <param name="path">deeptstream.io endpoint path</param>
        /// <param name="useSecureConnection"></param>
        /// <param name="options" cref="DeepStreamOptions">set options other then default</param>
        public DeepStreamClient(string host, int port, string path, bool useSecureConnection, DeepStreamOptions options)
        {
            connection = new Connection(host, port, path, useSecureConnection);
            Options = options;
        }

        /// <summary>
        /// DeepStreamClient for connecting to deepstream.io server
        /// </summary>
        /// <param name="host">deepstream.io endpoint address or ip</param>
        /// <param name="port">deeptstream.io endpoint port</param>
        /// <param name="path">deeptstream.io endpoint path</param>
        /// <param name="useSecureConnection"></param>
        public DeepStreamClient(string host, int port = 6020, string path = "deepstream", bool useSecureConnection = false)
            : this(host, port, path, useSecureConnection, new DeepStreamOptions())
        {
        }

        /// <summary>
        /// Anonymous Login to deepstream.io server
        /// </summary>
        /// <returns>true if login was successful otherwise false</returns>
        public Task<bool> LoginAsync()
        {
            return LoginAsync(Constants.EmptyCredentials);
        }

        /// <summary>
        /// Login to deepstream.io server
        /// </summary>
        /// <param name="userName">Username for authentication on deepstream.io server</param>
        /// <param name="password">Password for authentication on deepstream.io server</param>
        /// <returns>true if login was successful otherwise false</returns>
        public Task<bool> LoginAsync(string userName, string password)
        {
            string credentials = Constants.EmptyCredentials;
            if (!string.IsNullOrWhiteSpace(userName) && !string.IsNullOrWhiteSpace(password))
                credentials = JsonConvert.SerializeObject(new { username = userName, password });

            return LoginAsync(credentials);
        }

        async Task<bool> LoginAsync(string credentials)
        {
            var tcs = new TaskCompletionSource<bool>();

            await Connection.OpenAsync().ConfigureAwait(false);
            Connection.State = ConnectionState.AWAITING_AUTHENTICATION;

            Connection.StartMessageLoop();

            var challengeResult = await RegisterAuthChallenge(credentials).ConfigureAwait(false);
            if (challengeResult.HasValue)
            {
                return challengeResult.Value;
            }

            EventHandler<ErrorArgs> errorHandler = null;

            errorHandler = (s, e) =>
            {
                if (e.Action != Action.ERROR)
                    return;

                Connection.Error -= errorHandler;
                Connection.State = ConnectionState.AWAITING_AUTHENTICATION;

                tcs.TrySetException(new DeepStreamException(e.Error, e.Message));
            };

            Connection.Error += errorHandler;

            Connection.State = ConnectionState.AUTHENTICATING;

            var result = await Connection.SendWithAckAsync(Topic.AUTH, Action.REQUEST, Action.Empty, credentials, Options.SubscriptionTimeout).ConfigureAwait(false);

            if (result)
            {
                Connection.State = ConnectionState.OPEN;
                Connection.PingReceived += Connection_PingReceived;
                events = new DeepStreamEvents(Connection, Options);
                records = new DeepStreamRecords(Connection, Options);
                rpcs = new DeepStreamRemoteProcedureCalls(Connection, Options);
            }

            return result;
        }

        Task<bool?> RegisterAuthChallenge(string credentials)
        {
            var tcs = new TaskCompletionSource<bool?>();

            EventHandler<ChallengeEventArgs> handler = null;
            handler = async (s, e) =>
            {
                if (e.Action == Action.CHALLENGE)
                {
                    Connection.Send(Utils.BuildCommand(Topic.CONNECTION, Action.CHALLENGE_RESPONSE, Connection.Endpoint));
                }
                else if (e.Action == Action.REDIRECT && e is RedirectionEventArgs redirectArgs)
                {
                    Connection.ChallengeReceived -= handler;
                    tcs.SetResult(await RecreateClientAsync(redirectArgs.RedirectUrl, credentials).ConfigureAwait(false));
                }
                else if (e.Action == Action.ACK)
                {
                    Connection.ChallengeReceived -= handler;
                    tcs.SetResult(null);
                }
            };

            Connection.ChallengeReceived += handler;

            return tcs.Task;
        }

        void Connection_PingReceived(object sender, EventArgs e) => Connection.Send(Utils.BuildCommand(Topic.CONNECTION, Action.PONG));

        Task<bool> RecreateClientAsync(string endPointUrl, string credentials)
        {
            Connection.Dispose();
            connection = new Connection(endPointUrl);
            return LoginAsync(credentials);
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
                Connection.PingReceived -= Connection_PingReceived;
                Connection.Dispose();
            }
        }
    }
}