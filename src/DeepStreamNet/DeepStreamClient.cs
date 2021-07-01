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
        private Connection _connection;
        /// <summary>
        /// inner Connection
        /// </summary>
        private Connection Connection => _connection;

        private IDeepStreamEvents _events;
        /// <summary>
        /// DeepStreamEvents
        /// </summary>
        /// <exception cref="DeepStreamException"></exception>
        public IDeepStreamEvents Events
        {
            get
            {
                if (_events == null)
                {
                    throw new DeepStreamException("not initialized", "please login first");
                }

                return _events;
            }
        }

        private IDeepStreamRecords _records;
        /// <summary>
        /// DeepStreamRecords
        /// </summary>
        /// <exception cref="DeepStreamException"></exception>
        public IDeepStreamRecords Records
        {
            get
            {
                if (_records == null)
                {
                    throw new DeepStreamException("not initialized", "please login first");
                }

                return _records;
            }
        }

        private IDeepStreamRemoteProcedureCalls _rpcs;
        /// <summary>
        /// DeepStreamRemoteProcedures
        /// </summary>
        /// <exception cref="DeepStreamException"></exception>
        public IDeepStreamRemoteProcedureCalls Rpcs
        {
            get
            {
                if (_rpcs == null)
                {
                    throw new DeepStreamException("not initialized", "please login first");
                }

                return _rpcs;
            }
        }

        private IDeepStreamPresence _presence;
        /// <summary>
        /// DeepStreamPresence
        /// </summary>
        /// <exception cref="DeepStreamException"></exception>
        public IDeepStreamPresence Presence
        {
            get
            {
                if (_presence == null)
                {
                    throw new DeepStreamException("not initialized", "please login first");
                }

                return _presence;
            }
        }

        private readonly DeepStreamOptions _options;

        /// <summary>
        /// DeepStreamClient for connecting to deepstream.io server
        /// </summary>
        /// <param name="host">deepstream.io endpoint address or ip</param>
        /// <param name="port">deeptstream.io endpoint port</param>
        /// <param name="path">deeptstream.io endpoint path</param>
        /// <param name="useSecureConnection"></param>
        /// <param name="options" cref="DeepStreamOptions">set options other then default</param>
        public DeepStreamClient(string host, short port, string path, bool useSecureConnection, DeepStreamOptions options)
        {
            _connection = new Connection(host, port, path, useSecureConnection);
            _options = options;
        }

        /// <summary>
        /// DeepStreamClient for connecting to deepstream.io server
        /// </summary>
        /// <param name="host">deepstream.io endpoint address or ip</param>
        /// <param name="port">deeptstream.io endpoint port</param>
        /// <param name="path">deeptstream.io endpoint path</param>
        /// <param name="useSecureConnection"></param>
        public DeepStreamClient(string host, short port = 6020, string path = "deepstream", bool useSecureConnection = false)
            : this(host, port, path, useSecureConnection, new DeepStreamOptions())
        {
        }

        /// <summary>
        /// Anonymous Login to deepstream.io server
        /// </summary>
        /// <returns>true if login was successful otherwise false</returns>
        public Task<bool> LoginAsync() => LoginAsync(Constants.EmptyCredentials);

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
            {
                credentials = JsonConvert.SerializeObject(new { username = userName, password });
            }

            return LoginAsync(credentials);
        }

        private async Task<bool> LoginAsync(string credentials)
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

            Connection.Error += ErrorHandler;

            Connection.State = ConnectionState.AUTHENTICATING;

            var result = await Connection.SendWithAckAsync(Topic.AUTH, Action.REQUEST, Action.Empty, credentials, _options.SubscriptionTimeout).ConfigureAwait(false);

            if (result)
            {
                Connection.State = ConnectionState.OPEN;
                Connection.PingReceived += Connection_PingReceived;
                _events = new DeepStreamEvents(Connection, _options);
                _records = new DeepStreamRecords(Connection, _options);
                _rpcs = new DeepStreamRemoteProcedureCalls(Connection, _options);
                _presence = new DeepStreamPresence(Connection, _options);
            }

            return result;

            void ErrorHandler(object sender, ErrorArgs e)
            {
                if (e.Action != Action.ERROR)
                {
                    return;
                }

                Connection.Error -= ErrorHandler;
                Connection.State = ConnectionState.AWAITING_AUTHENTICATION;

                tcs.TrySetException(new DeepStreamException(e.Error, e.Message));
            }
        }

        private Task<bool?> RegisterAuthChallenge(string credentials)
        {
            var tcs = new TaskCompletionSource<bool?>();

            Connection.ChallengeReceived += ChallengeReceivedHandler;

            return tcs.Task;

            async void ChallengeReceivedHandler(object sender, ChallengeEventArgs e)
            {
                if (e.Action == Action.CHALLENGE)
                {
                    Connection.Send(Utils.BuildCommand(Topic.CONNECTION, Action.CHALLENGE_RESPONSE, Connection.Endpoint));
                }
                else if (e.Action == Action.REDIRECT && e is RedirectionEventArgs redirectArgs)
                {
                    Connection.ChallengeReceived -= ChallengeReceivedHandler;
                    tcs.SetResult(await RecreateClientAsync(redirectArgs.RedirectUrl, credentials).ConfigureAwait(false));
                }
                else if (e.Action == Action.ACK)
                {
                    Connection.ChallengeReceived -= ChallengeReceivedHandler;
                    tcs.SetResult(null);
                }
            }
        }

        private void Connection_PingReceived(object sender, EventArgs e) => Connection.Send(Utils.BuildCommand(Topic.CONNECTION, Action.PONG));

        private Task<bool> RecreateClientAsync(string endPointUrl, string credentials)
        {
            Connection.Dispose();
            _connection = new Connection(endPointUrl);
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

        private void Dispose(bool disposing)
        {
            if (disposing)
            {
                (_rpcs as IDisposable)?.Dispose();
                (_records as IDisposable)?.Dispose();
                Connection.PingReceived -= Connection_PingReceived;
                Connection.Dispose();
            }
        }
    }
}