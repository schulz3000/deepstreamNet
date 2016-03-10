using System;
using System.Threading;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;
using Jil;

namespace DeepStreamNet
{
    public class DeepStreamClient : IDisposable
    {
        readonly Connection connection;

        readonly CancellationTokenSource cts = new CancellationTokenSource();
        
        public IDeepStreamEvents Events { get; }
        public IDeepStreamRecords Records { get; }
        public IDeepStreamRemoteProcedureCalls Rpcs  {  get; }


        /// <summary>
        /// DeepStreamClient for connecting to deepstream.io server
        /// </summary>
        /// <param name="host">deepstream.io endpoint address or ip</param>
        /// <param name="port">deeptstream.io endpoint port</param>
        public DeepStreamClient(string host, int port)
        {
            connection = new Connection(host, port, cts.Token);
            Events = new DeepStreamEvents(connection);
            Records = new DeepStreamRecords(connection);
            Rpcs = new DeepStreamRemoteProcedureCalls(connection);
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
            string credentials = "{}";
            if (!string.IsNullOrWhiteSpace(userName) && !string.IsNullOrWhiteSpace(password))
                credentials = JSON.SerializeDynamic(new { username = userName, password });

            await connection.Open();

            connection.StartMessageLoop();

            var result = await connection.SendWithAckAsync(Topic.AUTH, Action.REQUEST, Action.Empty,  credentials);

            connection.IsLoggedIn = result;

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
