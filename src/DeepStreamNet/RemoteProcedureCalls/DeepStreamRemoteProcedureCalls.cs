using DeepStreamNet.Contracts;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    class DeepStreamRemoteProcedureCalls : DeepStreamBase, IDeepStreamRemoteProcedureCalls, IDisposable
    {
        readonly HashSet<RemoteProcedure> remoteProcedures = new HashSet<RemoteProcedure>(new RemoteProcedureEqualityComparer());

        public DeepStreamRemoteProcedureCalls(Connection connection, DeepStreamOptions options)
            : base(connection, options)
        {
            Connection.PerformRemoteProcedureRequested += Connection_PerformRemoteProcedureRequested;
        }

        async void Connection_PerformRemoteProcedureRequested(object sender, RemoteProcedureMessageArgs e)
        {
            var command = Utils.BuildCommand(Topic.RPC, Action.ACK, Action.REQUEST, e.Identifier, e.Uid);
            Connection.Send(command);

            if (!remoteProcedures.Any(a => a.Name == e.Identifier))
            {
                var unsupportedCommand = Utils.BuildCommand(Topic.RPC, Action.REJECTION, e.Identifier, e.Uid);
                Connection.Send(unsupportedCommand);
                return;
            }

            var procedure = remoteProcedures.First(w => w.Name == e.Identifier);

            if (e.DataType != procedure.ParameterType)
            {
                var errorCommand = Utils.BuildCommand(Topic.RPC, Action.ERROR, "Input datatype don't match", e.Identifier, e.Uid);
                Connection.Send(errorCommand);
            }
            else
            {
                try
                {
                    var parameter = e.Data.ToObject(procedure.OriginalParameterType);

                    var rpcResponseType = typeof(RpcResponse<>).MakeGenericType(procedure.ReturnType);
                    var response = Activator.CreateInstance(rpcResponseType, e.Identifier, e.Uid, Connection);

                    var result = procedure.Procedure.DynamicInvoke(parameter, response);

                    if (result != null)
                        await (Task)result;
                }
                catch
                {
                    var exceptionCommand = Utils.BuildCommand(Topic.RPC, Action.ERROR, "Procedure failed at execution", e.Identifier, e.Uid);
                    Connection.Send(exceptionCommand);
                }
            }
        }

        public Task<IAsyncDisposable> RegisterProviderAsync<TInput, TResult>(string procedureName, Func<TInput, IRpcResponse<TResult>, Task> procedure)
        {
            return InnerRegisterProviderAsync(procedureName, procedure);
        }

        public Task<IAsyncDisposable> RegisterProviderAsync<TInput, TResult>(string procedureName, Action<TInput, IRpcResponse<TResult>> procedure)
        {
            return InnerRegisterProviderAsync(procedureName, procedure);
        }

        async Task<IAsyncDisposable> InnerRegisterProviderAsync(string procedureName, Delegate procedure)
        {
            if (string.IsNullOrWhiteSpace(procedureName))
                throw new ArgumentNullException(nameof(procedureName));

            if (procedure == null)
                throw new ArgumentNullException(nameof(procedure));

            if (remoteProcedures.Any(a => a.Name == procedureName))
                throw new DeepStreamException("Procedure with this name still registered");

            await Connection.SendWithAckAsync(Topic.RPC, Action.SUBSCRIBE, Action.ACK, procedureName, Options.RpcAckTimeout).ConfigureAwait(false);
            remoteProcedures.Add(new RemoteProcedure(procedureName, procedure));

            return new AsyncDisposableAction(async () =>
            {
                remoteProcedures.RemoveWhere(w => w.Name == procedureName);
                await Connection.SendWithAckAsync(Topic.RPC, Action.UNSUBSCRIBE, Action.ACK, procedureName, Options.RpcAckTimeout).ConfigureAwait(false);
            });
        }

        public async Task<TResult> MakeRequest<TInput, TResult>(string procedureName, TInput parameter)
        {
            var tcs = new TaskCompletionSource<TResult>();

            if (string.IsNullOrWhiteSpace(procedureName))
                throw new ArgumentNullException(nameof(procedureName));

            var uid = Utils.CreateUid();

            EventHandler<RemoteProcedureMessageArgs> ackHandler = null;
            EventHandler<ErrorArgs> errorHandler = null;

            ackHandler = (s, e) =>
            {
                if (e.Uid != uid || e.Identifier != procedureName)
                    return;

                Connection.Error -= errorHandler;
                Connection.RemoteProcedureResultReceived -= ackHandler;

                try
                {
                    var result = e.Data.ToObject<TResult>();
                    tcs.TrySetResult(result);
                }
                catch (Exception ex)
                {
                    tcs.TrySetException(new DeepStreamException("Wrong datatype received for " + procedureName, ex));
                }
            };

            errorHandler = (s, e) =>
            {
                if (e.Message != procedureName)
                    return;

                Connection.Error -= errorHandler;
                Connection.RemoteProcedureResultReceived -= ackHandler;

                tcs.TrySetException(new DeepStreamException(e.Error + " | " + e.Message));
            };

            Connection.RemoteProcedureResultReceived += ackHandler;
            Connection.Error += errorHandler;

            await SendWithAckAsync(Topic.RPC, Action.REQUEST, Action.ACK, procedureName, uid, parameter, Options.RpcAckTimeout).ConfigureAwait(false);

            return await tcs.Task.ConfigureAwait(false);
        }

        async Task<bool> SendWithAckAsync<T>(Topic topic, Action action, Action expectedReceivedAction, string identifier, string uid, T parameter, int ackTimeout)
        {
            var tcs = new TaskCompletionSource<bool>();

            EventHandler<AcknoledgedArgs> ackHandler = null;
            EventHandler<ErrorArgs> errorHandler = null;
            EventHandler timerHandler = null;
            var timer = new AckTimer(ackTimeout);

            ackHandler = (s, e) =>
            {
                var args = e as AcknoledgedWithUidArgs;

                if (args != null && args.Topic == topic && args.Action == expectedReceivedAction && args.Identifier == identifier && args.Uid == uid)
                    tcs.TrySetResult(true);

                if (tcs.Task.IsCompleted)
                {
                    timer.Elapsed -= timerHandler;
                    timer.Dispose();
                    Connection.Acknoledged -= ackHandler;
                    Connection.Error -= errorHandler;
                }
            };

            errorHandler = (s, e) =>
            {
                if (e.Topic == Topic.RPC && e.Error == Constants.Errors.NO_RPC_PROVIDER)
                {
                    timer.Elapsed -= timerHandler;
                    timer.Dispose();
                    Connection.Acknoledged -= ackHandler;
                    Connection.Error -= errorHandler;

                    tcs.TrySetException(new DeepStreamException("No RPC Provider found for " + e.Message));
                }
            };

            timerHandler = (s, e) =>
            {
                timer.Elapsed -= timerHandler;
                timer.Dispose();
                Connection.Acknoledged -= ackHandler;
                Connection.Error -= errorHandler;
                tcs.TrySetException(new DeepStreamException(Constants.Errors.ACK_TIMEOUT));
            };

            timer.Elapsed += timerHandler;

            var command = Utils.BuildCommand(topic, action, identifier, uid, Utils.ConvertAndPrefixData(parameter));

            Connection.Acknoledged += ackHandler;
            Connection.Error += errorHandler;

            timer.Start();

            Connection.Send(command);

            return await tcs.Task.ConfigureAwait(false);
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
                if (Connection != null)
                    Connection.PerformRemoteProcedureRequested -= Connection_PerformRemoteProcedureRequested;
            }
        }
    }
}