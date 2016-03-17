using System;
using System.Collections.Generic;
using System.Reflection;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    class DeepStreamRemoteProcedureCalls : DeepStreamBase, IDeepStreamRemoteProcedureCalls
    {
        readonly Dictionary<string, Delegate> remoteProcedures = new Dictionary<string, Delegate>();

        public DeepStreamRemoteProcedureCalls(Connection connection, DeepStreamOptions options)
            : base(connection, options)
        {
            Connection.PerformRemoteProcedureRequested += Connection_PerformRemoteProcedureRequested;
        }

        async void Connection_PerformRemoteProcedureRequested(object sender, RemoteProcedureMessageArgs e)
        {
            var ackCommand = Utils.BuildCommand(Topic.RPC, Action.ACK, e.Identifier, e.Uid);
            await Connection.SendAsync(ackCommand);

            if (!remoteProcedures.ContainsKey(e.Identifier))
            {
                var unsupportedCommand = Utils.BuildCommand(Topic.RPC, Action.REJECTION, e.Identifier, e.Uid);
                await Connection.SendAsync(unsupportedCommand);
                return;
            }

            var procedure = remoteProcedures[e.Identifier];

            if (e.DataType != procedure.GetMethodInfo().GetParameters()[0].ParameterType)
            {
                var errorCommand = Utils.BuildCommand(Topic.RPC, Action.ERROR, "Input datatype don't match", e.Identifier, e.Uid);
                await Connection.SendAsync(errorCommand);
            }
            else {
                try
                {
                    var result = procedure.DynamicInvoke(e.Data);
                    var resultCommand = Utils.BuildCommand(Topic.RPC, Action.RESPONSE, e.Identifier, e.Uid, Utils.ConvertAndPrefixData(result));
                    await Connection.SendAsync(resultCommand);
                }
                catch
                {
                    var exceptionCommand = Utils.BuildCommand(Topic.RPC, Action.ERROR, "Procedure failed at execution", e.Identifier, e.Uid);
                    await Connection.SendAsync(exceptionCommand);
                }
            }
        }

        public async Task<IDisposable> RegisterProvider<TInput, TResult>(string procedureName, Func<TInput, TResult> procedure)
        {
            if (string.IsNullOrWhiteSpace(procedureName))
                throw new ArgumentNullException(nameof(procedureName));

            if (procedure == null)
                throw new ArgumentNullException(nameof(procedure));

            if (remoteProcedures.ContainsKey(procedureName))
                throw new DeepStreamException("Procedure with this name still registered");

            await Connection.SendWithAckAsync(Topic.RPC, Action.SUBSCRIBE, Action.SUBSCRIBE, procedureName, Options.RpcAckTimeout);
            remoteProcedures.Add(procedureName, procedure);

            return new DisposableAction(async () =>
            {
                remoteProcedures.Remove(procedureName);
                await Connection.SendWithAckAsync(Topic.RPC, Action.UNSUBSCRIBE, Action.UNSUBSCRIBE, procedureName, Options.RpcAckTimeout);
            });
        }

        public async Task<TResult> MakeRequest<TInput, TResult>(string procedureName, TInput parameter)
        {
            var tcs = new TaskCompletionSource<TResult>();

            if (string.IsNullOrWhiteSpace(procedureName))
                throw new ArgumentNullException(nameof(procedureName));

            var uid = Utils.CreateUid();

            EventHandler<RemoteProcedureMessageArgs> ackHandler = null;

            ackHandler = (s, e) =>
            {
                if (e.Uid != uid && e.Identifier != procedureName)
                    return;

                Connection.RemoteProcedureResultReceived -= ackHandler;

                tcs.TrySetResult((TResult)e.Data);
            };

            Connection.RemoteProcedureResultReceived += ackHandler;

            await SendWithAckAsync(Topic.RPC, Action.REQUEST, Action.ACK, procedureName, uid, parameter, Options.RpcAckTimeout);

            return await tcs.Task;
        }

        async Task<bool> SendWithAckAsync<T>(Topic topic, Action action, Action expectedReceivedAction, string identifier, string uid, T parameter, int ackTimeout)
        {
            var tcs = new TaskCompletionSource<bool>();

            EventHandler<AcknoledgedArgs> ackHandler = null;
            EventHandler<ErrorArgs> errorHandler = null;
            var timer = new AckTimer(ackTimeout);

            ackHandler = (s, e) =>
            {
                var args = e as AcknoledgedWithUidArgs;

                if (args != null && args.Topic == topic && args.Action == expectedReceivedAction && args.Identifier == identifier && args.Uid == uid)
                    tcs.TrySetResult(true);

                if (tcs.Task.IsCompleted)
                {
                    timer.Dispose();
                    Connection.Acknoledged -= ackHandler;
                    Connection.Error -= errorHandler;
                }
            };

            errorHandler = (s, e) =>
            {
                if (e.Topic == Topic.RPC && e.Error == "NO_RPC_PROVIDER")
                {
                    timer.Dispose();
                    Connection.Acknoledged -= ackHandler;
                    Connection.Error -= errorHandler;

                    tcs.TrySetException(new DeepStreamException("No RPC Provider found for " + e.Message));
                }
            };

            timer.Elapsed += (s, e) =>
            {
                timer.Dispose();
                Connection.Acknoledged -= ackHandler;
                Connection.Error -= errorHandler;
                tcs.TrySetException(new DeepStreamException("ACK Timeout"));
            };

            string command = Utils.BuildCommand(topic, action, identifier, uid, Utils.ConvertAndPrefixData(parameter));

            Connection.Acknoledged += ackHandler;
            Connection.Error += errorHandler;

            timer.Start();

            await Connection.SendAsync(command);

            return await tcs.Task;
        }
    }
}