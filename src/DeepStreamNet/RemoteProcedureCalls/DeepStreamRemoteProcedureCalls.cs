﻿using DeepStreamNet.Contracts;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    internal class DeepStreamRemoteProcedureCalls : DeepStreamBase, IDeepStreamRemoteProcedureCalls, IDisposable
    {
        private readonly HashSet<RemoteProcedure> remoteProcedures = new(new RemoteProcedureEqualityComparer());

        private readonly Dictionary<Type, Func<string, string, Connection, IRpcResponse>> rpcResponses = new();

        public DeepStreamRemoteProcedureCalls(Connection connection, DeepStreamOptions options)
            : base(connection, options)
        {
            Connection.PerformRemoteProcedureRequested += Connection_PerformRemoteProcedureRequested;
        }

        private Func<string, string, Connection, IRpcResponse> GetRpcResponseByType(Type type)
        {
            if (rpcResponses.ContainsKey(type))
            {
                return rpcResponses[type];
            }

            var func = CreateRpcResponseFunc(type);

            rpcResponses.Add(type, func);

            return func;
        }

        private static Func<string, string, Connection, IRpcResponse> CreateRpcResponseFunc(Type type)
        {
            var rpcResponseType = typeof(RpcResponse<>).MakeGenericType(type);
            var expParameter = new[] {
                        Expression.Parameter(typeof(string)),
                        Expression.Parameter(typeof(string)),
                        Expression.Parameter(typeof(Connection))
                    };
            return Expression.Lambda<Func<string, string, Connection, IRpcResponse>>(Expression.New(rpcResponseType.GetTypeInfo().GetConstructor(new[] { typeof(string), typeof(string), typeof(Connection) }), expParameter), expParameter).Compile();
        }

        private async void Connection_PerformRemoteProcedureRequested(object sender, RemoteProcedureMessageArgs e)
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

                    var func = GetRpcResponseByType(procedure.ReturnType);

                    var response = func(e.Identifier, e.Uid, Connection);

                    var result = procedure.Procedure.GetMethodInfo().Invoke(procedure.Procedure.Target, new[] { parameter, response });

                    if (result != null)
                        await ((Task)result).ConfigureAwait(false);
                }
                catch
                {
                    var exceptionCommand = Utils.BuildCommand(Topic.RPC, Action.ERROR, "Procedure failed at execution", e.Identifier, e.Uid);
                    Connection.Send(exceptionCommand);
                }
            }
        }

        public Task<IAsyncDisposable> RegisterProviderAsync<TInput, TResult>(string procedureName, Func<TInput, IRpcResponse<TResult>, Task> procedure)
            => InnerRegisterProviderAsync(procedureName, procedure);

        public Task<IAsyncDisposable> RegisterProviderAsync<TInput, TResult>(string procedureName, Action<TInput, IRpcResponse<TResult>> procedure)
            => InnerRegisterProviderAsync(procedureName, procedure);

        private async Task<IAsyncDisposable> InnerRegisterProviderAsync(string procedureName, Delegate procedure)
        {
            if (string.IsNullOrWhiteSpace(procedureName))
            {
                throw new ArgumentNullException(nameof(procedureName));
            }

            if (procedure == null)
            {
                throw new ArgumentNullException(nameof(procedure));
            }

            if (remoteProcedures.Any(a => a.Name == procedureName))
            {
                throw new DeepStreamException("Procedure with this name already registered");
            }

            await Connection.SendWithAckAsync(Topic.RPC, Action.SUBSCRIBE, Action.ACK, procedureName, Options.RpcAckTimeout).ConfigureAwait(false);
            remoteProcedures.Add(new RemoteProcedure(procedureName, procedure));

            return new AsyncDisposableAction(() =>
            {
                remoteProcedures.RemoveWhere(w => w.Name == procedureName);
                return Connection.SendWithAckAsync(Topic.RPC, Action.UNSUBSCRIBE, Action.ACK, procedureName, Options.RpcAckTimeout);
            });
        }

        public async Task<TResult> MakeRequestAsync<TInput, TResult>(string procedureName, TInput parameter)
        {
            var tcs = new TaskCompletionSource<TResult>();

            if (string.IsNullOrWhiteSpace(procedureName))
            {
                throw new ArgumentNullException(nameof(procedureName));
            }

            var uid = Utils.CreateUid();

            Connection.RemoteProcedureResultReceived += ackHandler;
            Connection.Error += errorHandler;

            await SendWithAckAsync(Topic.RPC, Action.REQUEST, Action.ACK, procedureName, uid, parameter, Options.RpcAckTimeout).ConfigureAwait(false);

            return await tcs.Task.ConfigureAwait(false);

            void ackHandler(object sender, RemoteProcedureMessageArgs e)
            {
                if (e.Uid != uid || e.Identifier != procedureName)
                {
                    return;
                }

                Connection.Error -= errorHandler;
                Connection.RemoteProcedureResultReceived -= ackHandler;

                try
                {
                    if (!Utils.IsJTokenTypeEqualNetType(e.Data.Type, typeof(TResult)))
                    {
                        tcs.TrySetException(new DeepStreamException("Wrong datatype received for " + procedureName));
                    }
                    else
                    {
                        var result = e.Data.ToObject<TResult>();
                        tcs.TrySetResult(result);
                    }
                }
                catch (Exception ex)
                {
                    tcs.TrySetException(new DeepStreamException("Wrong datatype received for " + procedureName, ex));
                }
            }

            void errorHandler(object sender, ErrorArgs e)
            {
                if (e.Message != procedureName)
                {
                    return;
                }

                Connection.Error -= errorHandler;
                Connection.RemoteProcedureResultReceived -= ackHandler;

                tcs.TrySetException(new DeepStreamException(e.Error + " | " + e.Message));
            }
        }

        private Task<bool> SendWithAckAsync<T>(Topic topic, Action action, Action expectedReceivedAction, string identifier, string uid, T parameter, int ackTimeout)
        {
            var tcs = new TaskCompletionSource<bool>();
            var timer = new AckTimer(ackTimeout);
            timer.Elapsed += timerHandler;

            var command = Utils.BuildCommand(topic, action, identifier, uid, Utils.ConvertAndPrefixData(parameter));

            Connection.Acknoledged += ackHandler;
            Connection.Error += errorHandler;

            timer.Start();

            Connection.Send(command);

            return tcs.Task;

            void ackHandler(object sender, AcknoledgedArgs e)
            {
                if (e is AcknoledgedWithUidArgs args && args.Topic == topic && args.Action == expectedReceivedAction && args.Identifier == identifier && args.Uid == uid)
                {
                    tcs.TrySetResult(true);
                }

                if (tcs.Task.IsCompleted)
                {
                    timer.Elapsed -= timerHandler;
                    timer.Dispose();
                    Connection.Acknoledged -= ackHandler;
                    Connection.Error -= errorHandler;
                }
            }

            void errorHandler(object sender, ErrorArgs e)
            {
                if (e.Topic == Topic.RPC && e.Error == Constants.Errors.NO_RPC_PROVIDER)
                {
                    timer.Elapsed -= timerHandler;
                    timer.Dispose();
                    Connection.Acknoledged -= ackHandler;
                    Connection.Error -= errorHandler;

                    tcs.TrySetException(new DeepStreamException("No RPC Provider found for " + e.Message));
                }
            }

            void timerHandler(object sender, EventArgs e)
            {
                timer.Elapsed -= timerHandler;
                timer.Dispose();
                Connection.Acknoledged -= ackHandler;
                Connection.Error -= errorHandler;
                tcs.TrySetException(new DeepStreamException(Constants.Errors.ACK_TIMEOUT));
            }
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (disposing && Connection != null)
            {
                Connection.PerformRemoteProcedureRequested -= Connection_PerformRemoteProcedureRequested;
            }
        }
    }
}