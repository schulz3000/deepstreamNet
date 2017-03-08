using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    class RpcResponse<TResult> : IRpcResponse<TResult>
    {
        readonly Connection Connection;
        readonly string ProcedureName;
        readonly string Uid;

        public RpcResponse(string procedureName, string uid, Connection connection)
        {
            ProcedureName = procedureName;
            Uid = uid;
            Connection = connection;
        }

        public void Error(string message)
        {
            var exceptionCommand = Utils.BuildCommand(Topic.RPC, Action.ERROR, message, ProcedureName, Uid);
             Connection.Send(exceptionCommand);
        }

        public void Reject()
        {
            var rejectCommand = Utils.BuildCommand(Topic.RPC, Action.REJECTION, ProcedureName, Uid);
            Connection.Send(rejectCommand);
        }

        public void Send(TResult result)
        {
            var resultCommand = Utils.BuildCommand(Topic.RPC, Action.RESPONSE, ProcedureName, Uid, Utils.ConvertAndPrefixData(result));
            Connection.Send(resultCommand);
        }
    }
}