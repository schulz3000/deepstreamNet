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

        public void Error(string message) => Connection.Send(Utils.BuildCommand(Topic.RPC, Action.ERROR, message, ProcedureName, Uid));

        public void Reject() => Connection.Send(Utils.BuildCommand(Topic.RPC, Action.REJECTION, ProcedureName, Uid));

        public void Send(TResult result) => Connection.Send(Utils.BuildCommand(Topic.RPC, Action.RESPONSE, ProcedureName, Uid, Utils.ConvertAndPrefixData(result)));
    }
}