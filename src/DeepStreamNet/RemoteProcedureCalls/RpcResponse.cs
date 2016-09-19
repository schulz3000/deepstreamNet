using DeepStreamNet.Contracts;
using System.Threading.Tasks;

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

        public async Task Error(string message)
        {
            var exceptionCommand = Utils.BuildCommand(Topic.RPC, Action.ERROR, message, ProcedureName, Uid);
            await Connection.SendAsync(exceptionCommand).ConfigureAwait(false);
        }

        public async Task Reject()
        {
            var rejectCommand = Utils.BuildCommand(Topic.RPC, Action.REJECTION, ProcedureName, Uid);
            await Connection.SendAsync(rejectCommand).ConfigureAwait(false);
        }

        public async Task Send(TResult result)
        {
            var resultCommand = Utils.BuildCommand(Topic.RPC, Action.RESPONSE, ProcedureName, Uid, Utils.ConvertAndPrefixData(result));
            await Connection.SendAsync(resultCommand).ConfigureAwait(false);
        }
    }
}