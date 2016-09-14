using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    public interface IRpcResponse<TResult>
    {
        Task Send(TResult result);
        Task Reject();
        Task Error(string message);
    }
}
