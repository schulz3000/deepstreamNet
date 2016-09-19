using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    /// 
    /// </summary>
    /// <typeparam name="TResult"></typeparam>
    public interface IRpcResponse<TResult>
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="result"></param>
        /// <returns></returns>
        Task Send(TResult result);
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        Task Reject();
        /// <summary>
        /// 
        /// </summary>
        /// <param name="message"></param>
        /// <returns></returns>
        Task Error(string message);
    }
}
