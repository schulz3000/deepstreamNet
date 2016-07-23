using System;
using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDeepStreamRemoteProcedureCalls
    {
        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="TInput"></typeparam>
        /// <typeparam name="TResult"></typeparam>
        /// <param name="procedureName"></param>
        /// <param name="procedure"></param>
        /// <returns></returns>
        Task<IAsyncDisposable> RegisterProvider<TInput, TResult>(string procedureName, Func<TInput, TResult> procedure);

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="TInput"></typeparam>
        /// <typeparam name="TResult"></typeparam>
        /// <param name="procedureName"></param>
        /// <param name="parameter"></param>
        /// <returns></returns>
        Task<TResult> MakeRequest<TInput, TResult>(string procedureName, TInput parameter);
    }
}