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
        Task<IAsyncDisposable> RegisterProviderAsync<TInput, TResult>(string procedureName, Func<TInput, IRpcResponse<TResult>, Task> procedure);

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="TInput"></typeparam>
        /// <typeparam name="TResult"></typeparam>
        /// <param name="procedureName"></param>
        /// <param name="procedure"></param>
        /// <returns></returns>
        Task<IAsyncDisposable> RegisterProviderAsync<TInput, TResult>(string procedureName, Action<TInput, IRpcResponse<TResult>> procedure);

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="TInput"></typeparam>
        /// <typeparam name="TResult"></typeparam>
        /// <param name="procedureName"></param>
        /// <param name="parameter"></param>
        /// <returns></returns>
        Task<TResult> MakeRequestAsync<TInput, TResult>(string procedureName, TInput parameter);        
    }
}