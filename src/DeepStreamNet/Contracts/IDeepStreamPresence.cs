using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDeepStreamPresence
    {
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        Task<IEnumerable<string>> GetAllAsync();

        /// <summary>
        /// 
        /// </summary>
        /// <param name="listener"></param>
        /// <returns></returns>
        Task<IAsyncDisposable> SubscribeAsync(Action<string, bool> listener);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="listener"></param>
        /// <returns></returns>
        Task<IAsyncDisposable> SubscribeAsync(Func<string, bool,Task> listener);
    }
}
