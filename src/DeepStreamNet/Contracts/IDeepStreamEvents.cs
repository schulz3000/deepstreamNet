using System;
using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDeepStreamEvents
    {
        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="eventName"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        void Publish<T>(string eventName, T data);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="eventName"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        Task<IAsyncDisposable> SubscribeAsync(string eventName, Action<object> data);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="pattern"></param>
        /// <param name="listener"></param>
        /// <returns></returns>
        Task<IAsyncDisposable> ListenAsync(string pattern, Action<string, bool, IListenerResponse> listener);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="pattern"></param>
        /// <param name="listener"></param>
        /// <returns></returns>
        Task<IAsyncDisposable> ListenAsync(string pattern, Func<string, bool, IListenerResponse, Task> listener);
    }
}