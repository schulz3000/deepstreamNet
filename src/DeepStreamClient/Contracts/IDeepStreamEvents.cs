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
        Task PublishAsync<T>(string eventName, T data);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="eventName"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        Task<IDisposable> Subscribe(string eventName, Action<object> data);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="pattern"></param>
        /// <returns></returns>
        Task<IDisposable> Listen(string pattern);
    }
}