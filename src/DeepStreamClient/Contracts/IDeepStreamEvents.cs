using System;
using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    public interface IDeepStreamEvents
    {
        Task PublishAsync<T>(string eventName, T data);

        Task<IDisposable> Subscribe(string eventName, Action<object> data);

        Task<IDisposable> Listen(string pattern);
    }
}