using System;
using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    public interface IDeepStreamRemoteProcedureCalls
    {
        Task<IDisposable> RegisterProvider<TInput, TResult>(string procedureName, Func<TInput, TResult> procedure);

        Task<TResult> MakeRequest<TInput, TResult>(string procedureName, TInput parameter);
    }
}