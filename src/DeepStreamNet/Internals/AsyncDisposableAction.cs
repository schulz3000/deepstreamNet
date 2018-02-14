using DeepStreamNet.Contracts;
using System;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    class AsyncDisposableAction : IAsyncDisposable
    {
        readonly Func<Task> _asyncAction;

        public AsyncDisposableAction(Func<Task> asyncAction)
        {
            _asyncAction = asyncAction;
        }

        public Task DisposeAsync() => DisposeAsync(true);

        Task DisposeAsync(bool disposing)
        {
            if (disposing)
                return _asyncAction();

            return Task.FromResult(0);
        }
    }
}
