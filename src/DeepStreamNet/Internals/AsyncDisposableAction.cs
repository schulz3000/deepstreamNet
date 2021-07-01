using DeepStreamNet.Contracts;
using System;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    internal class AsyncDisposableAction : IAsyncDisposable
    {
        private readonly Func<Task> _asyncAction;

        public AsyncDisposableAction(Func<Task> asyncAction)
        {
            _asyncAction = asyncAction;
        }

        public Task DisposeAsync() => DisposeAsync(true);

        private Task DisposeAsync(bool disposing)
        {
            if (disposing)
            {
                return _asyncAction();
            }

            return Task.FromResult(0);
        }
    }
}
