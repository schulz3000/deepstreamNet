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

        public Task DisposeAsync()
        {
            return DisposeAsync(true);
        }

        async Task DisposeAsync(bool disposing)
        {
            if (disposing)
                await _asyncAction().ConfigureAwait(false);
        }
    }
}
