using System;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    class AsyncDisposableAction:IAsyncDisposable
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
               await _asyncAction();
        }
    }
}
