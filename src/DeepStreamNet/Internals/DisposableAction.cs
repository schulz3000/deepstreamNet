using System;

namespace DeepStreamNet
{
    internal class DisposableAction : IDisposable
    {
        private readonly System.Action _action;

        public DisposableAction(System.Action action)
        {
            _action = action;
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                _action();
            }
        }
    }
}