using System;
using System.Threading;

namespace DeepStreamNet
{
    internal class AckTimer : IDisposable
    {
        private readonly Timer timer;
        private readonly int timeout;

        public event EventHandler Elapsed;

        public AckTimer(int interval)
        {
            timeout = interval;
            timer = new Timer(OnElapsed, null, Timeout.Infinite, Timeout.Infinite);
        }

        public void Start() => timer.Change(timeout, Timeout.Infinite);

        private void OnElapsed(object state) => Elapsed?.Invoke(this, EventArgs.Empty);

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                timer.Dispose();
            }
        }
    }
}
