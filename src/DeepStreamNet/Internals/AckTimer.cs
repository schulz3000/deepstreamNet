using System;
using System.Threading;

namespace DeepStreamNet
{
    class AckTimer : IDisposable
    {
        readonly Timer timer;
        readonly int timeout;

        public event EventHandler Elapsed;

        public AckTimer(int interval)
        {
            timeout = interval;
            timer = new Timer(OnElapsed, null, Timeout.Infinite, Timeout.Infinite);
        }

        public void Start()
        {
            timer.Change(timeout, Timeout.Infinite);
        }

        void OnElapsed(object state)
        {
            Elapsed?.Invoke(this, EventArgs.Empty);
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        void Dispose(bool disposing)
        {
            if(disposing)
            {
                timer.Dispose();
            }
        }
    }
}
