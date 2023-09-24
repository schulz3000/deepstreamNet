using System;

namespace DeepStreamNet
{
    internal abstract class DeepStreamBaseEventArgs : EventArgs
    {
        public Topic Topic { get; }

        public Action Action { get; }

        protected DeepStreamBaseEventArgs(Topic topic, Action action)
        {
            Topic = topic;
            Action = action;
        }
    }
}