using System;

namespace DeepStreamNet
{
    internal abstract class ListenerChangedBaseEventArgs : EventArgs
    {
        public string Pattern { get; }

        public string Name { get; }

        public ListenerState ListenerState { get; }

        protected ListenerChangedBaseEventArgs(string pattern, string name, ListenerState state)
        {
            Pattern = pattern;
            Name = name;
            ListenerState = state;
        }
    }

    internal enum ListenerState
    {
        Add = 1,
        Remove = -1
    }
}
