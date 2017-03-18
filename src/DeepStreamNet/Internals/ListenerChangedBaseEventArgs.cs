using System;

namespace DeepStreamNet
{
    abstract class ListenerChangedBaseEventArgs:EventArgs
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

    enum ListenerState
    {
        Add = 1,
        Remove = -1
    }
}
