using System;

namespace DeepStreamNet
{
    class EventListenerChangedArgs : EventArgs
    {
        public string Pattern { get; }

        public EventListenerState EventListenerState { get; }

        public EventListenerChangedArgs(string pattern, EventListenerState state)
        {
            Pattern = pattern;
            EventListenerState = state;
        }
    }

    enum EventListenerState
    {
        Add = 1,
        Remove = -1
    }
}