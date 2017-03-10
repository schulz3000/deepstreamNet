using System;

namespace DeepStreamNet
{
    class EventListenerChangedArgs : EventArgs
    {
        public string Pattern { get; }

        public string Name { get; }

        public EventListenerState EventListenerState { get; }


        public EventListenerChangedArgs(string pattern, string name, EventListenerState state)
        {
            Pattern = pattern;
            Name = name;
            EventListenerState = state;
        }
    }

    enum EventListenerState
    {
        Add = 1,
        Remove = -1
    }
}