namespace DeepStreamNet
{
    internal class EventListenerChangedEventArgs : ListenerChangedBaseEventArgs
    {
        public EventListenerChangedEventArgs(string pattern, string name, ListenerState state)
            : base(pattern, name, state)
        {
        }
    }
}