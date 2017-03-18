namespace DeepStreamNet
{
    class RecordListenerChangedEventArgs : ListenerChangedBaseEventArgs
    {
        public RecordListenerChangedEventArgs(string pattern, string name, ListenerState state)
            : base(pattern, name, state)
        {
        }
    }
}
