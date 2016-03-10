namespace DeepStreamNet
{
    class RecordReceivedArgs : RecordBaseEventArgs
    {
        public RecordReceivedArgs(Topic topic, Action action, string identifier, int version, object data)
            : base(topic, action, identifier, version, data)
        {
        }
    }
}