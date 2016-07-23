namespace DeepStreamNet
{
    class RecordUpdatedArgs : RecordBaseEventArgs
    {
        public RecordUpdatedArgs(Topic topic, Action action, string identifier, int version, object data)
            : base(topic, action, identifier, version, data)
        {
        }
    }
}