namespace DeepStreamNet
{
    class RecordPatchedArgs : RecordBaseEventArgs
    {
        public string Property { get; }

        public RecordPatchedArgs(Topic topic, Action action, string identifier, int version, string property, object data)
            : base(topic, action, identifier, version, data)
        {
            Property = property;
        }
    }
}