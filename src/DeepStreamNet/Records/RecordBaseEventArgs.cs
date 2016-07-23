namespace DeepStreamNet
{
    abstract class RecordBaseEventArgs : DeepStreamBaseEventArgs
    {
        public string Identifier { get; }
        public int Version { get; }
        public object Data { get; }

        protected RecordBaseEventArgs(Topic topic, Action action, string identifier, int version, object data)
            : base(topic, action)
        {
            Identifier = identifier;
            Version = version;
            Data = data;
        }
    }
}