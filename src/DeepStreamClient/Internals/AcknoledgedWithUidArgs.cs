namespace DeepStreamNet
{
    class AcknoledgedWithUidArgs : AcknoledgedArgs
    {
        public string Uid { get; }

        public AcknoledgedWithUidArgs(Topic topic, Action action, string identifier, string uid)
            : base(topic, action, identifier)
        {
            Uid = uid;
        }
    }
}