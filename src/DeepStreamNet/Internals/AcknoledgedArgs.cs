namespace DeepStreamNet
{
    class AcknoledgedArgs : DeepStreamBaseEventArgs
    {
        public string Identifier { get; }

        public AcknoledgedArgs(Topic topic, Action action, string identifier)
            : base(topic, action)
        {
            Identifier = identifier;
        }
    }
}