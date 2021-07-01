namespace DeepStreamNet
{
    internal class HasRecordArgs : DeepStreamBaseEventArgs
    {
        public string Name { get; }
        public bool Result { get; }

        public HasRecordArgs(Topic topic, Action action, string name, bool result)
            : base(topic, action)
        {
            Name = name;
            Result = result;
        }
    }
}
