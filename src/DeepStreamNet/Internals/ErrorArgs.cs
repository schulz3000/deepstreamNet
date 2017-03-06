namespace DeepStreamNet
{
    class ErrorArgs : DeepStreamBaseEventArgs
    {
        public string Error { get; }
        public string Message { get; }

        public ErrorArgs(Topic topic, Action action, string error, string message)
            : base(topic, action)
        {
            Error = error;
            Message = message;
        }
    }
}