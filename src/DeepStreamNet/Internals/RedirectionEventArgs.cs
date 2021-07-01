namespace DeepStreamNet
{
    internal class RedirectionEventArgs : ChallengeEventArgs
    {
        public string RedirectUrl { get; }

        public RedirectionEventArgs(Topic topic, Action action, string redirectUrl)
            : base(topic, action)
        {
            RedirectUrl = redirectUrl;
        }
    }
}
