namespace DeepStreamNet
{
    class EventListenerResponse : ListenerResponseBase
    {
        public EventListenerResponse(string pattern, string name, Connection connection)
            : base(pattern, name, connection)
        {
        }

        protected override Topic ListenerTopic => Topic.EVENT;
    }
}
