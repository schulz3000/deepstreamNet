namespace DeepStreamNet
{
    class RecordListenerResponse : ListenerResponseBase
    {
        public RecordListenerResponse(string pattern, string name, Connection connection)
            : base(pattern, name, connection)
        {
        }

        protected override Topic ListenerTopic
        {
            get { return Topic.RECORD; }
        }
    }
}
