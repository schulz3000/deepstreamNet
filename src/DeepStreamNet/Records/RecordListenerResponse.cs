﻿namespace DeepStreamNet
{
    internal class RecordListenerResponse : ListenerResponseBase
    {
        public RecordListenerResponse(string pattern, string name, Connection connection)
            : base(pattern, name, connection)
        {
        }

        protected override Topic ListenerTopic => Topic.RECORD;
    }
}
