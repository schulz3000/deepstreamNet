﻿using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    internal abstract class ListenerResponseBase : IListenerResponse
    {
        private readonly string Pattern;
        private readonly string Name;
        private readonly Connection Connection;

        protected abstract Topic ListenerTopic { get; }

        protected ListenerResponseBase(string pattern, string name, Connection connection)
        {
            Pattern = pattern;
            Name = name;
            Connection = connection;
        }

        public void Accept() => Connection.Send(Utils.BuildCommand(ListenerTopic, Action.LISTEN_ACCEPT, Pattern, Name));

        public void Reject() => Connection.Send(Utils.BuildCommand(ListenerTopic, Action.LISTEN_REJECT, Pattern, Name));
    }
}
