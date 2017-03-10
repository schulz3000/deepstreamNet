using System;
using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    class EventListenerResponse : IEventListenerResponse
    {
        readonly string Pattern;
        readonly string Name;
        readonly Connection Connection;
        public EventListenerResponse(string pattern, string name, Connection connection)
        {
            Pattern = pattern;
            Name = name;
            Connection = connection;
        }

        public void Accept()
        {
            Connection.Send(Utils.BuildCommand(Topic.EVENT, Action.LISTEN_ACCEPT, Pattern, Name));
        }

        public void Reject()
        {
            Connection.Send(Utils.BuildCommand(Topic.EVENT, Action.LISTEN_REJECT, Pattern, Name));
        }
    }
}
