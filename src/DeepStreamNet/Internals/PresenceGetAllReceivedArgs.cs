using System;

namespace DeepStreamNet
{
    class PresenceGetAllReceivedArgs : EventArgs
    {
        static readonly string[] EmptyUserNames = new string[0];

        public string[] Usernames { get; }

        public PresenceGetAllReceivedArgs()
        {
            Usernames = EmptyUserNames;
        }

        public PresenceGetAllReceivedArgs(string[] usernames)
        {
            Usernames = usernames;
        }
    }
}