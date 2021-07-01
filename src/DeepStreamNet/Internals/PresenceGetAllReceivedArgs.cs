using System;

namespace DeepStreamNet
{
    internal class PresenceGetAllReceivedArgs : EventArgs
    {
        private static readonly string[] EmptyUserNames = new string[0];

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