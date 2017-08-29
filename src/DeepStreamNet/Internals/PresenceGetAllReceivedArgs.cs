using System;

namespace DeepStreamNet
{
    class PresenceGetAllReceivedArgs : EventArgs
    {
        readonly static string[] emptyUserNames = new string[0];

        public string[] Usernames { get; }

        public PresenceGetAllReceivedArgs()
        {
            Usernames = emptyUserNames;
        }

        public PresenceGetAllReceivedArgs(string[] usernames)
        {
            Usernames = usernames;
        }
    }
}