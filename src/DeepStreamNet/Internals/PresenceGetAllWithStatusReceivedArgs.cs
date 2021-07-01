using System;
using System.Collections.Generic;

namespace DeepStreamNet
{
    internal class PresenceGetAllWithStatusReceivedArgs : EventArgs
    {
        private static readonly Dictionary<string, bool> EmptyUserNamesWithStatus = new();

        public Dictionary<string, bool> UsernamesWithStatus { get; }

        public PresenceGetAllWithStatusReceivedArgs()
        {
            UsernamesWithStatus = EmptyUserNamesWithStatus;
        }

        public PresenceGetAllWithStatusReceivedArgs(Dictionary<string, bool> usernamesWithStatus)
        {
            UsernamesWithStatus = usernamesWithStatus;
        }
    }
}
