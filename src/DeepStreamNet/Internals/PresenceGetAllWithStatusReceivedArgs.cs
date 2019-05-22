using System;
using System.Collections.Generic;

namespace DeepStreamNet
{
    class PresenceGetAllWithStatusReceivedArgs : EventArgs
    {
        static readonly Dictionary<string, bool> EmptyUserNamesWithStatus = new Dictionary<string, bool>();

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
