using System;
using System.Collections.Generic;

namespace DeepStreamNet
{
    class PresenceGetAllWithStatusReceivedArgs : EventArgs
    {
        readonly static Dictionary<string, bool> emptyUserNamesWithStatus = new Dictionary<string, bool>();

        public Dictionary<string, bool> UsernamesWithStatus { get; }

        public PresenceGetAllWithStatusReceivedArgs()
        {
            UsernamesWithStatus = emptyUserNamesWithStatus;
        }

        public PresenceGetAllWithStatusReceivedArgs(Dictionary<string, bool> usernamesWithStatus)
        {
            UsernamesWithStatus = usernamesWithStatus;
        }
    }
}
