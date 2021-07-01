using System;

namespace DeepStreamNet
{
    internal class PresenceListenerChangedEventArgs: EventArgs
    {
        public string Username { get; }
        public bool IsLoggedIn { get; }

        public PresenceListenerChangedEventArgs(string username,bool isLoggedIn)
        {
            Username = username;
            IsLoggedIn = isLoggedIn;
        }
    }
}
