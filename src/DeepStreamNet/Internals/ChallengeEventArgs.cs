﻿namespace DeepStreamNet
{
    internal class ChallengeEventArgs : DeepStreamBaseEventArgs
    {
        public ChallengeEventArgs(Topic topic, Action action)
            : base(topic, action)
        {
        }
    }
}
