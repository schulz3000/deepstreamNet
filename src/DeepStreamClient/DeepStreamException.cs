using System;

namespace DeepStreamNet
{
    [Serializable]
    public class DeepStreamException : Exception
    {
        public DeepStreamException(string message)
            : base(message)
        {
        }
    }
}
