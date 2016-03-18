using System;

namespace DeepStreamNet
{
    /// <summary>
    /// DeepStreamException
    /// </summary>
    public class DeepStreamException : Exception
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="message">Exceptionmessage</param>
        public DeepStreamException(string message)
            : base(message)
        {
        }
    }
}