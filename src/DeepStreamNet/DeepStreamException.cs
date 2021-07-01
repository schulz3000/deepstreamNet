using System;
#if !COREFX
using System.Runtime.Serialization;
#endif

namespace DeepStreamNet
{
    /// <summary>
    /// DeepStreamException
    /// </summary>
    [Serializable]
    public class DeepStreamException : Exception
    {
        /// <summary>
        ///
        /// </summary>
        public DeepStreamException()
        {
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="message">Exceptionmessage</param>
        public DeepStreamException(string message)
            : base(message)
        {
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="message"></param>
        /// <param name="innerException"></param>
        public DeepStreamException(string message, Exception innerException)
        : base(message, innerException)
        {
        }

        /// <summary>
        ///
        /// </summary>
        /// <param name="error"></param>
        /// <param name="message"></param>
        public DeepStreamException(string error, string message)
            : base(error+" - "+message)
        {
        }

#if !COREFX
        /// <summary>
        ///
        /// </summary>
        /// <param name="info"></param>
        /// <param name="context"></param>
        protected DeepStreamException(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
        }
#endif
    }
}