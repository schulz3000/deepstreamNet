namespace DeepStreamNet.Contracts
{
    /// <summary>
    ///
    /// </summary>
    /// <typeparam name="TResult"></typeparam>
    public interface IRpcResponse<in TResult> : IRpcResponse
    {
        /// <summary>
        ///
        /// </summary>
        /// <param name="result"></param>
        void Send(TResult result);

        /// <summary>
        ///
        /// </summary>>
        void Reject();

        /// <summary>
        ///
        /// </summary>
        /// <param name="message"></param>
        void Error(string message);
    }

    /// <summary>
    ///
    /// </summary>
    public interface IRpcResponse
    {
    }
}
