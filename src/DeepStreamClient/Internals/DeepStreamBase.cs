namespace DeepStreamNet
{
    abstract class DeepStreamBase
    {
        protected Connection Connection { get; }

        protected DeepStreamOptions Options { get; }

        protected DeepStreamBase(Connection connection, DeepStreamOptions options)
        {
            Connection = connection;
            Options = options;
        }

        protected void ThrowIfConnectionNotOpened()
        {
            if (Connection.State != ConnectionState.OPEN)
                throw new DeepStreamException("Login required first");
        }
    }
}