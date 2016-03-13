namespace DeepStreamNet
{
    abstract class DeepStreamBase
    {
        protected Connection Connection { get; }

        protected DeepStreamBase(Connection con)
        {
            Connection = con;
        }

        protected void ThrowIfConnectionNotOpened()
        {
            if (Connection.State != ConnectionState.OPEN)
                throw new DeepStreamException("Login required first");
        }
    }
}