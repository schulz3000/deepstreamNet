namespace DeepStreamNet
{
    abstract class DeepStreamBase
    {
        protected Connection Connection { get; }

        protected DeepStreamBase(Connection con)
        {
            Connection = con;
        }

        protected void ThrowIfNotLoggedIn()
        {
            if (!Connection.IsLoggedIn)
                throw new DeepStreamException("Login required first");
        }
    }
}
