namespace DeepStreamNet
{
    internal class RpcAcknoledgedArgs : AcknoledgedArgs
    {
        public Action RpcAction { get; }

        public RpcAcknoledgedArgs(Topic topic, Action action, Action rpcAction, string identifier)
            : base(topic, action, identifier)
        {
            RpcAction = rpcAction;
        }
    }
}
