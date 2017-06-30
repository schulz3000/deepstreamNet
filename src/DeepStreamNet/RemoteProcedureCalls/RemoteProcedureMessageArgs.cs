using Newtonsoft.Json.Linq;
using System;

namespace DeepStreamNet
{
    class RemoteProcedureMessageArgs : DeepStreamBaseEventArgs
    {
        public string Uid { get; }
        public string Identifier { get; }
        public JToken Data { get; }
        public Type DataType { get; }

        public RemoteProcedureMessageArgs(Topic topic, Action action, string identifier, string uid, Type dataType, JToken data)
            : base(topic, action)
        {
            Identifier = identifier;
            Uid = uid;
            DataType = dataType;
            Data = data;
        }
    }
}