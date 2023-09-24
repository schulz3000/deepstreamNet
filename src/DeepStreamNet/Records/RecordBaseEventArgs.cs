using Newtonsoft.Json.Linq;

namespace DeepStreamNet
{
    internal abstract class RecordBaseEventArgs : DeepStreamBaseEventArgs
    {
        public string Identifier { get; }
        public int Version { get; }
        public JToken Data { get; }

        protected RecordBaseEventArgs(Topic topic, Action action, string identifier, int version, JToken data)
            : base(topic, action)
        {
            Identifier = identifier;
            Version = version;
            Data = data;
        }
    }
}