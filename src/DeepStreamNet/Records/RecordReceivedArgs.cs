using Newtonsoft.Json.Linq;

namespace DeepStreamNet
{
    internal class RecordReceivedArgs : RecordBaseEventArgs
    {
        public RecordReceivedArgs(Topic topic, Action action, string identifier, int version, JToken data)
            : base(topic, action, identifier, version, data)
        {
        }
    }
}