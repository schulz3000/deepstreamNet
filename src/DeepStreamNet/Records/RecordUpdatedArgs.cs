using Newtonsoft.Json.Linq;

namespace DeepStreamNet
{
    class RecordUpdatedArgs : RecordBaseEventArgs
    {
        public RecordUpdatedArgs(Topic topic, Action action, string identifier, int version, JToken data)
            : base(topic, action, identifier, version, data)
        {
        }
    }
}