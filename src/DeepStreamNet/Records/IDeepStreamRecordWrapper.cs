using DeepStreamNet.Contracts;
using Newtonsoft.Json.Linq;

namespace DeepStreamNet
{
    interface IDeepStreamRecordWrapper:IDeepStreamRecord
    {
        int RecordVersion { get; }

        object Get(string path);

        void Patch(string path, JToken item);

        void Update(JToken item);

        void IncrementVersion();
    }
}