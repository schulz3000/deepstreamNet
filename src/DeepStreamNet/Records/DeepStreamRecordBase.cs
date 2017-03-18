using DeepStreamNet.Contracts;
using Newtonsoft.Json.Linq;
using System.ComponentModel;
using System.Dynamic;

namespace DeepStreamNet
{
    abstract class DeepStreamRecordBase : DynamicObject, IDeepStreamRecord
    {
        public int RecordVersion { get; private set; }

        protected DeepStreamRecordBase(string name, int version)
        {
            RecordName = name;
            RecordVersion = version;
        }

        public abstract dynamic this[object key] { get; set; }

        public string RecordName { get; }

        public abstract event PropertyChangedEventHandler PropertyChanged;

        public void IncrementVersion() => RecordVersion++;
    }
}
