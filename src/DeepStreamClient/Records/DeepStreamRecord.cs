using System.Collections.Generic;
using DeepStreamNet.Records;

namespace DeepStreamNet
{
    class DeepStreamRecord : DeepStreamInnerRecord, IDeepStreamRecordWrapper
    {
        public int RecordVersion
        {
            get;
            private set;
        }

        public ChangeListener Listener { get; private set; }

        public DeepStreamRecord(string name, int version, IDictionary<string, object> obj)
            : base(name,null, obj)
        {
            RecordVersion = version;

            Listener = ChangeListener.Create(this);            
        }

        public void IncrementVersion()
        {
            RecordVersion++;
        }            
    }
}