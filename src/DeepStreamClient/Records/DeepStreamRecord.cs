using System.Collections.Generic;

namespace DeepStreamNet
{
    class DeepStreamRecord : DeepStreamInnerRecord, IDeepStreamRecordWrapper
    {
        public int RecordVersion
        {
            get;
            private set;
        }

        public DeepStreamRecord(string name, int version, IDictionary<string, object> obj)
            : base(name, obj)
        {
            RecordVersion = version;
        }

        public void UpdatePartial(int newVersion, string updatePath, object value)
        {
            RecordVersion = newVersion;

            UpdatePartial(updatePath, value);
        }

        public void Merge(int newVersion, IDictionary<string, object> obj)
        {
            RecordVersion = newVersion;

            Merge(obj);
        }
    }
}