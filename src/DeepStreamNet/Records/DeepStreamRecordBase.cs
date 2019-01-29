using System;
using System.ComponentModel;
using System.Dynamic;
using DeepStreamNet.Contracts;

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

        public abstract event PropertyChangingEventHandler PropertyChanging;

        public abstract event PropertyChangedEventHandler PropertyChanged;

        public abstract event EventHandler RecordChanged;

        public void IncrementVersion() => RecordVersion++;
    }
}
