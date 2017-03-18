using System.ComponentModel;
using Newtonsoft.Json.Linq;
using System.Collections.Specialized;

namespace DeepStreamNet
{
    class DeepStreamRecordBase<T> : DeepStreamRecordBase, IDeepStreamRecordWrapper
        where T : JContainer
    {
        protected ChangeListener Listener;

        public override dynamic this[object key]
        {
            get => Data[key];
            set => Data[key] = value;
        }

        protected T Data;

        public override event PropertyChangedEventHandler PropertyChanged
        {
            add { Listener.PropertyChanged += value; }
            remove { Listener.PropertyChanged -= value; }
        }

        protected DeepStreamRecordBase(string name, int version, T data)
            : base(name, version)
        {
            Data = data;
            Data.AddAnnotation(name);
            Listener = ChangeListener.Create(Data);
        }

        public void Patch(string path, JToken item) => Data.SelectToken(path)?.Replace(item);

        public void Update(JToken item)
        {
            Data = (T)item;
            Data.AddAnnotation(RecordName);
            Listener.Resubscribe((INotifyCollectionChanged)Data);
        }

        public object Get(string path) => Data.SelectToken(path);

        public override string ToString() => Data.ToString();
    }
}