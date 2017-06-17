using System.ComponentModel;
using Newtonsoft.Json.Linq;
using System;

namespace DeepStreamNet
{
    class DeepStreamRecordBase<T> : DeepStreamRecordBase, IDeepStreamRecordWrapper, IDisposable
        where T : JContainer
    {
        protected JsonNetChangeListener Listener;

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
            Listener = JsonNetChangeListener.Create(Data);
        }

        public void Patch(string path, JToken item)
        {
            if (path.EndsWith("]", StringComparison.Ordinal) && Data.SelectToken(path) == null)
            {
                var arrayParentPath = path.Substring(0, path.Length - (path.LastIndexOf("[", StringComparison.Ordinal)+1));
                var token = Data.SelectToken(arrayParentPath);
                ((JArray)token).Add(item);
            }
            else
            {
                Data.SelectToken(path)?.Replace(item);
            }
        }

        public void Update(JToken item)
        {
            Data = (T)item;
            Data.AddAnnotation(RecordName);
            //Listener.Resubscribe(Data);
        }

        public object Get(string path) => Data.SelectToken(path);

        public override string ToString() => Data.ToString();

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        void Dispose(bool disposing)
        {
            if (disposing)
            {
                Listener.Dispose();
            }
        }
    }
}