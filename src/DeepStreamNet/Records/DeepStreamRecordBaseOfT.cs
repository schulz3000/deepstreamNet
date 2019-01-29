using System;
using System.ComponentModel;
using Newtonsoft.Json.Linq;

namespace DeepStreamNet
{
    class DeepStreamRecordBase<T> : DeepStreamRecordBase, IDeepStreamRecordWrapper, IDisposable
        where T : JContainer
    {
        static readonly JsonMergeSettings jsonMergeSettings = new JsonMergeSettings
        {
            MergeArrayHandling = MergeArrayHandling.Replace,
            MergeNullValueHandling = MergeNullValueHandling.Merge
        };

        protected JsonNetChangeListener Listener;

        public override dynamic this[object key]
        {
            get => Data[key];
            set => Data[key] = value;
        }

        protected T Data;

        public override event PropertyChangingEventHandler PropertyChanging
        {
            add { Listener.PropertyChanging += value; }
            remove { Listener.PropertyChanging -= value; }
        }

        public override event PropertyChangedEventHandler PropertyChanged
        {
            add { Listener.PropertyChanged += value; }
            remove { Listener.PropertyChanged -= value; }
        }

        public override event EventHandler RecordChanged;

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
                var arrayParentPath = path.Substring(0, path.Length - (path.LastIndexOf("[", StringComparison.Ordinal) + 1));
                if (path.StartsWith("[", StringComparison.Ordinal) && path.EndsWith("]", StringComparison.Ordinal))
                {
                    arrayParentPath = String.Empty;
                }
                var token = Data.SelectToken(arrayParentPath);
                ((JArray)token).Add(item);
            }
            else
            {
                if (!Data.HasValues)
                {
                    Data[path]=item;
                }
                else
                {
                    Data.SelectToken(path)?.Replace(item);
                }
            }
        }

        public void Update(JToken item)
        {
            Data.Merge(item, jsonMergeSettings);
            RecordChanged.Invoke(this, EventArgs.Empty);
            Data.AddAnnotation(RecordName);
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