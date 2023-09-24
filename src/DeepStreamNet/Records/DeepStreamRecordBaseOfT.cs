using System;
using System.ComponentModel;
using Newtonsoft.Json.Linq;

namespace DeepStreamNet
{
    internal class DeepStreamRecordBase<T> : DeepStreamRecordBase, IDeepStreamRecordWrapper, IDisposable
        where T : JContainer
    {
        private static readonly JsonMergeSettings JsonMergeSettings = new()
        {
            MergeArrayHandling = MergeArrayHandling.Replace,
            MergeNullValueHandling = MergeNullValueHandling.Merge
        };

        private readonly JsonNetChangeListener _listener;

        public override dynamic this[object key]
        {
            get => Data[key];
            set => Data[key] = value;
        }

        protected T Data;

        public override event PropertyChangingEventHandler PropertyChanging
        {
            add => _listener.PropertyChanging += value;
            remove => _listener.PropertyChanging -= value;
        }

        public override event PropertyChangedEventHandler PropertyChanged
        {
            add => _listener.PropertyChanged += value;
            remove => _listener.PropertyChanged -= value;
        }
        
        public override event EventHandler RecordChanged
        {
            add => _listener.RecordChanged += value;
            remove => _listener.RecordChanged -= value;
        }

        protected DeepStreamRecordBase(string name, int version, T data)
            : base(name, version)
        {
            Data = data;
            Data.AddAnnotation(name);
            _listener = JsonNetChangeListener.Create(Data);
        }

        public void Patch(string path, JToken item)
        {
            if (path.EndsWith("]", StringComparison.Ordinal) && Data.SelectToken(path) == null)
            {
                var arrayParentPath = path.Substring(0, path.Length - (path.LastIndexOf("[", StringComparison.Ordinal) + 1));
                if (path.StartsWith("[", StringComparison.Ordinal) && path.EndsWith("]", StringComparison.Ordinal))
                {
                    arrayParentPath = string.Empty;
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
            Data.Merge(item, JsonMergeSettings);
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

        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                _listener.Dispose();
            }
        }
    }
}