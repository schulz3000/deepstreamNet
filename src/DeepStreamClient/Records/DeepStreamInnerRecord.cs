using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Dynamic;
using System.Linq;
using DeepStreamNet.Contracts;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace DeepStreamNet
{
    class DeepStreamInnerRecord : DynamicObject, IDeepStreamRecord, INotifyPropertyChanging, INotifyPropertyChanged
    {
        readonly HashSet<IRecordPropertyWrapper> properties = new HashSet<IRecordPropertyWrapper>();

        public event PropertyChangingEventHandler PropertyChanging;

        public event PropertyChangedEventHandler PropertyChanged;

        public string RecordName
        {
            get;
        }

        public string Path
        {
            get;
        }

        public object this[string name]
        {
            get { return GetPropertyValue(name); }
            set { SetPropertyValue(name, value); }
        }

        public DeepStreamInnerRecord(string name, string path, IDictionary<string, object> obj)
        {
            RecordName = name;

            Path = string.IsNullOrWhiteSpace(path) ? name : path + "." + name;

            FillProperties(obj);
        }

        void FillProperties(IDictionary<string, object> obj)
        {
            foreach (var item in obj)
            {
                if (item.Value is JArray)
                {
                    var arr = item.Value as JArray;
                    var list = new DeepStreamRecordCollection<object>();

                    for (int i = 0; i < arr.Count; i++)
                    {
                        if (arr[i] is JObject)
                        {
                            var innerItems = arr[i].ToObject<Dictionary<string, object>>();

                            list.Add(new DeepStreamInnerRecord(item.Key + "." + i, Path, innerItems));
                        }
                        else
                        {
                            list.Add(arr[i]);
                        }
                    }

                    properties.Add(new RecordPropertyWrapper(item.Key, list));

                }
                else {
                    object insert = item.Value;

                    if (item.Value is IDictionary<string, object>)
                        insert = new DeepStreamInnerRecord(item.Key, Path, item.Value as IDictionary<string, object>);
                    else if (item.Value is JObject)
                        insert = new DeepStreamInnerRecord(item.Key, Path, (item.Value as JObject).ToObject<Dictionary<string, object>>());

                    properties.Add(new RecordPropertyWrapper(item.Key, insert));
                }
            }
        }

        protected void Merge(IDictionary<string, object> obj)
        {
            foreach (var item in obj)
            {
                if (this[item.Key] != null)
                {
                    if (this[item.Key] is DeepStreamInnerRecord)
                        (this[item.Key] as DeepStreamInnerRecord).Merge(item.Value as Dictionary<string, object>);
                    else
                        this[item.Key] = item.Value;
                }
                else {

                    if (item.Value is JArray)
                    {
                        var arr = item.Value as JArray;
                        var list = new DeepStreamRecordCollection<object>();

                        for (int i = 0; i < arr.Count; i++)
                        {
                            if (arr[i] is JObject)
                            {
                                var innerItems = arr[i].ToObject<Dictionary<string, object>>();

                                list.Add(new DeepStreamInnerRecord(item.Key + "." + i, Path, innerItems));
                            }
                            else
                            {
                                list.Add(arr[i]);
                            }
                        }

                        properties.Add(new RecordPropertyWrapper(item.Key, list));

                    }
                    else {
                        object insert = item.Value;

                        if (item.Value is IDictionary<string, object>)
                            insert = new DeepStreamInnerRecord(item.Key, Path, item.Value as IDictionary<string, object>);
                        else if (item.Value is JObject)
                            insert = new DeepStreamInnerRecord(item.Key, Path, (item.Value as JObject).ToObject<Dictionary<string, object>>());

                        properties.Add(new RecordPropertyWrapper(item.Key, insert));
                    }
                }
            }
        }

        protected object GetPropertyValue(string name)
        {
            return properties.Where(w => w.Name == name).Select(s => s.Value).FirstOrDefault();
        }

        protected bool SetPropertyValue(string name, object value)
        {
            var property = properties.FirstOrDefault(w => w.Name == name);
            if (property == null)
            {
                properties.Add(new RecordPropertyWrapper(name, value));
            }
            else {
                if (property.Value != value)
                {
                    PropertyChanging?.Invoke(this, new PropertyChangingEventArgs(name));
                    property.Value = value;
                    PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
                }
            }

            return true;
        }

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = GetPropertyValue(binder.Name);
            if (result != null)
                return true;

            return false;
        }

        public override bool TrySetMember(SetMemberBinder binder, object value)
        {
            return SetPropertyValue(binder.Name, value);
        }

        public override IEnumerable<string> GetDynamicMemberNames()
        {
            return properties.Select(s => s.Name);
        }

        public override string ToString()
        {
            return JsonConvert.SerializeObject(this);
        }
    }
}