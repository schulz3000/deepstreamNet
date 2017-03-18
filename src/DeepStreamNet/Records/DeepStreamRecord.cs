//using System.Collections.Generic;
//using System.ComponentModel;
//using System.Dynamic;
//using System.Linq;
//using DeepStreamNet.Contracts;
//using Newtonsoft.Json.Linq;
//using System.Collections;

//namespace DeepStreamNet
//{
//    class DeepStreamRecord : DynamicObject, IDeepStreamRecord, IDeepStreamRecordWrapper, INotifyPropertyChanging, INotifyPropertyChanged, IEnumerable<object>
//    {
//        JToken data;

//        public event PropertyChangingEventHandler PropertyChanging;
//        public event PropertyChangedEventHandler PropertyChanged
//        {
//            add { Listener.PropertyChanged += value; }
//            remove { Listener.PropertyChanged -= value; }
//        }

//        public string RecordName { get; }

//        public dynamic this[object key]
//        {
//            get { return GetPropertyValue(key); }
//            set { SetPropertyValue(key, value); }
//        }

//        protected ChangeListener Listener { get; }

//        public int RecordVersion { get; private set; }

//        public DeepStreamRecord(string name, int version, JToken obj)
//        {
//            RecordVersion = version;

//            RecordName = name;

//            data = obj;

//            data.AddAnnotation(name);

//            Listener = ChangeListener.Create((JContainer)data);
//        }

//        public void IncrementVersion()
//        {
//            RecordVersion++;
//        }

//        protected JToken GetPropertyValue(object key)
//        {
//            return data[key];
//        }

//        protected bool SetPropertyValue(object key, JToken value)
//        {
//            if (data[key] == null && data.Type == JTokenType.Object)
//            {
//                ((JObject)data).Add(key.ToString(), value);
//            }
//            else if (data[key] == null && data.Type == JTokenType.Array)
//            {
//                ((JArray)data).Add(value);
//            }
//            else
//            {
//                PropertyChanging?.Invoke(this, new PropertyChangingEventArgs(key.ToString()));
//                data[key] = value;
//                // PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
//            }

//            return true;
//        }

//        public void Patch(string path, JToken item)
//        {
//            data.SelectToken(path)?.Replace(item);
//        }

//        public void Update(JToken item)
//        {
//            data = item;
//        }

//        public object Get(string path) => data.SelectToken(path);

//        public override bool TryGetMember(GetMemberBinder binder, out object result)
//        {
//            if (data.Type == JTokenType.Array)
//            {
//                result = null;
//                return true;
//            }

//            result = GetPropertyValue(binder.Name);
//            if (result != null)
//                return true;

//            return false;
//        }

//        public override bool TrySetMember(SetMemberBinder binder, object value)
//        {
//            return SetPropertyValue(binder.Name, JToken.FromObject(value));
//        }

//        public override bool TryConvert(ConvertBinder binder, out object result)
//        {
//            if (data.Type == JTokenType.Array)
//            {
//                result = data.ToArray();
//                return true;
//            }
//            return base.TryConvert(binder, out result);
//        }

//        public override IEnumerable<string> GetDynamicMemberNames()
//        {
//            if (data.Type == JTokenType.Object)
//                return ((JObject)data).Properties().Select(s => s.Name);
//            else
//                return base.GetDynamicMemberNames();
//        }

//        public override string ToString() => data.ToString();

//        public IEnumerator<object> GetEnumerator()
//        {
//            if (data.Type == JTokenType.Array)
//            {
//                return ((JArray)data).Cast<object>().GetEnumerator();
//            }
//            return null;
//        }

//        IEnumerator IEnumerable.GetEnumerator()
//        {
//            return GetEnumerator();
//        }
//    }
//}