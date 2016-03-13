using System.Collections.Generic;
using System.ComponentModel;
using System.Dynamic;
using System.Linq;

namespace DeepStreamNet
{
    class DeepStreamInnerRecord : DynamicObject, INotifyPropertyChanging, INotifyPropertyChanged
    {
        readonly HashSet<IRecordPropertyWrapper> properties = new HashSet<IRecordPropertyWrapper>();

        public event PropertyChangingEventHandler PropertyChanging;

        public event PropertyChangedEventHandler PropertyChanged;

        public string RecordName
        {
            get;
        }

        public object this[string name]
        {
            get { return GetPropertyValue(name); }
            set { SetPropertyValue(name, value); }
        }

        public DeepStreamInnerRecord(string name, IDictionary<string, object> obj)
        {
            RecordName = name;
            FillProperties(obj);
        }

        void FillProperties(IDictionary<string, object> obj)
        {
            foreach (var item in obj)
            {
                var insert = item.Value is IDictionary<string, object> ? new DeepStreamInnerRecord(item.Key, item.Value as IDictionary<string, object>) : item.Value;

                properties.Add(new RecordPropertyWrapper(item.Key, insert));
            }
        }

        protected void Merge(IDictionary<string, object> obj)
        {
            foreach (var item in obj)
            {
                if (!SetPropertyValue(item.Key, item.Value))
                    properties.Add(new RecordPropertyWrapper(item.Key, item.Value));
            }
        }

        protected void UpdatePartial(string updatePath, object value)
        {
            var split = updatePath.Split('.');
            if (split.Length == 1)
                SetPropertyValue(split[0], value);
            else if (split.Length > 1)
            {
                var property = GetPropertyValue(split[0]) as DeepStreamInnerRecord;
                property.UpdatePartial(string.Join(".", split, 1, split.Length - 2), value);
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
    }
}