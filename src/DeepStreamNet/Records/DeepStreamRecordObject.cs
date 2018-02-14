using Newtonsoft.Json.Linq;
using System.Collections.Generic;
using System.Dynamic;
using System.Linq;

namespace DeepStreamNet
{
    class DeepStreamRecordObject : DeepStreamRecordBase<JObject>
    {
        public DeepStreamRecordObject(string name, int version, JObject data)
            : base(name, version, data)
        {
        }

        public override dynamic this[object key]
        {
            get => GetPropertyValue(key).ToObject<object>();
            set => SetPropertyValue(key, JToken.FromObject(value));
        }

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = GetPropertyValue(binder.Name);
            if (result != null)
                return true;

            return false;
        }

        public override bool TrySetMember(SetMemberBinder binder, object value)
            => SetPropertyValue(binder.Name, JToken.FromObject(value));

        public override IEnumerable<string> GetDynamicMemberNames()
            => Data.Properties().Select(s => s.Name).ToList();

        protected JToken GetPropertyValue(object key) => Data[key];

        protected bool SetPropertyValue(object key, JToken value)
        {
            if (Data[key] == null)
            {
                Data.Add(key.ToString(), value);
            }
            else
            {
                Data[key] = value;
            }

            return true;
        }
    }
}
