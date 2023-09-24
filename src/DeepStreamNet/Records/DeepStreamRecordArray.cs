using Newtonsoft.Json.Linq;
using System.Collections.Generic;
using System.Linq;
using System.Collections;
using System.Dynamic;

namespace DeepStreamNet
{
    internal class DeepStreamRecordArray : DeepStreamRecordBase<JArray>, IEnumerable<object>
    {
        public DeepStreamRecordArray(string name, int version, JArray data)
            : base(name, version, data)
        {
        }

        public JToken this[int index]
        {
            get => Data[index];
            set => Data[index] = value;
        }

        public override bool TryConvert(ConvertBinder binder, out object result)
        {
            if(Data.GetType() == binder.Type)
            {
                result = Data.ToArray();
                return true;
            }

            return base.TryConvert(binder, out result);
        }

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            result = null;
            return true;
        }

        public IEnumerator<object> GetEnumerator() => Data.GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => Data.GetEnumerator();
    }
}
