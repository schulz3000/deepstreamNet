using DeepStreamNet.Contracts;
using System;
using System.ComponentModel;
using System.Dynamic;
using System.Linq.Expressions;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    class DeepStreamAnonymousRecord : IDeepStreamAnonymousRecord
    {
        readonly DeepStreamRecords Context;
        IDeepStreamRecord innerRecord;
        public DeepStreamAnonymousRecord(DeepStreamRecords context)
        {
            Context = context;
        }

        public async Task SetNameAsync(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentNullException(nameof(name));

            innerRecord = await Context.GetRecordAsync(name).ConfigureAwait(false);
        }

        public dynamic this[object key]
        {
            get => innerRecord[key];
            set => innerRecord[key] = value;
        }

        public string RecordName => innerRecord.RecordName;

        public event PropertyChangedEventHandler PropertyChanged
        {
            add { innerRecord.PropertyChanged += value; }
            remove { innerRecord.PropertyChanged -= value; }
        }

        public DynamicMetaObject GetMetaObject(Expression parameter)
            => innerRecord.GetMetaObject(parameter);
    }
}
