using System;
using System.ComponentModel;
using System.Dynamic;
using System.Linq.Expressions;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;

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

        public event PropertyChangingEventHandler PropertyChanging
        {
            add { innerRecord.PropertyChanging += value; }
            remove { innerRecord.PropertyChanging -= value; }
        }

        public event PropertyChangedEventHandler PropertyChanged
        {
            add { innerRecord.PropertyChanged += value; }
            remove { innerRecord.PropertyChanged -= value; }
        }

        public event EventHandler RecordChanged
        {
            add { innerRecord.RecordChanged += value; }
            remove { innerRecord.RecordChanged -= value; }
        }

        public DynamicMetaObject GetMetaObject(Expression parameter)
            => innerRecord.GetMetaObject(parameter);
    }
}
