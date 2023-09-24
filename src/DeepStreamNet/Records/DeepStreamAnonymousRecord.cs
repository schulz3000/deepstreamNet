using System;
using System.ComponentModel;
using System.Dynamic;
using System.Linq.Expressions;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    internal class DeepStreamAnonymousRecord : IDeepStreamAnonymousRecord
    {
        private readonly DeepStreamRecords _context;
        private IDeepStreamRecord _innerRecord;

        public DeepStreamAnonymousRecord(DeepStreamRecords context)
        {
            _context = context;
        }

        public async Task SetNameAsync(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
            {
                throw new ArgumentNullException(nameof(name));
            }

            _innerRecord = await _context.GetRecordAsync(name).ConfigureAwait(false);
        }

        public dynamic this[object key]
        {
            get => _innerRecord[key];
            set => _innerRecord[key] = value;
        }

        public string RecordName => _innerRecord.RecordName;

        public event PropertyChangingEventHandler PropertyChanging
        {
            add => _innerRecord.PropertyChanging += value;
            remove => _innerRecord.PropertyChanging -= value;
        }

        public event PropertyChangedEventHandler PropertyChanged
        {
            add => _innerRecord.PropertyChanged += value;
            remove => _innerRecord.PropertyChanged -= value;
        }

        public event EventHandler RecordChanged
        {
            add { innerRecord.RecordChanged += value; }
            remove { innerRecord.RecordChanged -= value; }
        }

        public DynamicMetaObject GetMetaObject(Expression parameter)
            => _innerRecord.GetMetaObject(parameter);
    }
}
