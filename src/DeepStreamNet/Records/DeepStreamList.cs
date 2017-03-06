using System.Collections.Generic;
using System.Collections.Specialized;
using DeepStreamNet.Contracts;
using System.Threading.Tasks;
using System.Linq;

namespace DeepStreamNet.Records
{
    class DeepStreamList : IDeepStreamList, INotifyCollectionChanged
    {
        readonly HashSet<string> innerList = new HashSet<string>();
        readonly DeepStreamRecords deepStreamRecordsHandler;
        readonly dynamic innerRecord;

        public event NotifyCollectionChangedEventHandler CollectionChanged;

        public string ListName { get; }

        public DeepStreamList(string name, DeepStreamRecords deepStreamRecords , IDeepStreamRecord record)
        {
            ListName = name;
            deepStreamRecordsHandler = deepStreamRecords;
            innerRecord = record;
        }

        public int Count
        {
            get
            {
                return innerList.Count;
            }
        }

        public bool IsEmpty { get { return innerList.Count == 0; } }

        public async Task Add(string item)
        {
            innerList.Add(item);
            innerRecord.Add(item);
            ////(innerRecord as HashSet<string>).Add(item);
            await deepStreamRecordsHandler.Save(innerRecord);
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item));
        }

        public void AddRange(IEnumerable<string> items)
        {
            foreach (var item in items.ToArray())
                innerList.Add(item);

            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, items));
        }

        public void Clear()
        {
            innerList.Clear();
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }       

        public bool Contains(string name)
        {
            return innerList.Contains(name);
        }

        public bool Remove(string item)
        {
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item));
            return innerList.Remove(item);
        }        
    }
}
