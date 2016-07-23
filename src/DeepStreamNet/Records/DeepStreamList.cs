using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DeepStreamNet;
using DeepStreamNet.Contracts;

namespace DeepStreamNet.Records
{
    class DeepStreamList : INotifyCollectionChanged
    {
        readonly Dictionary<string, IDeepStreamRecord> dict = new Dictionary<string, IDeepStreamRecord>();

        public event NotifyCollectionChangedEventHandler CollectionChanged;

        public string ListName { get; }

        public DeepStreamList(string name)
        {
            ListName = name;
        }

        public IDeepStreamRecord this[string name]
        {
            get
            {
                return dict[name];
            }

            set
            {
                if (dict.ContainsKey(name))
                {
                    var oldItem = dict[name];
                    dict[name] = value;
                    CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, oldItem, value));
                }
                else
                {
                    Add(value);
                }
            }
        }

        public int Count
        {
            get
            {
                return dict.Count;
            }
        }

        public void Add(IDeepStreamRecord item)
        {
            dict.Add(item.RecordName, item);
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item));
        }

        public void AddRange(IEnumerable<IDeepStreamRecord> items)
        {
            foreach (var item in items)
                dict.Add(item.RecordName, item);

            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, items));
        }

        public void Clear()
        {
            dict.Clear();
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public bool Contains(IDeepStreamRecord item)
        {
            return dict.ContainsKey(item.RecordName);
        }

        public bool Contains(string name)
        {
            return dict.ContainsKey(name);
        }

        public bool Remove(IDeepStreamRecord item)
        {
            return Remove(item.RecordName);
        }

        public bool Remove(string name)
        {
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, dict[name]));

            return dict.Remove(name);
        }
    }
}
