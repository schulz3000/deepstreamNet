using System.Collections.Generic;
using DeepStreamNet.Contracts;
using System.Collections.Specialized;
using System;
using System.Collections;
using Newtonsoft.Json.Linq;
using System.Linq;

namespace DeepStreamNet
{
    class DeepStreamList : IDeepStreamListWrapper
    {
        public string ListName { get; }

        public int Count => innerList.Count;

        public bool IsReadOnly => false;

        public string this[int index]
        {
            get => innerList[index];
            set
            {
                var oldItem = innerList[index];
                innerList[index] = value;
                CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, value, oldItem, index));
            }
        }

        readonly List<string> innerList = new List<string>();

        public event NotifyCollectionChangedEventHandler CollectionChanged;

        public DeepStreamList(string name, IEnumerable<string> initialList)
        {
            ListName = name;
            innerList.AddRange(initialList);
        }

        public int IndexOf(string item) => innerList.IndexOf(item);

        public void Insert(int index, string item)
        {
            innerList.Insert(index, item);
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item, null, index));
        }

        public void RemoveAt(int index)
        {
            var oldItem = innerList[index];
            innerList.RemoveAt(index);
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, null, oldItem, index));
        }

        public void Add(string item)
        {
            innerList.Add(item);
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, item));
        }

        public void Clear()
        {
            var tmp = innerList;
            innerList.Clear();
            CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset));
        }

        public bool Contains(string item) => innerList.Contains(item);

        public void CopyTo(string[] array, int arrayIndex) => innerList.CopyTo(array, arrayIndex);

        public bool Remove(string item)
        {
            var index = innerList.IndexOf(item);
            var result = innerList.Remove(item);
            if (result)
            {
                CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, item, index));
            }

            return result;
        }

        public IEnumerator<string> GetEnumerator() => innerList.GetEnumerator();

        IEnumerator IEnumerable.GetEnumerator() => innerList.GetEnumerator();

        public void Update(JToken list)
        {
            var newList = list.ToObject<List<string>>();

            if (newList.Count == 0 && innerList.Count != 0)
            {
                Clear();
                return;
            }

            var newItems = newList.Except(innerList).ToList();
            var removedItems = innerList.Except(newList).ToList();

            for (int i = 0; i < newItems.Count; i++)
            {
                Add(newItems[i]);
            }

            for (int i = 0; i < removedItems.Count; i++)
            {
                Remove(removedItems[i]);
            }
        }
    }
}
