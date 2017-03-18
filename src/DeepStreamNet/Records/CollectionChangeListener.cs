using Newtonsoft.Json.Linq;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;

namespace DeepStreamNet
{
    class CollectionChangeListener : ChangeListener
    {
        public INotifyCollectionChanged Value { get; private set; }
        readonly Dictionary<INotifyPropertyChanged, ChangeListener> _collectionListeners = new Dictionary<INotifyPropertyChanged, ChangeListener>();

        public CollectionChangeListener(INotifyCollectionChanged collection, string propertyName)
        {
            Value = collection;
            _propertyName = propertyName;

            Subscribe();
        }

        public override void Resubscribe(INotifyCollectionChanged item)
        {
            Unsubscribe();
            Value = item;
            Subscribe();
        }

        void Subscribe()
        {
            Value.CollectionChanged += Value_CollectionChanged;

            foreach (var item in (IEnumerable)Value)
            {
                if (item is INotifyPropertyChanged notifyableItem)
                    ResetChildListener(notifyableItem);
            }
        }

        void ResetChildListener(INotifyPropertyChanged item)
        {
            if (item == null)
                throw new ArgumentNullException(nameof(item));

            RemoveItem(item);

            ChangeListener listener = null;

            // Add new
            if (item is INotifyCollectionChanged)
                listener = new CollectionChangeListener(item as INotifyCollectionChanged, _propertyName);
            else
                listener = new ChildChangeListener(item as INotifyPropertyChanged);

            listener.PropertyChanged += Listener_PropertyChanged;
            _collectionListeners.Add(item, listener);
        }

        void RemoveItem(INotifyPropertyChanged item)
        {
            // Remove old
            if (_collectionListeners.ContainsKey(item))
            {
                _collectionListeners[item].PropertyChanged -= Listener_PropertyChanged;

                _collectionListeners[item].Dispose();
                _collectionListeners.Remove(item);
            }
        }

        void ClearCollection()
        {
            foreach (var key in _collectionListeners.Keys)
            {
                _collectionListeners[key].Dispose();
            }

            _collectionListeners.Clear();
        }

        void Value_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.Action == NotifyCollectionChangedAction.Reset)
            {
                ClearCollection();
            }
            else
            {
                // Don't care about e.Action, if there are old items, Remove them...
                if (e.OldItems != null)
                {
                    foreach (var item in e.OldItems.OfType<INotifyPropertyChanged>().ToArray())
                        RemoveItem(item);
                }

                // ...add new items as well
                if (e.NewItems != null)
                {
                    foreach (var item in e.NewItems.OfType<INotifyPropertyChanged>().ToArray())
                    {
                        ResetChildListener(item);
                    }

                    foreach (var item in e.NewItems.OfType<object>().Where(w => !(w is INotifyPropertyChanged)).ToArray())
                    {
                        RaisePropertyChanged(((JToken)item).Path);
                    }
                        //RaisePropertyChanged(_propertyName + "." + e.NewStartingIndex);
                }
            }
        }

        void Listener_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            // ...then, notify about it

            var record = ((sender as ChildChangeListener)?.Value as IDeepStreamRecordWrapper)?.RecordName;

            if (record != null)
            {
                RaisePropertyChanged(record + "." + e.PropertyName);
            }
            else
            {
                RaisePropertyChanged(string.Format("{0}{1}{2}",
                   _propertyName, _propertyName != null ? "[]." : null, e.PropertyName));
            }
        }

        /// <summary>
        /// Releases all collection item handlers and self handler
        /// </summary>
        protected override void Unsubscribe()
        {
            ClearCollection();

            Value.CollectionChanged -= Value_CollectionChanged;
        }
    }
}
