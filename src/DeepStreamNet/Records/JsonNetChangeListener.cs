using Newtonsoft.Json.Linq;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;

namespace DeepStreamNet
{
    class JsonNetChangeListener : INotifyPropertyChanged, INotifyPropertyChanging, IDisposable
    {
        readonly INotifyCollectionChanged Collection;
        readonly HashSet<JsonNetChangeListener> subListener = new HashSet<JsonNetChangeListener>();

        public event PropertyChangedEventHandler PropertyChanged;
        public event PropertyChangingEventHandler PropertyChanging;

        protected JsonNetChangeListener(INotifyCollectionChanged collection)
        {
            Collection = collection;

            foreach (var item in ((IEnumerable)collection).OfType<JProperty>().Where(w => w.Value.Type == JTokenType.Array || w.Value.Type == JTokenType.Object))
            {
                var nlistener = Create((INotifyCollectionChanged)item.Value);
                subListener.Add(nlistener);
                nlistener.PropertyChanging += OnPropertyChanging;
                nlistener.PropertyChanged += OnPropertyChanged;
            }

            Collection.CollectionChanged += Collection_CollectionChanged;
        }

        void OnPropertyChanging(object sender, PropertyChangingEventArgs args)
            => PropertyChanging?.Invoke(Collection, args);

        void OnPropertyChanged(object sender, PropertyChangedEventArgs args)
            => PropertyChanged?.Invoke(Collection, args);

        void Collection_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if ((e.Action == NotifyCollectionChangedAction.Replace || e.Action == NotifyCollectionChangedAction.Remove || e.Action == NotifyCollectionChangedAction.Reset) && e.OldItems != null)
            {
                foreach (var item in e.OldItems)
                {
                    if (subListener.Contains(item))
                    {
                        var notify = subListener.FirstOrDefault(f => f == item);
                        notify.PropertyChanging -= OnPropertyChanging;
                        notify.PropertyChanged -= OnPropertyChanged;
                        subListener.Remove(notify);
                        notify.Dispose();
                    }
                }
            }

            if ((e.Action == NotifyCollectionChangedAction.Add || e.Action == NotifyCollectionChangedAction.Replace) && e.NewItems != null)
            {
                foreach (var item in e.NewItems.OfType<JContainer>().Where(w => w.Type == JTokenType.Array || w.Type == JTokenType.Object))
                {
                    var nlistener = Create((INotifyCollectionChanged)item);
                    subListener.Add(nlistener);
                    nlistener.PropertyChanging += OnPropertyChanging;
                    nlistener.PropertyChanged += OnPropertyChanged;
                }
            }

            if ((e.Action != NotifyCollectionChangedAction.Add && e.Action != NotifyCollectionChangedAction.Replace) || e.NewItems == null)
                return;

            foreach (var item in e.NewItems.OfType<JValue>())
            {
                PropertyChanged?.Invoke(Collection, new PropertyChangedEventArgs(item.Path));
            }

            foreach (var item in e.NewItems.OfType<JProperty>()/*.Where(w => w.Value.Type != JTokenType.Array && w.Value.Type != JTokenType.Object)*/)
            {
                PropertyChanged?.Invoke(Collection, new PropertyChangedEventArgs(item.Path));
            }

            foreach (var item in e.NewItems.OfType<JObject>())
            {
                PropertyChanged?.Invoke(Collection, new PropertyChangedEventArgs(item.Path));
            }
        }

        public void Dispose()
        {
            Collection.CollectionChanged -= Collection_CollectionChanged;
            foreach (var item in subListener)
            {
                item.PropertyChanging -= OnPropertyChanging;
                item.PropertyChanged -= OnPropertyChanged;
                item.Dispose();
            }
        }

        public static JsonNetChangeListener Create(INotifyCollectionChanged collection)
            => new JsonNetChangeListener(collection);
    }
}
