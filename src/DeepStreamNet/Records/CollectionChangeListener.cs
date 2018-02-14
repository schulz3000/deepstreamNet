﻿using Newtonsoft.Json.Linq;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;

namespace DeepStreamNet
{
    class CollectionChangeListener : ChangeListener
    {
        public INotifyCollectionChanged Value { get; }
        readonly Dictionary<INotifyPropertyChanged, ChangeListener> childListeners = new Dictionary<INotifyPropertyChanged, ChangeListener>();
        readonly Dictionary<INotifyCollectionChanged, ChangeListener> collectionListeners = new Dictionary<INotifyCollectionChanged, ChangeListener>();

        public CollectionChangeListener(INotifyCollectionChanged collection, string propertyName)
        {
            Value = collection;
            PropertyName = propertyName;

            Subscribe();
        }

        void Subscribe()
        {
            Value.CollectionChanged += Value_CollectionChanged;

            foreach (INotifyPropertyChanged item in ((IEnumerable)Value).OfType<INotifyPropertyChanged>())
            {
                ResetChildListener(item);
            }

            foreach (INotifyCollectionChanged item in ((IEnumerable)Value).OfType<INotifyCollectionChanged>())
            {
                ResetCollectionListener(item);
            }
        }

        void ResetChildListener(INotifyPropertyChanged item)
        {
            if (item == null)
                throw new ArgumentNullException(nameof(item));

            RemoveChildItem(item);

            // Add new
            var trackableCollection = item as INotifyCollectionChanged;
            var listener = trackableCollection != null
                ? (ChangeListener)new CollectionChangeListener(trackableCollection, PropertyName)
                : new ChildChangeListener(item);

            listener.PropertyChanged += Listener_PropertyChanged;
            childListeners.Add(item, listener);
        }

        void ResetCollectionListener(INotifyCollectionChanged item)
        {
            if (item == null)
                throw new ArgumentNullException(nameof(item));

            RemoveCollectionItem(item);

            // Add new
            var trackableCollection = item as INotifyCollectionChanged;
            var listener = new CollectionChangeListener(trackableCollection, PropertyName);

            listener.PropertyChanged += Listener_PropertyChanged;
            collectionListeners.Add(item, listener);
        }

        void RemoveChildItem(INotifyPropertyChanged item)
        {
            // Remove old
            if (childListeners.ContainsKey(item))
            {
                childListeners[item].PropertyChanged -= Listener_PropertyChanged;

                childListeners[item].Dispose();
                childListeners.Remove(item);
            }
        }

        void RemoveCollectionItem(INotifyCollectionChanged item)
        {
            if (collectionListeners.ContainsKey(item))
            {
                collectionListeners[item].PropertyChanged -= Listener_PropertyChanged;

                collectionListeners[item].Dispose();
                collectionListeners.Remove(item);
            }
        }

        void ClearCollection()
        {
            foreach (var key in childListeners.Keys)
            {
                childListeners[key].Dispose();
            }

            childListeners.Clear();
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
                    foreach (INotifyPropertyChanged item in e.OldItems.OfType<INotifyPropertyChanged>())
                        RemoveChildItem(item);

                    foreach (INotifyCollectionChanged item in e.OldItems.OfType<INotifyCollectionChanged>())
                        RemoveCollectionItem(item);
                }

                // ...add new items as well
                if (e.NewItems != null)
                {
                    foreach (INotifyPropertyChanged item in e.NewItems.OfType<INotifyPropertyChanged>())
                        ResetChildListener(item);

                    foreach (INotifyCollectionChanged item in e.NewItems.OfType<INotifyCollectionChanged>())
                        ResetCollectionListener(item);

                    foreach (var item in e.NewItems.OfType<JValue>())
                        RaisePropertyChanged(item.Path);
                }
            }
        }

        void Listener_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            // ...then, notify about it
            // ReSharper disable once ExplicitCallerInfoArgument
            RaisePropertyChanged(PropertyName+(PropertyName != null ? "[]." : string.Empty)+e.PropertyName);
        }

        /// <summary>
        /// Releases all collection item handlers and self handler
        /// </summary>
        protected override void Unsubscribe()
        {
            ClearCollection();

            Value.CollectionChanged -= Value_CollectionChanged;

            Debug.WriteLine("CollectionChangeListener unsubscribed");
        }
    }
}
