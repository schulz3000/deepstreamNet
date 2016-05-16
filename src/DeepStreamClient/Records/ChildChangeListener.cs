using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Reflection;

namespace DeepStreamNet.Records
{
    class ChildChangeListener : ChangeListener
    {
        protected static readonly Type _inotifyType = typeof(INotifyPropertyChanged);

        public INotifyPropertyChanged Value { get; }

        readonly Type _type;
        readonly Dictionary<string, ChangeListener> _childListeners = new Dictionary<string, ChangeListener>();


        public ChildChangeListener(INotifyPropertyChanged instance)
        {
            if (instance == null)
                throw new ArgumentNullException(nameof(instance));

            Value = instance;
            _type = Value.GetType();

            Subscribe();
        }

        public ChildChangeListener(INotifyPropertyChanged instance, string propertyName)
            : this(instance)
        {
            _propertyName = propertyName;
        }


        void Subscribe()
        {
            Value.PropertyChanged += value_PropertyChanged;

            var obj = Value as DeepStreamInnerRecord;
            if (obj != null)
            {
                var list = obj.GetDynamicMemberNames();
                foreach (var name in list)
                {
                    _childListeners.Add(name, null);
                    ResetDynamicChildListener(name, obj);
                }
            }

            var query =
                from property
                in _type.GetProperties(BindingFlags.Instance | BindingFlags.Public)
                where _inotifyType.IsAssignableFrom(property.PropertyType)
                select property;

            foreach (var property in query)
            {
                // Declare property as known "Child", then register it
                _childListeners.Add(property.Name, null);
                ResetChildListener(property.Name);
            }
        }


        void ResetDynamicChildListener(string propertyName, DeepStreamInnerRecord obj)
        {
            if (_childListeners.ContainsKey(propertyName))
            {
                // Unsubscribe if existing
                if (_childListeners[propertyName] != null)
                {
                    _childListeners[propertyName].PropertyChanged -= child_PropertyChanged;

                    // Should unsubscribe all events
                    _childListeners[propertyName].Dispose();
                    _childListeners[propertyName] = null;
                }

                var property = obj[propertyName];
                if (property == null)
                    throw new InvalidOperationException(string.Format("Was unable to get '{0}' property information from Type '{1}'", propertyName, obj.RecordName));

                // Only recreate if there is a new value
                if (property != null)
                {
                    if (property is INotifyCollectionChanged)
                    {
                        _childListeners[propertyName] =
                            new CollectionChangeListener(property as INotifyCollectionChanged, propertyName);
                    }
                    else if (property is INotifyPropertyChanged)
                    {
                        _childListeners[propertyName] =
                            new ChildChangeListener(property as INotifyPropertyChanged, propertyName);
                    }

                    if (_childListeners[propertyName] != null)
                        _childListeners[propertyName].PropertyChanged += child_PropertyChanged;
                }

            }

        }


        /// <summary>
        /// Resets known (must exist in children collection) child event handlers
        /// </summary>
        /// <param name="propertyName">Name of known child property</param>
        void ResetChildListener(string propertyName)
        {
            if (_childListeners.ContainsKey(propertyName))
            {
                // Unsubscribe if existing
                if (_childListeners[propertyName] != null)
                {
                    _childListeners[propertyName].PropertyChanged -= child_PropertyChanged;

                    // Should unsubscribe all events
                    _childListeners[propertyName].Dispose();
                    _childListeners[propertyName] = null;
                }

                var property = _type.GetProperty(propertyName);
                if (property == null)
                    throw new InvalidOperationException(string.Format("Was unable to get '{0}' property information from Type '{1}'", propertyName, _type.Name));

                object newValue = property.GetValue(Value, null);

                // Only recreate if there is a new value
                if (newValue != null)
                {
                    if (newValue is INotifyCollectionChanged)
                    {
                        _childListeners[propertyName] =
                            new CollectionChangeListener(newValue as INotifyCollectionChanged, propertyName);
                    }
                    else if (newValue is INotifyPropertyChanged)
                    {
                        _childListeners[propertyName] =
                            new ChildChangeListener(newValue as INotifyPropertyChanged, propertyName);
                    }

                    if (_childListeners[propertyName] != null)
                        _childListeners[propertyName].PropertyChanged += child_PropertyChanged;
                }
            }
        }

        void child_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            RaisePropertyChanged(e.PropertyName);
        }

        void value_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            // First, reset child on change, if required... 
            if (sender is DeepStreamInnerRecord)
                ResetDynamicChildListener(e.PropertyName, sender as DeepStreamInnerRecord);
            else
                ResetChildListener(e.PropertyName);

            // ...then, notify about it
            RaisePropertyChanged(e.PropertyName);
        }

        protected override void RaisePropertyChanged(string propertyName)
        {
            // Special Formatting
            base.RaisePropertyChanged(string.Format("{0}{1}{2}",
                _propertyName, _propertyName != null ? "." : null, propertyName));
        }

        /// <summary>
        /// Release all child handlers and self handler
        /// </summary>
        protected override void Unsubscribe()
        {
            Value.PropertyChanged -= value_PropertyChanged;

            foreach (var binderKey in _childListeners.Keys)
            {
                if (_childListeners[binderKey] != null)
                    _childListeners[binderKey].Dispose();
            }

            _childListeners.Clear();            
        }
    }
}
