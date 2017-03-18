using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Reflection;

namespace DeepStreamNet
{
    class ChildChangeListener : ChangeListener
    {
        protected static readonly Type _inotifyType = typeof(INotifyPropertyChanged);

        public INotifyPropertyChanged Value { get; }

        readonly Type _type;
        readonly Dictionary<string, ChangeListener> _childListeners = new Dictionary<string, ChangeListener>();

        public ChildChangeListener(INotifyPropertyChanged instance)
        {
            Value = instance ?? throw new ArgumentNullException(nameof(instance));
            _type = Value.GetType();

            Subscribe();
        }

        public ChildChangeListener(INotifyPropertyChanged instance, string propertyName)
            : this(instance)
        {
            _propertyName = propertyName;
        }

        public override void Resubscribe(INotifyCollectionChanged item)
        {
            throw new NotImplementedException();
        }

        void Subscribe()
        {
            Value.PropertyChanged += Value_PropertyChanged;

            if (Value is JObject obj)
            {
                var list = obj.Properties().Select(s => s.Name).ToArray();
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

            foreach (var property in query.ToArray())
            {
                // Declare property as known "Child", then register it
                _childListeners.Add(property.Name, null);
                ResetChildListener(property.Name);
            }
        }

        void ResetDynamicChildListener(string propertyName, JToken obj)
        {
            if (_childListeners.ContainsKey(propertyName))
            {
                // Unsubscribe if existing
                if (_childListeners[propertyName] != null)
                {
                    _childListeners[propertyName].PropertyChanged -= Child_PropertyChanged;

                    // Should unsubscribe all events
                    _childListeners[propertyName].Dispose();
                    _childListeners[propertyName] = null;
                }

                var property = obj[propertyName];
                if (property == null)
                    throw new InvalidOperationException(string.Format("Was unable to get '{0}' property information from Type '{1}'", propertyName, "?"));

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
                        _childListeners[propertyName].PropertyChanged += Child_PropertyChanged;
                }
            }
        }

        /// <summary>
        /// Resets known (must exist in children collection) child event handlers
        /// </summary>
        /// <param name="propertyName">Name of known child property</param>
        /// <exception cref="InvalidOperationException"></exception>
        void ResetChildListener(string propertyName)
        {
            if (_childListeners.ContainsKey(propertyName))
            {
                // Unsubscribe if existing
                if (_childListeners[propertyName] != null)
                {
                    _childListeners[propertyName].PropertyChanged -= Child_PropertyChanged;

                    // Should unsubscribe all events
                    _childListeners[propertyName].Dispose();
                    _childListeners[propertyName] = null;
                }

                var property = _type.GetProperty(propertyName);
                if (property == null)
                    throw new InvalidOperationException(string.Format("Was unable to get '{0}' property information from Type '{1}'", propertyName, _type.Name));

                var newValue = property.GetValue(Value, null);

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
                        _childListeners[propertyName].PropertyChanged += Child_PropertyChanged;
                }
            }
        }

        void Child_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            RaisePropertyChanged(e.PropertyName);
        }

        void Value_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            // First, reset child on change, if required... 
            if (sender is JToken)
                ResetDynamicChildListener(e.PropertyName, sender as JToken);
            else
                ResetChildListener(e.PropertyName);

            // ...then, notify about it
            RaisePropertyChanged(e.PropertyName);
        }

        protected override void RaisePropertyChanged(string propertyName)
        {
            var x = propertyName;
            // Special Formatting
            //base.RaisePropertyChanged(string.Format("{0}{1}{2}",
            //    _propertyName, _propertyName != null ? "." : null, propertyName));
        }

        /// <summary>
        /// Release all child handlers and self handler
        /// </summary>
        protected override void Unsubscribe()
        {
            Value.PropertyChanged -= Value_PropertyChanged;

            foreach (var binderKey in _childListeners.Keys)
            {
                if (_childListeners[binderKey] != null)
                    _childListeners[binderKey].Dispose();
            }

            _childListeners.Clear();
        }
    }
}
