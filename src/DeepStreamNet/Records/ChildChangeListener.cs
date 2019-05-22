using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Reflection;

namespace DeepStreamNet
{
    class ChildChangeListener : ChangeListener
    {
        readonly INotifyPropertyChanged value;
        readonly Type type;
        readonly Dictionary<string, ChangeListener> childListeners = new Dictionary<string, ChangeListener>();

        public ChildChangeListener(INotifyPropertyChanged instance)
        {
            if (instance == null)
                throw new ArgumentNullException(nameof(instance));

            Debug.WriteLine($"creating {nameof(ChildChangeListener)} for {instance}");

            value = instance;
            type = value.GetType();

            Subscribe();
        }

        public ChildChangeListener(INotifyPropertyChanged instance, string propertyName)
            : this(instance)
        {
            PropertyName = propertyName;
        }

        void Subscribe()
        {
            value.PropertyChanged += Value_PropertyChanged;

            foreach (var property in type.GetTypeInfo().DeclaredProperties)
            {
                if (!IsPubliclyReadable(property))
                    continue;
                if (!IsNotifier(property.GetValue(value)))
                    continue;

                ResetChildListener(property.Name);
            }
        }

        static bool IsPubliclyReadable(PropertyInfo prop) => (prop.GetMethod?.IsPublic ?? false) && !prop.GetMethod.IsStatic;

        static bool IsNotifier(object value) => (value is INotifyCollectionChanged) || (value is INotifyPropertyChanged);

        /// <summary>
        /// Resets known (must exist in children collection) child event handlers
        /// </summary>
        /// <param name="propertyName">Name of known child property</param>
        /// <exception cref="InvalidOperationException"></exception>
        void ResetChildListener(string propertyName)
        {
            // Unsubscribe if existing
            if (childListeners.TryGetValue(propertyName, out ChangeListener listener)
                && listener != null)
            {
                listener.PropertyChanged -= Child_PropertyChanged;

                // Should unsubscribe all events
                listener.Dispose();
                listener = null;
                childListeners.Remove(propertyName);
            }

            var property = type.GetTypeInfo().GetDeclaredProperty(propertyName);
            if (property == null)
            {
                throw new InvalidOperationException(
                   $"Was unable to get '{propertyName}' property information from Type '{type.Name}'");
            }

            object newValue = property.GetValue(value, null);

            // Only recreate if there is a new value
            if (newValue != null)
            {
                if (newValue is INotifyCollectionChanged changed)
                {
                    listener = childListeners[propertyName] =
                        new CollectionChangeListener(changed, propertyName);
                }
                else if (newValue is INotifyPropertyChanged instance)
                {
                    listener = childListeners[propertyName] =
                        new ChildChangeListener(instance, propertyName);
                }
                else
                {
                    Debug.WriteLine($"not listening to {propertyName} as {newValue} is {newValue.GetType()}");
                }

                if (listener != null)
                    listener.PropertyChanged += Child_PropertyChanged;
            }
        }

        void Child_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            RaisePropertyChanged(propertyName: e.PropertyName);
        }

        void Value_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            // First, reset child on change, if required...
            ResetChildListener(e.PropertyName);

            // ...then, notify about it
            RaisePropertyChanged(propertyName: e.PropertyName);
        }

        protected override void RaisePropertyChanged(string propertyName)
        {
            // Special Formatting
            base.RaisePropertyChanged($"{PropertyName}{(PropertyName != null ? "." : null)}{propertyName}");
        }

        /// <summary>
        /// Release all child handlers and self handler
        /// </summary>
        protected override void Unsubscribe()
        {
            value.PropertyChanged -= Value_PropertyChanged;

            foreach (var binderKey in childListeners.Keys)
            {
                childListeners[binderKey]?.Dispose();
            }

            childListeners.Clear();

            Debug.WriteLine("ChildChangeListener '{0}' unsubscribed", PropertyName);
        }
    }
}
