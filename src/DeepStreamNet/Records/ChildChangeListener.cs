using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Reflection;

namespace DeepStreamNet
{
    internal class ChildChangeListener : ChangeListener
    {
        private readonly INotifyPropertyChanged value;
        private readonly Type type;
        private readonly Dictionary<string, ChangeListener> childListeners = new();

        public ChildChangeListener(INotifyPropertyChanged instance)
        {
            value = instance ?? throw new ArgumentNullException(nameof(instance));
            type = value.GetType();

            Subscribe();
        }

        public ChildChangeListener(INotifyPropertyChanged instance, string propertyName)
            : this(instance)
        {
            PropertyName = propertyName;
        }

        private void Subscribe()
        {
            value.PropertyChanged += Value_PropertyChanged;

            foreach (var name in type.GetTypeInfo().DeclaredProperties
                .Where(property => IsPubliclyReadable(property) && IsNotifier(property.GetValue(value)))
                .Select(property => property.Name))
            {
                ResetChildListener(name);
            }
        }

        private static bool IsPubliclyReadable(PropertyInfo prop) => prop.GetMethod?.IsPublic ?? false && !prop.GetMethod.IsStatic;

        private static bool IsNotifier(object value) => value is INotifyCollectionChanged or INotifyPropertyChanged;

        /// <summary>
        /// Resets known (must exist in children collection) child event handlers
        /// </summary>
        /// <param name="propertyName">Name of known child property</param>
        /// <exception cref="InvalidOperationException"></exception>
        private void ResetChildListener(string propertyName)
        {
            // Unsubscribe if existing
            if (childListeners.TryGetValue(propertyName, out var listener)
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
                throw new InvalidOperationException($"Was unable to get '{propertyName}' property information from Type '{type.Name}'");
            }

            object newValue = property.GetValue(value, null);

            // Only recreate if there is a new value
            if (newValue != null)
            {
                if (newValue is INotifyCollectionChanged changed)
                {
                    listener = childListeners[propertyName] = new CollectionChangeListener(changed, propertyName);
                }
                else if (newValue is INotifyPropertyChanged instance)
                {
                    listener = childListeners[propertyName] = new ChildChangeListener(instance, propertyName);
                }
                else
                {
                    Debug.WriteLine($"not listening to {propertyName} as {newValue} is {newValue.GetType()}");
                }

                if (listener != null)
                {
                    listener.PropertyChanged += Child_PropertyChanged;
                }
            }
        }

        private void Child_PropertyChanged(object sender, PropertyChangedEventArgs e)
            => RaisePropertyChanged(propertyName: e.PropertyName);

        private void Value_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            // First, reset child on change, if required...
            ResetChildListener(e.PropertyName);

            // ...then, notify about it
            RaisePropertyChanged(propertyName: e.PropertyName);
        }

        protected override void RaisePropertyChanged(string propertyName) =>
            // Special Formatting
            base.RaisePropertyChanged(PropertyName + (PropertyName != null ? "." : null) + propertyName);

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
        }
    }
}
