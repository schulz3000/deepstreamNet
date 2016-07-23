using System;
using System.Collections.Specialized;
using System.ComponentModel;

namespace DeepStreamNet.Records
{   
        abstract class ChangeListener : INotifyPropertyChanged, IDisposable
        {            
            protected string _propertyName;
                        
            protected abstract void Unsubscribe();
                      
            public event PropertyChangedEventHandler PropertyChanged;

            protected virtual void RaisePropertyChanged(string propertyName)
            {
                PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
            }
            
            public void Dispose()
            {
                Dispose(true);
                GC.SuppressFinalize(this);
            }

            protected virtual void Dispose(bool disposing)
            {
                if (disposing)
                {
                    Unsubscribe();
                }
            }

            ~ChangeListener()
            {
                Dispose(false);
            }
            
            public static ChangeListener Create(INotifyPropertyChanged value)
            {
                return Create(value, null);
            }

            public static ChangeListener Create(INotifyPropertyChanged value, string propertyName)
            {
                if (value is INotifyCollectionChanged)
                {
                    return new CollectionChangeListener(value as INotifyCollectionChanged, propertyName);
                }

                if (value is INotifyPropertyChanged)
                {
                    return new ChildChangeListener(value as INotifyPropertyChanged, propertyName);
                }
                
                return null;
            }            
        }
    
}
