using System.ComponentModel;
using System.Dynamic;

namespace DeepStreamNet.Contracts
{
    public interface IDeepStreamRecord : IDynamicMetaObjectProvider, INotifyPropertyChanging, INotifyPropertyChanged
    {
        object this[string name]
        {
            get;
            set;
        }
    }
}