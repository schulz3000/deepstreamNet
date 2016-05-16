using System.ComponentModel;
using System.Dynamic;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDeepStreamRecord : IDynamicMetaObjectProvider, INotifyPropertyChanging, INotifyPropertyChanged
    {
        /// <summary>
        /// Name of record
        /// </summary>
        string RecordName { get; }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        object this[string name]
        {
            get;
            set;
        }
    }
}