using Newtonsoft.Json.Linq;
using System.ComponentModel;
using System.Dynamic;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDeepStreamRecord : IDynamicMetaObjectProvider, /*INotifyPropertyChanging,*/ INotifyPropertyChanged
    {
        /// <summary>
        /// Name of record
        /// </summary>
        string RecordName { get; }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        dynamic this[object key] { get; set; }
    }
}