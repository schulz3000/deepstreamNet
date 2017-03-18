using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDeepStreamList: IList<string>, INotifyCollectionChanged
    {
        /// <summary>
        /// 
        /// </summary>
        string ListName { get; }
    }
}
