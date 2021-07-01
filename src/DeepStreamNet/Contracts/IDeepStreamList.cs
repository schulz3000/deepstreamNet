using System.Collections.Generic;
using System.Collections.Specialized;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    ///
    /// </summary>
    public interface IDeepStreamList : IList<string>, INotifyCollectionChanged
    {
        /// <summary>
        ///
        /// </summary>
        string ListName { get; }
    }
}
