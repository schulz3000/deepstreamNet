using System.Collections.Generic;
using System.Collections.Specialized;
using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDeepStreamList: INotifyCollectionChanged
    {
        /// <summary>
        /// 
        /// </summary>
        string ListName { get; }        

        /// <summary>
        /// 
        /// </summary>
        int Count { get; }

        /// <summary>
        /// 
        /// </summary>
        bool IsEmpty { get; }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="item"></param>
        Task Add(string item);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="items"></param>
        void AddRange(IEnumerable<string> items);

        /// <summary>
        /// 
        /// </summary>
        void Clear();

        /// <summary>
        /// 
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        bool Contains(string item);       

        /// <summary>
        /// 
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        bool Remove(string item);
    }
}
