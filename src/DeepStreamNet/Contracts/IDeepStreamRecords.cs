using System;
using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDeepStreamRecords
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        /// <exception cref="ArgumentNullException"></exception>
        Task<IDeepStreamRecord> GetRecordAsync(string name);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="record"></param>
        /// <param name="item"></param>
        /// <exception cref="ArgumentNullException"><paramref name="item"/> is <c>null</c>.</exception>
        /// <exception cref="DeepStreamException"><paramref name="item"/> must be class or array</exception>
        void Set(IDeepStreamRecord record, object item);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="record"></param>
        /// <param name="item"></param>
        /// <returns></returns>
        /// <exception cref="ArgumentNullException"><paramref name="item"/> is <c>null</c>.</exception>
        /// <exception cref="DeepStreamException"><paramref name="item"/> must be class or array.</exception>
        Task<bool> SetWithAckAsync(IDeepStreamRecord record, object item);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="record"></param>
        /// <param name="path"></param>
        /// <param name="item"></param>
        /// <returns></returns>
        /// <exception cref="ArgumentNullException"><paramref name="path"/> is <c>null</c>.</exception>
        /// <exception cref="ArgumentNullException"><paramref name="item"/> is <c>null</c>.</exception>
        void Set(IDeepStreamRecord record,string path, object item);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="record"></param>
        /// <param name="path"></param>
        /// <param name="item"></param>
        /// <returns></returns>
        Task<bool> SetWithAckAsync(IDeepStreamRecord record, string path, object item);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="record"></param>
        /// <returns></returns>
        Task DiscardAsync(IDeepStreamRecord record);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="record"></param>
        /// <returns></returns>
        Task DeleteAsync(IDeepStreamRecord record);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        Task<IDeepStreamList> GetListAsync(string name);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="name"></param>
        /// <exception cref="System.ArgumentNullException">Throws when param name is null or empty</exception>
        /// <returns></returns>
        Task<bool> HasAsync(string name);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        Task<IDeepStreamRecord> SnapshotAsync(string name);

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        IDeepStreamAnonymousRecord GetAnonymousRecord();

        /// <summary>
        /// 
        /// </summary>
        /// <param name="pattern"></param>
        /// <param name="listener"></param>
        /// <returns></returns>
        Task<IAsyncDisposable> ListenAsync(string pattern, Action<string, bool, IListenerResponse> listener);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="pattern"></param>
        /// <param name="listener"></param>
        /// <returns></returns>
        Task<IAsyncDisposable> ListenAsync(string pattern, Func<string, bool, IListenerResponse, Task> listener);
    }
}