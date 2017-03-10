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
        Task<IDeepStreamRecord> GetRecordAsync(string name);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="record"></param>
        /// <returns></returns>
        void Save(IDeepStreamRecord record);

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
        /// <returns></returns>
        Task<bool> HasAsync(string name);
    }
}