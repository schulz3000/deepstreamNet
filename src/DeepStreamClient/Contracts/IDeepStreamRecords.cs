using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    public interface IDeepStreamRecords
    {
        Task<IDeepStreamRecord> GetRecordAsync(string name);

        Task SaveAsync(IDeepStreamRecord record);

        Task DiscardAsync(IDeepStreamRecord record);

        Task DeleteAsync(IDeepStreamRecord record);
    }
}