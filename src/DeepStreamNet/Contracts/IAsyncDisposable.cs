using System.Threading.Tasks;

namespace DeepStreamNet.Contracts
{
    /// <summary>
    ///
    /// </summary>
    public interface IAsyncDisposable
    {
        /// <summary>
        ///
        /// </summary>
        /// <returns></returns>
        Task DisposeAsync();
    }
}
