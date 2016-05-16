using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
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
