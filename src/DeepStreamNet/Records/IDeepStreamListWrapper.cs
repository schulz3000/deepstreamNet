using DeepStreamNet.Contracts;
using Newtonsoft.Json.Linq;

namespace DeepStreamNet
{
    internal interface IDeepStreamListWrapper : IDeepStreamList
    {
        void Update(JToken list);
    }
}
