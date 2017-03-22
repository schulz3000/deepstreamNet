using DeepStreamNet.Contracts;
using Newtonsoft.Json.Linq;

namespace DeepStreamNet
{
    interface IDeepStreamListWrapper : IDeepStreamList
    {
        void Update(JToken list);
    }
}
