using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    interface IDeepStreamRecordWrapper : IDeepStreamRecord
    {        
        int RecordVersion { get; }
    }
}