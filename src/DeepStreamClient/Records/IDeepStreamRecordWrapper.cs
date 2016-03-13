using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    interface IDeepStreamRecordWrapper : IDeepStreamRecord
    {
        string RecordName { get; }
        int RecordVersion { get; }
    }
}