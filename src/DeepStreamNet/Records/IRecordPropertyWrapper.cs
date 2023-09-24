namespace DeepStreamNet
{
    internal interface IRecordPropertyWrapper
    {
        string Name { get; }
        object Value { get; set; }
    }
}