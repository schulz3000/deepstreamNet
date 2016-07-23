namespace DeepStreamNet
{
    interface IRecordPropertyWrapper
    {
        string Name { get; }
        object Value { get; set; }
    }
}