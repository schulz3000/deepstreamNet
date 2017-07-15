namespace DeepStreamNet
{
    class RecordPropertyWrapper : IRecordPropertyWrapper
    {
        readonly object initialValue;

        public RecordPropertyWrapper(string name, object value)
        {
            Name = name;
            initialValue = value;
            Value = value;
        }

        public string Name { get; }

        object currentValue;

        public object Value
        {
            get { return currentValue; }
            set
            {
                currentValue = value;
                IsDirty = value != initialValue;
            }
        }

        public bool IsDirty
        {
            get;
            private set;
        }
    }
}