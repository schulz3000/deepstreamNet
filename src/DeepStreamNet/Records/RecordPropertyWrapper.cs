namespace DeepStreamNet
{
    class RecordPropertyWrapper : IRecordPropertyWrapper
    {
        readonly object _initialValue;

        public RecordPropertyWrapper(string name, object value)
        {
            Name = name;
            _initialValue = value;
            Value = value;
        }

        public string Name { get; }

        object _currentValue;

        public object Value
        {
            get => _currentValue;
            set
            {
                _currentValue = value;
                IsDirty = value != _initialValue;
            }
        }

        public bool IsDirty
        {
            get;
            private set;
        }
    }
}