namespace DeepStreamNet
{
    static class Constants
    {
        internal const char RecordSeperator = (char)31;
        internal const char GroupSeperator = (char)30;

        internal static class Types
        {
            public const char STRING = 'S';
            public const char OBJECT = 'O';
            public const char NUMBER = 'N';
            public const char NULL = 'L';
            public const char TRUE = 'T';
            public const char FALSE = 'F';
            public const char UNDEFINED = 'U';
        }
    }
}