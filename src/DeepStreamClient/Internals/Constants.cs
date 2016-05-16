namespace DeepStreamNet
{
    static class Constants
    {
        internal const char RecordSeperator = (char)31;
        internal const char GroupSeperator = (char)30;

        internal static readonly char[] PathSplitter = { '.' };

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

        internal static class Errors
        {
            public const string CONNECTION_STATE_CHANGED = "connectionStateChanged";
            public const string ACK_TIMEOUT = "ACK_TIMEOUT";
            public const string RESPONSE_TIMEOUT = "RESPONSE_TIMEOUT";
            public const string DELETE_TIMEOUT = "DELETE_TIMEOUT";
            public const string UNSOLICITED_MESSAGE = "UNSOLICITED_MESSAGE";
            public const string MESSAGE_PARSE_ERROR = "MESSAGE_PARSE_ERROR";
            public const string VERSION_EXISTS = "VERSION_EXISTS";
            public const string NOT_AUTHENTICATED = "NOT_AUTHENTICATED";
            public const string LISTENER_EXISTS = "LISTENER_EXISTS";
            public const string NOT_LISTENING = "NOT_LISTENING";
            public const string TOO_MANY_AUTH_ATTEMPTS = "TOO_MANY_AUTH_ATTEMPTS";
            public const string IS_CLOSED = "IS_CLOSED";
            public const string UNKNOWN_CALLEE = "UNKNOWN_CALLEE";
        }
    }
}