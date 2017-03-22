using Newtonsoft.Json;

namespace DeepStreamNet
{
    static class Constants
    {
        internal static readonly char RecordSeperator = (char)31 ;
        internal static readonly char GroupSeperator =  (char)30 ;

        internal static readonly char[] PathSplitter = { '.' };

        internal static readonly string WriteSuccessIdentifier = JsonConvert.SerializeObject(new { writeSuccess = true });

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
            public const string CONNECTION_ERROR = "connectionError";
            public const string CONNECTION_STATE_CHANGED = "connectionStateChanged";
            public const string MAX_RECONNECTION_ATTEMPTS_REACHED = nameof(MAX_RECONNECTION_ATTEMPTS_REACHED);
            public const string CONNECTION_AUTHENTICATION_TIMEOUT = nameof(CONNECTION_AUTHENTICATION_TIMEOUT);
            public const string ACK_TIMEOUT = nameof(ACK_TIMEOUT);
            public const string NO_RPC_PROVIDER = nameof(NO_RPC_PROVIDER);
            public const string RESPONSE_TIMEOUT = nameof(RESPONSE_TIMEOUT);
            public const string DELETE_TIMEOUT = nameof(DELETE_TIMEOUT);
            public const string UNSOLICITED_MESSAGE = nameof(UNSOLICITED_MESSAGE);
            public const string MESSAGE_DENIED = nameof(MESSAGE_DENIED);
            public const string MESSAGE_PARSE_ERROR = nameof(MESSAGE_PARSE_ERROR);
            public const string VERSION_EXISTS = nameof(VERSION_EXISTS);
            public const string NOT_AUTHENTICATED = nameof(NOT_AUTHENTICATED);
            public const string MESSAGE_PERMISSION_ERROR = nameof(MESSAGE_PERMISSION_ERROR);
            public const string LISTENER_EXISTS = nameof(LISTENER_EXISTS);
            public const string NOT_LISTENING = nameof(NOT_LISTENING);
            public const string TOO_MANY_AUTH_ATTEMPTS = nameof(TOO_MANY_AUTH_ATTEMPTS);
            public const string IS_CLOSED = nameof(IS_CLOSED);
            public const string UNKNOWN_CALLEE = nameof(UNKNOWN_CALLEE);
            public const string RECORD_NOT_FOUND = nameof(RECORD_NOT_FOUND);
        }
    }
}