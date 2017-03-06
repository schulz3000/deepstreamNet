namespace DeepStreamNet
{
    enum ConnectionState
    {
        NONE = 0,
        CLOSED = 1,
        AWAITING_CONNECTION = 2,
        CHALLENGING = 3,
        AWAITING_AUTHENTICATION = 4,
        AUTHENTICATING = 5,
        OPEN = 6,
        ERROR = 7,
        RECONNECTING = 8
    }
}