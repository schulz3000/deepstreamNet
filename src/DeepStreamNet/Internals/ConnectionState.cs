using System;

namespace DeepStreamNet
{
    enum ConnectionState
    {
        NONE = 0,
        AWAITING_AUTHENTICATION,
        AUTHENTICATING,
        OPEN,
        ERROR,
        RECONNECTING
    }
}