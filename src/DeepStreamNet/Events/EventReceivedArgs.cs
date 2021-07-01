using System;

namespace DeepStreamNet
{
    internal class EventReceivedArgs : EventArgs
    {
        public string EventName { get; }

        public Type DataType { get; }

        public object Data { get; }

        public EventReceivedArgs(string eventName, Type type, object data)
        {
            EventName = eventName;
            DataType = type;
            Data = data;
        }
    }
}