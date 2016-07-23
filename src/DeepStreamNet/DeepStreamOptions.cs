using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    /// <summary>
    /// Options for DeepStreamClient
    /// </summary>
    public class DeepStreamOptions
    {
        /// <summary>
        /// Specifies the number of milliseconds by which the time until
        /// the next reconnection attempt will be incremented after every
        /// unsuccesful attempt.
        /// E.g. for 1500: if the connection is lost, the client will attempt to reconnect
        /// immediatly, if that fails it will try again after 1.5 seconds, if that fails
        /// it will try again after 3 seconds and so on
        /// </summary>
        public int ReconnectIntervalIncrement { get; set; } = 4000;


        /// <summary>
        /// The number of reconnection attempts until the client gives
        /// up and declares the connection closed
        /// </summary>	 	
        public int MaxReconnectAttempts { get; set; } = 5;

        /// <summary>
        /// The number of milliseconds after which a rpc will create an error if
        /// no Ack-message has been received
        /// </summary>	 
        public int RpcAckTimeout { get; set; } = 6000;

        /// <summary>
        /// The number of milliseconds after which a rpc will create an error if
        /// no response-message has been received 
        /// </summary>        
        public int RpcResponseTimeout { get; set; } = 10000;

        /// <summary>
        /// The number of milliseconds that can pass after providing/unproviding a RPC or subscribing/unsubscribing/
        /// listening to a record before an error is thrown
        /// </summary>
        public int SubscriptionTimeout { get; set; } = 2000;

        /// <summary>
        /// If the implementation tries to send a large number of messages at the same
        /// time, the deepstream client will try to split them into smaller packets and send
        /// these every [timeBetweenSendingQueuedPackages] ms.
        /// This parameter specifies the number of messages after which deepstream sends the
        /// packet and queues the remaining messages. Set to Infinity to turn the feature off.
        /// </summary>       
        public int MaxMessagesPerPacket { get; set; } = 100;

        /// <summary>
        /// Please see description for maxMessagesPerPacket. Sets the time in ms.
        /// </summary>
        public int TimeBetweenSendingQueuedPackages { get; set; } = 16;

        /// <summary>
        /// The number of milliseconds from the moment client.record.getRecord() is called
        /// until an error is thrown since no ack message has been received.
        /// </summary>       
        public int RecordReadAckTimeout { get; set; } = 1000;

        /// <summary>
        /// The number of milliseconds from the moment client.record.getRecord() is called
        /// until an error is thrown since no data has been received.
        /// </summary>
        public int RecordReadTimeout { get; set; } = 3000;

        /// <summary>
        /// The number of milliseconds from the moment record.delete() is called
        /// until an error is thrown since no delete ack message had been received. Please 
        /// take into account that the deletion is only complete after the record has been
        /// deleted from both cache and storage
        /// </summary>
        public int RecordDeleteTimeout { get; set; } = 3000;
    }
}
