using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;
using Jil;

namespace DeepStreamNet
{
    class DeepStreamRecords : DeepStreamBase, IDeepStreamRecords
    {
        readonly HashSet<IDeepStreamRecordWrapper> records = new HashSet<IDeepStreamRecordWrapper>(new DeepStreamRecordComparer());

        public DeepStreamRecords(Connection connection, DeepStreamOptions options)
            : base(connection, options)
        {
            connection.RecordUpdated += Con_RecordUpdated;
            connection.RecordPatched += Con_RecordPatched;
        }

        void Con_RecordPatched(object sender, RecordPatchedArgs e)
        {
            var record = records.FirstOrDefault(f => f.RecordName == e.Identifier);
            if (record == null)
                return;

            foreach (var item in e.Data as Dictionary<string, object>)
            {
                record[item.Key] = item.Value;
            }
        }

        void Con_RecordUpdated(object sender, RecordUpdatedArgs e)
        {
            var record = records.FirstOrDefault(f => f.RecordName == e.Identifier);
            if (record == null)
                return;

            //TODO implement patch
        }

        public async Task<IDeepStreamRecord> GetRecordAsync(string name)
        {
            var record = records.FirstOrDefault(f => f.RecordName == name);
            if (record != null)
                return record;

            var result = await InnerGetRecord(name);

            records.Add(result);
            return result;
        }

        public Task SaveAsync(IDeepStreamRecord record)
        {
            if (!IsRecordTracked(record))
                throw new DeepStreamException("Record not tracked");

            var wrapper = record as IDeepStreamRecordWrapper;

            var command = Utils.BuildCommand(Topic.RECORD, Action.UPDATE, wrapper.RecordName, (wrapper.RecordVersion + 1), JSON.SerializeDynamic(record));
            return Connection.SendAsync(command);
        }

        public async Task DiscardAsync(IDeepStreamRecord record)
        {
            if (!IsRecordTracked(record))
                throw new DeepStreamException("Record not tracked");

            var wrapper = record as IDeepStreamRecordWrapper;

            if (await Connection.SendWithAckAsync(Topic.RECORD, Action.UNSUBSCRIBE, Action.UNSUBSCRIBE, wrapper.RecordName, Options.SubscriptionTimeout))
            {
                records.Remove(wrapper);
            }
        }

        public Task Patch(IDeepStreamRecord record)
        {
            throw new NotImplementedException();
            //var wrapper = record as IDeepStreamRecordWrapper;

            //var command = Utils.BuildCommand(Topic.RECORD,Action.PATCH, wrapper.RecordName, (wrapper.RecordVersion + 1), "firstname", "SMax");
            //return Connection.SendAsync(command);
        }

        public async Task DeleteAsync(IDeepStreamRecord record)
        {
            if (!IsRecordTracked(record))
                throw new DeepStreamException("Record not tracked");

            var wrapper = record as IDeepStreamRecordWrapper;

            if (await Connection.SendWithAckAsync(Topic.RECORD, Action.DELETE, Action.DELETE, wrapper.RecordName, Options.RecordDeleteTimeout))
            {
                records.Remove(wrapper);
            }
        }

        async Task<DeepStreamRecord> InnerGetRecord(string identifier)
        {
            var tcs = new TaskCompletionSource<DeepStreamRecord>();

            var topic = Topic.RECORD;
            var isAck = false;

            EventHandler<RecordReceivedArgs> recHandler = null;

            recHandler = (s, e) =>
            {
                if (isAck && e.Topic == topic && e.Action == Action.READ && e.Identifier == identifier)
                {
                    Connection.RecordReceived -= recHandler;
                    tcs.TrySetResult(new DeepStreamRecord(e.Identifier, e.Version, e.Data as Dictionary<string, object>));
                }
            };

            Connection.RecordReceived += recHandler;

            isAck = await Connection.SendWithAckAsync(topic, Action.CREATEORREAD, Action.SUBSCRIBE, identifier, Options.RecordReadAckTimeout);

            return await tcs.Task;
        }

        bool IsRecordTracked(IDeepStreamRecord record)
        {
            return records.Contains(record);
        }
    }
}