using DeepStreamNet.Contracts;
using DeepStreamNet.Records;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    class DeepStreamRecords : DeepStreamBase, IDeepStreamRecords
    {
        readonly HashSet<IDeepStreamRecordWrapper> records = new HashSet<IDeepStreamRecordWrapper>(new DeepStreamRecordComparer());
        readonly HashSet<DeepStreamList> lists = new HashSet<DeepStreamList>();

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

            var listener = (record as DeepStreamRecord).Listener;
            listener.PropertyChanged -= Listener_PropertyChanged;

            var data = (KeyValuePair<Type, object>)e.Data;

            var path = e.Property.Split(Constants.PathSplitter, StringSplitOptions.RemoveEmptyEntries);

            if (path.Length == 1)
                record[e.Property] = data.Value;
            else
            {
                var item = record as IDeepStreamRecord;
                var max = path.Length - 1;
                var index = -1;
                for (int i = 0; i <= max; i++)
                {
                    if (i == max)
                    {
                        item[path[i]] = data.Value;
                    }
                    else if (int.TryParse(path[i + 1], out index))
                    {
                        item = (item[path[i]] as object[])[index] as IDeepStreamRecord;
                        i++;
                    }
                    else
                    {
                        item = item[path[i]] as IDeepStreamRecord;
                    }
                }
            }

            listener.PropertyChanged += Listener_PropertyChanged;
        }

        void Con_RecordUpdated(object sender, RecordUpdatedArgs e)
        {
            var record = records.FirstOrDefault(f => f.RecordName == e.Identifier);
            if (record == null)
                return;

            var listener = (record as DeepStreamRecord).Listener;
            listener.PropertyChanged -= Listener_PropertyChanged;

            //TODO implement full update

            listener.PropertyChanged += Listener_PropertyChanged;
        }

        public async Task<IDeepStreamRecord> GetRecordAsync(string name)
        {
            var record = records.FirstOrDefault(f => f.RecordName == name);
            if (record != null)
                return record;

            var result = await InnerGetRecord(name).ConfigureAwait(false);

            records.Add(result);

            result.Listener.PropertyChanged += Listener_PropertyChanged;

            return result;
        }

        public Task<DeepStreamList> GetListAsync(string name)
        {
            var list = lists.FirstOrDefault(f => f.ListName == name);
            if (list != null)
                return Task.FromResult(list);

            list = new DeepStreamList(name);

            lists.Add(list);

            return Task.FromResult(list);

        }

        async void Listener_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            var record = (sender as ChildChangeListener)?.Value as DeepStreamRecord;

            if (record == null)
                return;

            var changedData = Utils.GetValueByPath(e.PropertyName, record);

            record.IncrementVersion();

            var command = Utils.BuildCommand(Topic.RECORD, Action.PATCH, record.RecordName, record.RecordVersion, e.PropertyName, Utils.ConvertAndPrefixData(changedData));

            await Connection.SendAsync(command).ConfigureAwait(false);
        }

        public Task SaveAsync(IDeepStreamRecord record)
        {
            if (!IsRecordTracked(record))
                throw new DeepStreamException("Record not tracked");

            var wrapper = record as DeepStreamRecord;

            wrapper.IncrementVersion();

            var command = Utils.BuildCommand(Topic.RECORD, Action.UPDATE, wrapper.RecordName, wrapper.RecordVersion, JsonConvert.SerializeObject(record));
            return Connection.SendAsync(command);
        }

        public async Task DiscardAsync(IDeepStreamRecord record)
        {
            if (!IsRecordTracked(record))
                throw new DeepStreamException("Record not tracked");

            var wrapper = record as IDeepStreamRecordWrapper;

            if (await Connection.SendWithAckAsync(Topic.RECORD, Action.UNSUBSCRIBE, Action.UNSUBSCRIBE, wrapper.RecordName, Options.SubscriptionTimeout).ConfigureAwait(false))
            {
                records.Remove(wrapper);
            }
        }

        public async Task DeleteAsync(IDeepStreamRecord record)
        {
            if (!IsRecordTracked(record))
                throw new DeepStreamException("Record not tracked");

            var wrapper = record as IDeepStreamRecordWrapper;

            if (await Connection.SendWithAckAsync(Topic.RECORD, Action.DELETE, Action.DELETE, wrapper.RecordName, Options.RecordDeleteTimeout).ConfigureAwait(false))
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

            isAck = await Connection.SendWithAckAsync(topic, Action.CREATEORREAD, Action.SUBSCRIBE, identifier, Options.RecordReadAckTimeout).ConfigureAwait(false);

            return await tcs.Task.ConfigureAwait(false);
        }

        bool IsRecordTracked(IDeepStreamRecord record)
        {
            return records.Contains(record);
        }
    }
}