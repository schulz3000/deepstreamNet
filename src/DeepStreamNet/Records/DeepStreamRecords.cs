using DeepStreamNet.Contracts;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;

namespace DeepStreamNet
{
    class DeepStreamRecords : DeepStreamBase, IDeepStreamRecords
    {
        readonly HashSet<IDeepStreamRecordWrapper> records = new HashSet<IDeepStreamRecordWrapper>(new DeepStreamRecordComparer());
        readonly HashSet<IDeepStreamListWrapper> lists = new HashSet<IDeepStreamListWrapper>();
        readonly Dictionary<string, Delegate> listenerDict = new Dictionary<string, Delegate>();

        public DeepStreamRecords(Connection connection, DeepStreamOptions options)
            : base(connection, options)
        {
            connection.RecordUpdated += Con_RecordUpdated;
            connection.RecordPatched += Con_RecordPatched;
            connection.RecordListenerChanged += Connection_RecordListenerChanged;
        }

        void Con_RecordPatched(object sender, RecordPatchedArgs e)
        {
            var record = records.FirstOrDefault(f => f.RecordName == e.Identifier);
            if (record == null)
                return;

            record.PropertyChanged -= Record_PropertyChanged;
            record.Patch(e.Property, e.Data);
            record.PropertyChanged += Record_PropertyChanged;
        }

        void Con_RecordUpdated(object sender, RecordUpdatedArgs e)
        {
            var record = records.FirstOrDefault(f => f.RecordName == e.Identifier);
            if (record == null)
                return;

            record.PropertyChanged -= Record_PropertyChanged;
            record.Update(e.Data);
            record.PropertyChanged += Record_PropertyChanged;

            var list = lists.FirstOrDefault(f => f.ListName == e.Identifier);
            if (list != null)
            {
                list.CollectionChanged -= List_CollectionChanged;
                list.Update(e.Data);
                list.CollectionChanged += List_CollectionChanged;
            }
        }

        async void Connection_RecordListenerChanged(object sender, RecordListenerChangedEventArgs e)
        {
            if (!listenerDict.ContainsKey(e.Pattern))
                return;

            var listener = listenerDict[e.Pattern];

            var result = listener.DynamicInvoke(e.Name, e.ListenerState == ListenerState.Add, new RecordListenerResponse(e.Pattern, e.Name, Connection));
            if (result != null)
                await (Task)result;
        }

        public async Task<IDeepStreamRecord> GetRecordAsync(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentNullException(nameof(name));

            var record = records.FirstOrDefault(f => f.RecordName == name);
            if (record != null)
                return record;

            var result = await InnerGetRecordAsync<JObject>(name).ConfigureAwait(false);

            records.Add(result);

            result.PropertyChanged += Record_PropertyChanged;

            return result;
        }

        public async Task<IDeepStreamList> GetListAsync(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentNullException(nameof(name));

            var list = lists.FirstOrDefault(f => f.ListName == name);
            if (list != null)
                return list;

            var innerRecord = await InnerGetRecordAsync<JArray>(name).ConfigureAwait(false);

            records.Add(innerRecord);

            innerRecord.PropertyChanged += Record_PropertyChanged;

            list = new DeepStreamList(name, JToken.FromObject(innerRecord).ToObject<List<string>>());

            list.CollectionChanged += List_CollectionChanged;

            lists.Add(list);
            return list;
        }

        void List_CollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            var list = sender as DeepStreamList;

            var record = records.FirstOrDefault(f => f.RecordName == list.ListName);
            if (record == null)
                return;

            Set(record, list.ToArray());
        }

        void Record_PropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            var recordName = (sender as JToken).Annotation<string>();
            var record = records.FirstOrDefault(f => f.RecordName == recordName);
            if (record == null)
                return;

            var changedData = record.Get(e.PropertyName);

            record.IncrementVersion();

            var command = Utils.BuildCommand(Topic.RECORD, Action.PATCH, record.RecordName, record.RecordVersion, e.PropertyName, Utils.ConvertAndPrefixData(changedData));

            Connection.Send(command);
        }

        public async Task DiscardAsync(IDeepStreamRecord record)
        {
            if (record == null)
                throw new ArgumentNullException(nameof(record));

            if (!IsRecordTracked(record))
                throw new DeepStreamException("Record not tracked");

            var wrapper = record as IDeepStreamRecordWrapper;

            record.PropertyChanged -= Record_PropertyChanged;

            if (await Connection.SendWithAckAsync(Topic.RECORD, Action.UNSUBSCRIBE, Action.UNSUBSCRIBE, wrapper.RecordName, Options.SubscriptionTimeout).ConfigureAwait(false))
            {
                records.Remove(wrapper);
                ((IDisposable)wrapper).Dispose();
            }
        }

        public async Task DeleteAsync(IDeepStreamRecord record)
        {
            if (record == null)
                throw new ArgumentNullException(nameof(record));

            if (!IsRecordTracked(record))
                throw new DeepStreamException("Record not tracked");

            var wrapper = record as IDeepStreamRecordWrapper;

            record.PropertyChanged -= Record_PropertyChanged;

            if (await Connection.SendWithAckAsync(Topic.RECORD, Action.DELETE, Action.DELETE, wrapper.RecordName, Options.RecordDeleteTimeout).ConfigureAwait(false))
            {
                records.Remove(wrapper);
                ((IDisposable)wrapper).Dispose();
            }
        }

        public Task<bool> HasAsync(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentNullException(nameof(name));

            var tcs = new TaskCompletionSource<bool>();

            EventHandler<HasRecordArgs> handler = null;
            handler = (s, e) =>
            {
                if (e.Topic == Topic.RECORD && e.Action == Action.HAS && e.Name == name)
                {
                    Connection.HasRecordReceived -= handler;
                    tcs.TrySetResult(e.Result);
                }
            };

            Connection.HasRecordReceived += handler;

            Connection.Send(Utils.BuildCommand(Topic.RECORD, Action.HAS, name));

            return tcs.Task;
        }

        public async Task<IDeepStreamRecord> SnapshotAsync(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentNullException(nameof(name));

            var tcs = new TaskCompletionSource<IDeepStreamRecordWrapper>();

            var topic = Topic.RECORD;

            EventHandler<RecordReceivedArgs> recHandler = null;
            recHandler = (s, e) =>
            {
                if (e.Topic == topic && e.Action == Action.READ && e.Identifier == name)
                {
                    Connection.RecordReceived -= recHandler;

                    var data = e.Data;
                    if (e.Data.Type == JTokenType.Array)
                    {
                        if (!e.Data.HasValues)
                            data = JToken.Parse("[]");
                        tcs.TrySetResult(new DeepStreamRecordArray(e.Identifier, e.Version, (JArray)data));
                    }
                    else
                    {
                        tcs.TrySetResult(new DeepStreamRecordObject(e.Identifier, e.Version, (JObject)e.Data));
                    }
                }
            };

            Connection.RecordReceived += recHandler;

            Connection.Send(Utils.BuildCommand(topic, Action.SNAPSHOT, name));

            return (await tcs.Task) as IDeepStreamRecord;
        }

        public IDeepStreamAnonymousRecord GetAnonymousRecord()
        {
            return new DeepStreamAnonymousRecord(this);
        }

        public Task<IAsyncDisposable> ListenAsync(string pattern, Action<string, bool, IListenerResponse> listener)
        {
            return InnerListenAsync(pattern, listener);
        }

        public Task<IAsyncDisposable> ListenAsync(string pattern, Func<string, bool, IListenerResponse, Task> listener)
        {
            return InnerListenAsync(pattern, listener);
        }

        async Task<IAsyncDisposable> InnerListenAsync(string pattern, Delegate listener)
        {
            if (string.IsNullOrWhiteSpace(pattern))
                throw new ArgumentNullException(nameof(pattern));

            ThrowIfConnectionNotOpened();

            if (listenerDict.ContainsKey(pattern))
                throw new DeepStreamException("we already listen for " + pattern);

            if (!listenerDict.ContainsKey(pattern) && await Connection.SendWithAckAsync(Topic.RECORD, Action.LISTEN, Action.LISTEN, pattern, Options.SubscriptionTimeout).ConfigureAwait(false))
            {
                listenerDict.Add(pattern, listener);
            }

            return new AsyncDisposableAction(async () =>
            {
                if (await Connection.SendWithAckAsync(Topic.RECORD, Action.UNLISTEN, Action.UNLISTEN, pattern, Options.SubscriptionTimeout).ConfigureAwait(false))
                {
                    listenerDict.Remove(pattern);
                }
            });
        }

        async Task<IDeepStreamRecordWrapper> InnerGetRecordAsync<T>(string identifier)
            where T : JContainer
        {
            var tcs = new TaskCompletionSource<IDeepStreamRecordWrapper>();

            var topic = Topic.RECORD;
            var isAck = false;

            EventHandler<RecordReceivedArgs> recHandler = null;
            recHandler = (s, e) =>
            {
                if (isAck && e.Topic == topic && e.Action == Action.READ && e.Identifier == identifier)
                {
                    Connection.RecordReceived -= recHandler;
                    if (typeof(T) == typeof(JArray))
                    {
                        var data = e.Data;
                        if (e.Data.Type != JTokenType.Array && !e.Data.HasValues)
                            data = JToken.Parse("[]");
                        tcs.TrySetResult(new DeepStreamRecordArray(e.Identifier, e.Version, (JArray)data));
                    }
                    else
                    {
                        tcs.TrySetResult(new DeepStreamRecordObject(e.Identifier, e.Version, (JObject)e.Data));
                    }
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

        public void Set(IDeepStreamRecord record, object item)
        {
            if (record == null)
                throw new ArgumentNullException(nameof(record));

            if (item == null)
                throw new ArgumentNullException(nameof(item));

            var result = JToken.FromObject(item);

            if (result.Type != JTokenType.Object && result.Type != JTokenType.Array)
                throw new DeepStreamException($"{nameof(item)} must be a class or an array");

            var wrapper = record as IDeepStreamRecordWrapper;

            wrapper.Update(result);

            wrapper.IncrementVersion();

            var command = Utils.BuildCommand(Topic.RECORD, Action.UPDATE, wrapper.RecordName, wrapper.RecordVersion, JsonConvert.SerializeObject(wrapper));
            Connection.Send(command);
        }

        public void Set(IDeepStreamRecord record, string path, object item)
        {
            if (record == null)
                throw new ArgumentNullException(nameof(record));

            if (string.IsNullOrWhiteSpace(path))
                throw new ArgumentNullException(nameof(path));

            if (item == null)
                throw new ArgumentNullException(nameof(item));

            var wrapper = record as IDeepStreamRecordWrapper;

            wrapper.Patch(path, JToken.FromObject(item));
        }

        public Task<bool> SetWithAckAsync(IDeepStreamRecord record, object item)
        {
            if (record == null)
                throw new ArgumentNullException(nameof(record));

            if (item == null)
                throw new ArgumentNullException(nameof(item));

            var wrapper = record as IDeepStreamRecordWrapper;

            record.PropertyChanged -= Record_PropertyChanged;
            wrapper.Update(JToken.FromObject(item));
            record.PropertyChanged += Record_PropertyChanged;

            wrapper.IncrementVersion();

            return Connection.SendWithAckAsync(Topic.RECORD, Action.UPDATE, Action.WRITE_ACKNOWLEDGEMENT, record.RecordName, Options.RecordReadAckTimeout, wrapper.RecordVersion.ToString(), JsonConvert.SerializeObject(record), Constants.WriteSuccessIdentifier);
        }

        public Task<bool> SetWithAckAsync(IDeepStreamRecord record, string path, object item)
        {
            if (record == null)
                throw new ArgumentNullException(nameof(record));

            if (string.IsNullOrWhiteSpace(path))
                throw new ArgumentNullException(nameof(path));

            if (item == null)
                throw new ArgumentNullException(nameof(item));

            var wrapper = record as IDeepStreamRecordWrapper;

            record.PropertyChanged -= Record_PropertyChanged;
            wrapper.Patch(path, JToken.FromObject(item));
            record.PropertyChanged += Record_PropertyChanged;

            wrapper.IncrementVersion();

            return Connection.SendWithAckAsync(Topic.RECORD, Action.PATCH, Action.WRITE_ACKNOWLEDGEMENT, record.RecordName, Options.RecordReadAckTimeout, wrapper.RecordVersion.ToString(), path, Utils.ConvertAndPrefixData(wrapper.Get(path)), Constants.WriteSuccessIdentifier);
        }
    }
}