using DeepStreamNet.Tests.Helper;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection(TestConstants.ServerCommunication)]
    [TestCaseOrderer("DeepStreamNet.Tests.Helper.TestCollectionOrderer", "DeepStreamNet.Tests")]
    public class ListTests : IClassFixture<DeepStreamServerFixture>
    {
        public ListTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [Fact]
        public async Task NameNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                await Assert.ThrowsAsync<ArgumentNullException>("name", () => client.Records.GetListAsync(null));
            }
        }

        [Fact, TestPriority(1)]
        public async Task NameTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                Assert.Equal("list1", list.ListName);
            }
        }

        [Fact, TestPriority(2)]
        public async Task AddTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                list.Add("item1");
                list.Add("item2");

                Assert.Equal(2, list.Count);
            }
        }

        [Fact, TestPriority(3)]
        public async Task RemoveTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                list.Remove("item1");

                Assert.Equal(1, list.Count);
            }
        }

        [Fact, TestPriority(4)]
        public async Task ContainsTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                Assert.True(list.Contains("item2"));
            }
        }

        [Fact, TestPriority(5)]
        public async Task ClearTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                list.Clear();

                Assert.Equal(0, list.Count);
            }
        }

        [Fact, TestPriority(6)]
        public async Task TestAddingARecordToListAndListenToChanges()
        {
            var name = Guid.NewGuid().ToString();
            var key = Guid.NewGuid().ToString();
            var listKey = Guid.NewGuid().ToString();
            using (var updateClient = await TestHelper.GetClientAsync())
            {
                var list = await updateClient.Records.GetListAsync(listKey);

                var rec = await updateClient.Records.GetRecordAsync(key);
                var res = await updateClient.Records.SetWithAckAsync(rec, "name", name);
                Assert.True(res);
                using (var readClient = await TestHelper.GetClientAsync())
                {
                    var listRead = await readClient.Records.GetListAsync(listKey);
                    var changes = new List<NotifyCollectionChangedEventArgs>();
                    listRead.CollectionChanged += (sender, args) => changes.Add(args);
                    list.Add(rec.RecordName);
                    int time = 0;
                    while (changes.Count == 0)
                    {
                        await Task.Delay(200);
                        time += 200;
                        if (time > 4000)
                        {
                            Assert.True(false, "Time out should have got changes");
                        }
                    }
                    Assert.True(changes.Count(e => e.Action == NotifyCollectionChangedAction.Add) == 1);
                }
            }
        }
    }
}
