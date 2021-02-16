using DeepStreamNet.Tests.Helper;
using System;
using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection(TestConstants.ServerCommunication)]
    [TestCaseOrderer("DeepStreamNet.Tests.Helper.TestCollectionOrderer", "DeepStreamNet.Tests")]
    public class RecordTests : IClassFixture<DeepStreamServerFixture>
    {
        static readonly string RecordSessionName = Guid.NewGuid().ToString();

        public RecordTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [IgnoreOnCloudBuilds]
        public async Task GetRecordNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                await Assert.ThrowsAsync<ArgumentNullException>("name", () => client.Records.GetRecordAsync(null));
            }
        }

        [IgnoreOnCloudBuilds]
        public async Task HasRecordRecordNameNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                await Assert.ThrowsAsync<ArgumentNullException>("recordName", () => client.Records.HasAsync(null));
            }
        }

        [IgnoreOnCloudBuilds]
        public async Task SetRecordRecordNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                Assert.Throws<ArgumentNullException>("record", () => client.Records.Set(null, new { property = "abc" }));
            }
        }

        [IgnoreOnCloudBuilds]
        public async Task SetRecordWithPathRecordNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                Assert.Throws<ArgumentNullException>("record", () => client.Records.Set(null, "path1", new { property = "abc" }));
            }
        }

        [IgnoreOnCloudBuilds]
        public async Task SetRecordWithAckRecordNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                await Assert.ThrowsAsync<ArgumentNullException>("record", () => client.Records.SetWithAckAsync(null, new { property = "abc" }));
            }
        }

        [IgnoreOnCloudBuilds]
        public async Task GetSnapshotRecordNameNulllTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                await Assert.ThrowsAsync<ArgumentNullException>("recordName", () => client.Records.SnapshotAsync(null));
            }
        }

        [IgnoreOnCloudBuilds]
        public async Task RecordDiscardNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                await Assert.ThrowsAsync<ArgumentNullException>("record", () => client.Records.DiscardAsync(null));
            }
        }

        [IgnoreOnCloudBuilds]
        public async Task RecordDeleteNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                await Assert.ThrowsAsync<ArgumentNullException>("record", () => client.Records.DeleteAsync(null));
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(1)]
        public async Task HasRecordFalseTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var has = await client.Records.HasAsync(RecordSessionName);

                Assert.False(has);
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(2)]
        public async Task RecordNameTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);

                Assert.Equal(RecordSessionName, record.RecordName);
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(3)]
        public async Task HasRecordTrueTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var has = await client.Records.HasAsync(RecordSessionName);

                Assert.True(has);
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(4)]
        public async Task SetRecordWithAckContentNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                await Assert.ThrowsAsync<ArgumentNullException>("item", () => client.Records.SetWithAckAsync(record, null));
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(5)]
        public async Task SetRecordContentNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                Assert.Throws<ArgumentNullException>("item", () => client.Records.Set(record, null));
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(6)]
        public async Task SetRecordWithPathContentNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                Assert.Throws<ArgumentNullException>("item", () => client.Records.Set(record, "path1", null));
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(7)]
        public async Task SetRecordPathNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                Assert.Throws<ArgumentNullException>("path", () => client.Records.Set(record, null, new { property = "abc" }));
            }
        }

        //[Fact, TestPriority(4)]
        //public async Task RecordEmptyTest()
        //{
        //    using (var client = await TestHelper.GetClient())
        //    {
        //        var record = await client.Records.GetRecordAsync(RecordSessionName);

        //        Assert.Equal(null, record["property1"]);
        //    }
        //}

        [IgnoreOnCloudBuilds, TestPriority(8)]
        public async Task RecordSetLocalTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                record["property1"] = "test";
                Assert.Equal("test", record["property1"]);
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(9)]
        public async Task RecordSetRemoteTest()
        {
            using (var client1 = await TestHelper.GetClientAsync())
            {
                var record = await client1.Records.GetRecordAsync(RecordSessionName);
                record["property1"] = "test2";

                using (var client2 = await TestHelper.GetClientAsync())
                {
                    var record2 = await client2.Records.GetRecordAsync(RecordSessionName);
                    Assert.Equal("test2", record2["property1"]);
                }
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(10)]
        public async Task GetSnapshotTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.SnapshotAsync(RecordSessionName);
                Assert.Equal("test2", record["property1"]);
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(11)]
        public async Task RecordSetWithAckTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                var result = await client.Records.SetWithAckAsync(record, new { property3 = "test3" });

                Assert.True(result);
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(12)]
        public async Task RecordSetWithAckPathTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                await client.Records.SetWithAckAsync(record, new { property4 = new { property44 = "test" } });
                var result = await client.Records.SetWithAckAsync(record, "property4.property44", "change");

                Assert.True(result);
                Assert.Equal("change", record["property4"]["property44"].ToString());
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(13)]
        public async Task RecordDiscardTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                await client.Records.DiscardAsync(record);

                Assert.True(true);//how to test??
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(14)]
        public async Task RecordRemoteChangeEventTest()
        {
            using (var client1 = await TestHelper.GetClientAsync())
            {
                var record1 = await client1.Records.GetRecordAsync(RecordSessionName);
                using (var client2 = await TestHelper.GetClientAsync())
                {
                    var record2 = await client2.Records.GetRecordAsync(RecordSessionName);
                    string propertyName = null;
                    record2.PropertyChanged += (s, e) => propertyName = e.PropertyName;

                    record1["property3"] = "testchange";

                    await Task.Delay(500);

                    Assert.Equal("property3", propertyName);
                }
            }
        }

        [IgnoreOnCloudBuilds, TestPriority(15)]
        public async Task RecordDeleteTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                await client.Records.DeleteAsync(record);
                var has = await client.Records.HasAsync(RecordSessionName);
                Assert.False(has);
            }
        }
    }
}
