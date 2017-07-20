using DeepStreamNet.Tests.Helper;
using System;
using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection("ServerCommunication")]
    [TestCaseOrderer("DeepStreamNet.Tests.Helper.TestCollectionOrderer", "DeepStreamNet.Tests")]
    public class RecordTests : IClassFixture<DeepStreamServerFixture>
    {
        static readonly string RecordSessionName = Guid.NewGuid().ToString();

        public RecordTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [Fact, TestPriority(1)]
        public async Task HasRecordFalseTest()
        {
            using (var client = await TestHelper.GetClient())
            {
                var has = await client.Records.HasAsync(RecordSessionName);

                Assert.Equal(false, has);
            }
        }

        [Fact, TestPriority(2)]
        public async Task RecordNameTest()
        {
            using (var client = await TestHelper.GetClient())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);

                Assert.Equal(RecordSessionName, record.RecordName);
            }
        }

        [Fact, TestPriority(3)]
        public async Task HasRecordTrueTest()
        {
            using (var client = await TestHelper.GetClient())
            {
                var has = await client.Records.HasAsync(RecordSessionName);

                Assert.Equal(true, has);
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

        [Fact, TestPriority(5)]
        public async Task RecordSetLocalTest()
        {
            using (var client = await TestHelper.GetClient())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                record["property1"] = "test";
                Assert.Equal("test", record["property1"]);
            }
        }

        [Fact, TestPriority(6)]
        public async Task RecordSetRemoteTest()
        {
            using (var client1 = await TestHelper.GetClient())
            {
                var record = await client1.Records.GetRecordAsync(RecordSessionName);
                record["property1"] = "test2";

                using (var client2 = await TestHelper.GetClient())
                {
                    var record2 = await client2.Records.GetRecordAsync(RecordSessionName);
                    Assert.Equal("test2", record2["property1"]);
                }
            }
        }

        [Fact, TestPriority(7)]
        public async Task RecordDeleteTest()
        {
            using (var client = await TestHelper.GetClient())
            {
                var record = await client.Records.GetRecordAsync(RecordSessionName);
                await client.Records.DeleteAsync(record);
                var has = await client.Records.HasAsync(RecordSessionName);
                Assert.Equal(false, has);
            }
        }
    }
}
