using DeepStreamNet.Tests.Helper;
using System;
using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection(TestConstants.ServerCommunication)]
    public class AnonymousRecordTests : IClassFixture<DeepStreamServerFixture>
    {
        public AnonymousRecordTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [FactWithSkipOnCloudBuilds]
        public async Task SetNameNullTest()
        {
            using var client = await TestHelper.GetClientAsync();
            var record = client.Records.GetAnonymousRecord();
            await Assert.ThrowsAsync<ArgumentNullException>("name", () => record.SetNameAsync(null));
        }

        [FactWithSkipOnCloudBuilds]
        public async Task SetNameTest()
        {
            using var client = await TestHelper.GetClientAsync();
            var record = client.Records.GetAnonymousRecord();
            await record.SetNameAsync("arecord");
            Assert.Equal("arecord", record.RecordName);
        }

        [FactWithSkipOnCloudBuilds]
        public async Task SetNameRecordChangeTest()
        {
            using var client = await TestHelper.GetClientAsync();
            var record1 = await client.Records.GetRecordAsync("record1");
            var record2 = await client.Records.GetRecordAsync("record2");

            record1["prop"] = "record1_value";
            record2["prop"] = "record2_value";

            var record = client.Records.GetAnonymousRecord();
            await record.SetNameAsync("record1");
            Assert.Equal("record1", record.RecordName);
            Assert.Equal("record1_value", record["prop"]);

            await record.SetNameAsync("record2");
            Assert.Equal("record2", record.RecordName);
            Assert.Equal("record2_value", record["prop"]);

            //cleanup
            await client.Records.DeleteAsync(record1);
            await client.Records.DeleteAsync(record2);
        }
    }
}
