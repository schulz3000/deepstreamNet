using DeepStreamNet.Tests.Helper;
using Newtonsoft.Json.Linq;
using System;
using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection(TestConstants.ServerCommunication)]
    public class IssueTest : IClassFixture<DeepStreamServerFixture>
    {
        public IssueTest(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [Fact(Skip = "issue test")]
        public async Task TestDataProvider()
        {
            const string key = "test/datap/blah";
            const string listenKey = "test/datap/*";
            var val = Guid.NewGuid().ToString();

            // test a data provider

            using var client = await TestHelper.GetClientAsync();// GetConnection("chris", "test"))
            using var server = await TestHelper.GetClientAsync();//GetConnection("admin", "test"))
                                                                 // create a listener
            await server.Records.ListenAsync(listenKey, async (match, _, resposne) =>
            {
                Assert.Equal(key, match);
                // update the record
                var rec = await server.Records.GetRecordAsync(match);
                var obj = new JObject();
                obj["data"] = val;
                var result = await server.Records.SetWithAckAsync(rec, obj);
                if (result)
                {
                    resposne.Accept();
                }
                else
                {
                    resposne.Reject();
                }
            });

            // get the record, should trigger the data listener
            // to update the object
            var recsub = await client.Records.GetRecordAsync(key);
            int changes = 0;
            // subscribe to record changes
            recsub.PropertyChanged += (sender, args) =>
            {
                // never gets fired
                changes++;
            };

            // wait for a change to be triggered
            while (changes == 0)
            {
                await Task.Delay(500);// System.Threading.Thread.Sleep(300);
            }
            // never gets here
            Assert.Equal(val, recsub["val"].ToString());
        }

        [Fact(Skip = "issue test")]
        public async Task TestPububOnOneConnectons()
        {
            const string key = "test";
            var tcs = new TaskCompletionSource<string>();
            var c = await TestHelper.GetClientAsync(); //GetConnection("admin", "test");

            await c.Events.SubscribeAsync(key, _ =>
            {
                // never gets here
                tcs.SetResult("done");
                // assert it is the same object

            });

            await Task.Delay(500);

            c.Events.Publish(key, "Hello World");

            await tcs.Task;
            c.Dispose();
        }
    }
}
