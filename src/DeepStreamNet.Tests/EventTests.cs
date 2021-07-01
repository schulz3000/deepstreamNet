using DeepStreamNet.Tests.Helper;
using System;
using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection(TestConstants.ServerCommunication)]
    public class EventTests : IClassFixture<DeepStreamServerFixture>
    {
        public EventTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [FactWithSkipOnCloudBuilds]
        public async Task PublishSubscribeTest()
        {
            using var client1 = await TestHelper.GetClientAsync();
            var sub = await client1.Events.SubscribeAsync("unittest", x => Assert.Equal("msg", x));

            using (var client2 = await TestHelper.GetClientAsync())
            {
                client2.Events.Publish("unittest", "msg");
            }

            await Task.Delay(500);

            await sub.DisposeAsync();
        }

        [FactWithSkipOnCloudBuilds]
        public async Task PublishSubscribeOnSameConnectionTest()
        {
            using var client = await TestHelper.GetClientAsync();
            var sub = await client.Events.SubscribeAsync("unittest", x => Assert.Equal("msg", x));

            await Task.Delay(500);

            client.Events.Publish("unittest", "msg");

            await Task.Delay(500);

            await sub.DisposeAsync();
        }

        [FactWithSkipOnCloudBuilds]
        public async Task EventNameNullTest()
        {
            using var client = await TestHelper.GetClientAsync();
            Assert.Throws<ArgumentNullException>("eventName", () => client.Events.Publish(null, "test"));
        }

        [FactWithSkipOnCloudBuilds]
        public async Task ListenSubscribeUnsubscribeTest()
        {
            using var client1 = await TestHelper.GetClientAsync();
            using var client2 = await TestHelper.GetClientAsync();
            await client1.Events.ListenAsync("test/*", (_, isSubscribed, response) =>
            {
                if (isSubscribed)
                {
                    response.Accept();
                    client1.Events.Publish("test/hello", "world");
                }
                else
                {
                    Assert.True(true);
                }
            });

            var sub = await client2.Events.SubscribeAsync("test/hello", result => Assert.Equal("world", result));

            await Task.Delay(500);

            await sub.DisposeAsync();
        }
    }
}
