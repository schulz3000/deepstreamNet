using DeepStreamNet.Tests.Helper;
using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection("ServerCommunication")]
    public class EventTests : IClassFixture<DeepStreamServerFixture>
    {
        public EventTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [Fact]
        public async Task PublishSubscribeTest()
        {
            using (var client1 = await TestHelper.GetClientAsync())
            {
                var sub = await client1.Events.SubscribeAsync("unittest", x => Assert.Equal("msg", x));

                using (var client2 = await TestHelper.GetClientAsync())
                {
                    client2.Events.Publish("unittest", "msg");
                }

                await Task.Delay(500);

                await sub.DisposeAsync();
            }
        }
    }
}
