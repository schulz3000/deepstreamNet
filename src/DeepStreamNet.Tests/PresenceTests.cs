using DeepStreamNet.Tests.Helper;
using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection(TestConstants.ServerCommunication)]
    public class PresenceTests : IClassFixture<DeepStreamServerFixture>
    {
        public PresenceTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [IgnoreOnCloudBuilds]
        public async Task GetAllEmptyTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var users = await client.Presence.GetAllAsync();
                Assert.Empty(users);
            }
        }
    }
}
