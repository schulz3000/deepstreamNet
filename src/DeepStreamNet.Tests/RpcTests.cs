using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection("ServerCommunication")]
    public class RpcTests : IClassFixture<DeepStreamServerFixture>
    {
        public RpcTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [Fact]
        public async Task RequestTest()
        {
            using (var client1 = await TestHelper.GetClient())
            {
                await client1.Rpcs.RegisterProviderAsync<string, string>("unittestrpc", (input, rpc) => rpc.Send(input.ToUpper()));

                using (var client2 = await TestHelper.GetClient())
                {
                    var result = await client2.Rpcs.MakeRequest<string, string>("unittestrpc", "abc");

                    Assert.Equal("ABC", result);
                }
            }
        }

        [Fact]
        public async Task SubscribeTest()
        {
            using (var client1 = await TestHelper.GetClient())
            {
                await client1.Rpcs.RegisterProviderAsync<string, string>("unittestrpc", (input, rpc) =>
            {
                Assert.Equal("abc", input);

                rpc.Send(input.ToUpper());
            });

                using (var client2 = await TestHelper.GetClient())
                {
                    var result = await client2.Rpcs.MakeRequest<string, string>("unittestrpc", "abc");
                }
            }
        }

        [Fact]
        public async Task ErrorTest()
        {
            using (var client1 = await TestHelper.GetClient())
            {
                await client1.Rpcs.RegisterProviderAsync<string, string>("unittestrpc", (input, rpc) => rpc.Error("unittest-error"));

                using (var client2 = await TestHelper.GetClient())
                {
                    await Assert.ThrowsAsync<DeepStreamException>(() => client2.Rpcs.MakeRequest<string, string>("unittestrpc", "abc"));
                }
            }
        }

        [Fact]
        public async Task RejectTest()
        {
            using (var client1 = await TestHelper.GetClient())
            {
                await client1.Rpcs.RegisterProviderAsync<string, string>("unittestrpc", (input, rpc) => rpc.Reject());

                using (var client2 = await TestHelper.GetClient())
                {
                    await Assert.ThrowsAsync<DeepStreamException>(() => client2.Rpcs.MakeRequest<string, string>("unittestrpc", "abc"));
                }
            }
        }
    }
}
