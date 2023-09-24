using DeepStreamNet.Tests.Helper;
using System;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection(TestConstants.ServerCommunication)]
    public class DeepStreamClientTests : IClassFixture<DeepStreamServerFixture>
    {
        public DeepStreamClientTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [Fact]
        public void NotLoggedInTest()
        {
            using var client = new DeepStreamClient("localhost");
            Assert.Throws<DeepStreamException>(() => client.Events);
            Assert.Throws<DeepStreamException>(() => client.Records);
            Assert.Throws<DeepStreamException>(() => client.Rpcs);
            Assert.Throws<DeepStreamException>(() => client.Presence);
        }

        [Fact]
        public void HostNullOrEmptyTest()
        {
            Assert.Throws<ArgumentNullException>("host", () => new DeepStreamClient(string.Empty));
            Assert.Throws<ArgumentNullException>("host", () => new DeepStreamClient(null));
        }

        [Fact]
        public void WrongPortTest()
        {
            Assert.Throws<ArgumentOutOfRangeException>("port", () => new DeepStreamClient("localhost", -666));
            Assert.Throws<ArgumentOutOfRangeException>("port", () => new DeepStreamClient("localhost", 0));
        }

        [Fact]
        public void PathNullOrEmptyTest()
        {
            Assert.Throws<ArgumentNullException>("path", () => new DeepStreamClient("localhost", path: string.Empty));
            Assert.Throws<ArgumentNullException>("path", () => new DeepStreamClient("localhost", path: null));
        }
    }
}
