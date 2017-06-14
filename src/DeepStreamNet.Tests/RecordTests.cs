using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection("ServerCommunication")]
    public class RecordTests : IClassFixture<DeepStreamServerFixture>
    {
        public RecordTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }
    }
}
