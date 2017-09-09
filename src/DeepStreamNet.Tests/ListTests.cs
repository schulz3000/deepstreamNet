using DeepStreamNet.Tests.Helper;
using System;
using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection(TestConstants.ServerCommunication)]
    [TestCaseOrderer("DeepStreamNet.Tests.Helper.TestCollectionOrderer", "DeepStreamNet.Tests")]
    public class ListTests : IClassFixture<DeepStreamServerFixture>
    {
        public ListTests(DeepStreamServerFixture fixture)
        {
            fixture.StartServer();
        }

        [Fact]
        public async Task NameNullTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                await Assert.ThrowsAsync<ArgumentNullException>("name",()=>client.Records.GetListAsync(null));
            }
        }

        [Fact, TestPriority(1)]
        public async Task NameTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                Assert.Equal("list1", list.ListName);
            }
        }

        [Fact, TestPriority(2)]
        public async Task AddTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                list.Add("item1");
                list.Add("item2");

                Assert.Equal(2,list.Count);
            }
        }

        [Fact, TestPriority(3)]
        public async Task RemoveTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                list.Remove("item1");

                Assert.Equal(1, list.Count);
            }
        }

        [Fact, TestPriority(4)]
        public async Task ContainsTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                Assert.True(list.Contains("item2"));                
            }
        }

        [Fact, TestPriority(5)]
        public async Task ClearTest()
        {
            using (var client = await TestHelper.GetClientAsync())
            {
                var list = await client.Records.GetListAsync("list1");
                list.Clear();

                Assert.Equal(0, list.Count);
            }
        }

    }
}
