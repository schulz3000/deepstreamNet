using System.Threading.Tasks;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection("Local")]
    public class DisposableActionTests
    {
        [Fact]
        public void DisposeTest()
        {
            int i = 0;
            var disp = new DisposableAction(() => i = 1);

            Assert.Equal(0, i);

            disp.Dispose();

            Assert.Equal(1, i);
        }

        [Fact]
        public async Task AsyncDisposeTest()
        {
            int i = 0;
            var disp = new AsyncDisposableAction(async () =>
            {
                await Task.Delay(1);
                i = 1;
            });

            Assert.Equal(0, i);

            await disp.DisposeAsync();

            Assert.Equal(1, i);
        }
    }
}
