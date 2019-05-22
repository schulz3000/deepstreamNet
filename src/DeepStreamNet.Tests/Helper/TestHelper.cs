using Microsoft.Extensions.Configuration;
using System.Threading.Tasks;

namespace DeepStreamNet.Tests.Helper
{
    public static class TestHelper
    {
        public static async Task<DeepStreamClient> GetClientAsync()
        {
            var client = new DeepStreamClient(Config["deepStreamHost"], short.Parse(Config["deepStreamPort"]), Config["deepStreamPath"], bool.Parse(Config["useSecureConnection"]));
            await client.LoginAsync();
            return client;
        }

        static IConfigurationRoot config;

        public static IConfigurationRoot Config =>
            config ?? (config = new ConfigurationBuilder()
                .AddJsonFile("testsettings.json")
                .AddEnvironmentVariables("ds_")
                .Build());
    }
}
