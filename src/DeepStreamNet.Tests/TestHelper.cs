using Microsoft.Extensions.Configuration;
using System;
using System.Threading.Tasks;

namespace DeepStreamNet.Tests
{
    public static class TestHelper
    {
        public static async Task<DeepStreamClient> GetClient()
        {
            Console.WriteLine("Custom output");
            Console.WriteLine(Config["deepStreamHost"]);
            Console.WriteLine(Config["ds-deepStreamHost"]);
            var client = new DeepStreamClient(Config["deepStreamHost"], int.Parse(Config["deepStreamPort"]), Config["deepStreamPath"], bool.Parse(Config["useSecureConnection"]));
            await client.LoginAsync();
            return client;
        }

        static IConfigurationRoot config;

        public static IConfigurationRoot Config
        {
            get
            {
                if (config == null)
                {
                    config = new ConfigurationBuilder()
                      .AddJsonFile("testsettings.json")
                      .AddEnvironmentVariables("ds-")
                      .Build();
                }

                return config;
            }
        }
    }
}
