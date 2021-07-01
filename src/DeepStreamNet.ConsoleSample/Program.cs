using System;
using System.Threading.Tasks;

namespace DeepStreamNet.Core.ConsoleSample
{
    public static class Program
    {
        public static async Task Main(string[] args)
        {
            using var client = new DeepStreamClient("localhost", 6020);

            if (await client.LoginAsync())
            {
                Console.WriteLine("Login successful");

                var disp = await client.Events.SubscribeAsync("test", Console.WriteLine);

                await Task.Delay(2000);

                client.Events.Publish("test", "Hello World");

                await Task.Delay(30000);

                Console.ReadKey();

                await disp.DisposeAsync();
            }
            else
            {
                Console.WriteLine("Login was not successful");
            }

            Console.WriteLine("Finish. Press any key to for exit");
            Console.Read();
        }
    }
}
