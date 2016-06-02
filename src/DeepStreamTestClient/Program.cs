using System;
using System.Threading.Tasks;

namespace DeepStreamNet.ConsoleClient
{
    class Program
    {
        static void Main(string[] args)
        {

           MainAsync(args).GetAwaiter().GetResult();
           
            Console.Read();
        }

        static  Task MainAsync(string[] args)
        {
            return Exec();
        }        

        async static Task Exec()
        {
            var client = new DeepStreamClient("localhost", 6021);

            if (await client.LoginAsync())
            {
                var disp = await client.Events.Subscribe("test", Console.WriteLine);

                await Task.Delay(2000);

                await client.Events.PublishAsync("test", "Hello World");

                await Task.Delay(30000);

                Console.ReadKey();

                await disp.DisposeAsync();
            }

            client.Dispose();
        }
    }

}
