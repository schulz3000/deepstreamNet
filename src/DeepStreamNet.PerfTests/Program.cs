using BenchmarkDotNet.Attributes.Jobs;
using BenchmarkDotNet.Running;
using System;

namespace DeepStreamNet.PerfTests
{
    class Program
    {
        static void Main(string[] args)
        {
            var summary = BenchmarkRunner.Run<UtilsTests>();

            Console.ReadKey();
        }
    }
}