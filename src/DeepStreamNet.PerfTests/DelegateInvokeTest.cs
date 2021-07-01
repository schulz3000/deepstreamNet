using BenchmarkDotNet.Attributes;
using System;
using System.Reflection;

namespace DeepStreamNet.PerfTests
{
    [MemoryDiagnoser]
    public class DelegateInvokeTest
    {
        private readonly Func<string, string> func = (input) => input;
        private readonly MethodInfo info;

        public DelegateInvokeTest()
        {
            info = func.GetMethodInfo();
        }

        [Benchmark]
        public string DynamikInvoke()
            => func.DynamicInvoke("test").ToString();

        [Benchmark]
        public string Invoke()
            => func.GetMethodInfo().Invoke(func.Target, new[] { "test" }).ToString();

        [Benchmark]
        public string NormalCall()
            => func("test");
    }
}
