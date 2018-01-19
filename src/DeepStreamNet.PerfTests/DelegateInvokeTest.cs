using BenchmarkDotNet.Attributes;
using System;
using System.Reflection;

namespace DeepStreamNet.PerfTests
{
    [MemoryDiagnoser]
    public class DelegateInvokeTest
    {
        readonly Func<string, string> func = (input) => input;
        readonly MethodInfo info;

        public DelegateInvokeTest()
        {
            info = func.GetMethodInfo();
        }

        [Benchmark]
        public string DynamikInvoke()
        {
            return func.DynamicInvoke("test").ToString();
        }

        [Benchmark]
        public string Invoke()
        {
            return func.GetMethodInfo().Invoke(func.Target, new[] { "test" }).ToString();
        }

        [Benchmark]
        public string NormalCall()
        {
            return func("test");
        }
    }
}
