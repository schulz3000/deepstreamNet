using BenchmarkDotNet.Attributes;

namespace DeepStreamNet.PerfTests
{
    [MemoryDiagnoser]
    public class UtilsTests
    {
        [Benchmark]
        public string BuildCommandTest()
            => Utils.BuildCommand(Topic.EVENT, Action.EVENT, "abc", "def");

        [Benchmark]
        public bool IsNumericIntTest()
            => Utils.IsNumeric(typeof(int));

        [Benchmark]
        public bool IsNumericNullableIntTest()
            => Utils.IsNumeric(typeof(int?));

        [Benchmark]
        public bool IsNumericNullTest()
            => Utils.IsNumeric(null);

        [Benchmark]
        public string CreatUidTest()
            => Utils.CreateUid();
    }
}
