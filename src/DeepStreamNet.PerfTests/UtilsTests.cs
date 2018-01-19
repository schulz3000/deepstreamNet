using BenchmarkDotNet.Attributes;

namespace DeepStreamNet.PerfTests
{
    [MemoryDiagnoser]
    public class UtilsTests
    {
        [Benchmark]
        public string BuildCommandTest()
        {
            return Utils.BuildCommand(Topic.EVENT, Action.EVENT, "abc", "def");
        }

        [Benchmark]
        public bool IsNumericIntTest()
        {
            return Utils.IsNumeric(typeof(int));
        }

        [Benchmark]
        public bool IsNumericNullableIntTest()
        {
            return Utils.IsNumeric(typeof(int?));
        }

        [Benchmark]
        public bool IsNumericNullTest()
        {
            return Utils.IsNumeric(null);
        }

        [Benchmark]
        public string CreatUidTest()
        {
            return Utils.CreateUid();
        }
    }
}
