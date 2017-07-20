using Newtonsoft.Json.Linq;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection("Local")]
    public class UtilsTest
    {
        [Fact]
        public void BuildCommandTest()
        {
            const char recordSeperator = (char)31;
            const char groupSeperator = (char)30;
            var cmd = Utils.BuildCommand(Topic.EVENT, Action.EVENT, "abc", "def");
            Assert.Equal($"E{recordSeperator}EVT{recordSeperator}abc{recordSeperator}def{groupSeperator}", cmd);
        }

        [Fact]
        public void IsNumericIntTest()
        {
            Assert.True(Utils.IsNumeric(typeof(int)));
        }

        [Fact]
        public void IsNumericDoubleTest()
        {
            Assert.True(Utils.IsNumeric(typeof(double)));
        }

        [Fact]
        public void IsNumericDecimalTest()
        {
            Assert.True(Utils.IsNumeric(typeof(decimal)));
        }

        [Fact]
        public void IsNumericShortTest()
        {
            Assert.True(Utils.IsNumeric(typeof(short)));
        }

        [Fact]
        public void IsNumericByteTest()
        {
            Assert.True(Utils.IsNumeric(typeof(byte)));
        }

        [Fact]
        public void IsNumericNullableIntTest()
        {
            Assert.True(Utils.IsNumeric(typeof(int?)));
        }

        [Fact]
        public void IsNumericNullableDoubleTest()
        {
            Assert.True(Utils.IsNumeric(typeof(double?)));
        }

        [Fact]
        public void IsNumericNullableDecimalTest()
        {
            Assert.True(Utils.IsNumeric(typeof(decimal?)));
        }

        [Fact]
        public void IsNumericNullableShortTest()
        {
            Assert.True(Utils.IsNumeric(typeof(short?)));
        }

        [Fact]
        public void IsNumericNullableByteTest()
        {
            Assert.True(Utils.IsNumeric(typeof(byte?)));
        }

        [Fact]
        public void IsNumericStringTest()
        {
            Assert.False(Utils.IsNumeric(typeof(string)));
        }

        [Fact]
        public void IsNumericNullTest()
        {
            Assert.False(Utils.IsNumeric(null));
        }

        [Fact]
        public void ConvertPrefixedDataStringTest()
        {
            var type = Utils.ConvertPrefixedData("Sabc");
            Assert.Equal(typeof(string), type.Key);
            Assert.Equal(JTokenType.String, type.Value.Type);
            Assert.Equal("abc", type.Value.ToObject<string>());
        }

        [Fact]
        public void ConvertPrefixedDataNumberTest()
        {
            var type = Utils.ConvertPrefixedData("N3.55");
            Assert.Equal(typeof(double), type.Key);
            Assert.Equal(JTokenType.Float, type.Value.Type);
            Assert.Equal(3.55, type.Value.ToObject<double>());
        }

        [Fact]
        public void ConvertPrefixedDataBoolTrueTest()
        {
            var type = Utils.ConvertPrefixedData("T");
            Assert.Equal(typeof(bool), type.Key);
            Assert.Equal(JTokenType.Boolean, type.Value.Type);
            Assert.Equal(true, type.Value.ToObject<bool>());
        }

        [Fact]
        public void ConvertPrefixedDataBoolFalseTest()
        {
            var type = Utils.ConvertPrefixedData("F");
            Assert.Equal(typeof(bool), type.Key);
            Assert.Equal(JTokenType.Boolean, type.Value.Type);
            Assert.Equal(false, type.Value.ToObject<bool>());
        }

        [Fact]
        public void ConvertPrefixedDataNullTest()
        {
            var type = Utils.ConvertPrefixedData("L");
            Assert.Equal(typeof(object), type.Key);
            Assert.Equal(JTokenType.Null, type.Value.Type);
            Assert.Null(type.Value.ToObject<object>());
        }

        [Fact]
        public void ConvertPrefixedDataObjectTest()
        {
            var type = Utils.ConvertPrefixedData("O{prop:\"abc\"}");
            Assert.Equal(typeof(object), type.Key);
            Assert.Equal(JTokenType.Object, type.Value.Type);
            Assert.Equal(JToken.Parse("{prop:\"abc\"}"), type.Value);
        }

        [Fact]
        public void CreateUidTest()
        {
            Assert.Equal(32, Utils.CreateUid().Length);
        }
    }
}
