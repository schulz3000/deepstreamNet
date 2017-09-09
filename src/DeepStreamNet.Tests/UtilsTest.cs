using DeepStreamNet.Tests.Helper;
using Newtonsoft.Json.Linq;
using System;
using Xunit;

namespace DeepStreamNet.Tests
{
    [Collection(TestConstants.Local)]
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
        public void IsNumericNullableGuidTest()
        {            
            Assert.False(Utils.IsNumeric(typeof(Guid?)));
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
        public void ConvertAndPrefixDataFromJTokenNullTest()
        {
            var result = Utils.ConvertAndPrefixData(JValue.CreateNull());
            Assert.Equal(Constants.Types.NULL.ToString(),result);
        }

        [Fact]
        public void ConvertAndPrefixDataFromJTokenTrueTest()
        {
            var result = Utils.ConvertAndPrefixData(JToken.FromObject(true));
            Assert.Equal(Constants.Types.TRUE.ToString(), result);
        }

        [Fact]
        public void ConvertAndPrefixDataFromJTokenFalseTest()
        {
            var result = Utils.ConvertAndPrefixData(JToken.FromObject(false));
            Assert.Equal(Constants.Types.FALSE.ToString(), result);
        }

        [Fact]
        public void ConvertAndPrefixDataFromJTokenIntegerTest()
        {
            var result = Utils.ConvertAndPrefixData(JToken.FromObject(1));
            Assert.Equal(Constants.Types.NUMBER.ToString()+"1", result);
        }

        [Fact]
        public void ConvertAndPrefixDataFromJTokenDoubleTest()
        {
            var result = Utils.ConvertAndPrefixData(JToken.FromObject(1.1));
            Assert.Equal(Constants.Types.NUMBER.ToString()+"1.1", result);
        }

        [Fact]
        public void ConvertAndPrefixDataFromJTokenObjectTest()
        {
            var result = Utils.ConvertAndPrefixData(JToken.FromObject(new{ Property1="abc" }));
            Assert.Equal(Constants.Types.OBJECT.ToString() + "{\"Property1\":\"abc\"}", result);
        }        

        [Fact]
        public void CreateUidTest()
        {
            Assert.Equal(32, Utils.CreateUid().Length);
        }
    }
}
