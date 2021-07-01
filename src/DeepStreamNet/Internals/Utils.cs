﻿using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace DeepStreamNet
{
    internal static class Utils
    {
        public static string BuildCommand(Topic topic, Action action, params object[] args)
        {
            var sb = StringBuilderPool.Aquire();

            sb.Append(topic.ToString());
            sb.Append(Constants.RecordSeperator);
            sb.Append(action.ToString());
            sb.Append(Constants.RecordSeperator);

            foreach (var item in args)
            {
                sb.Append(item);
                sb.Append(Constants.RecordSeperator);
            }

            sb[sb.Length - 1] = Constants.GroupSeperator;

            return StringBuilderPool.ToStringAndRelease(sb);
        }

        public static string ConvertAndPrefixData<T>(T data)
        {
            if (data is JToken token)
            {
                return ConvertAndPrefixDataFromJToken(token);
            }

            if (data is string)
            {
                return Constants.Types.STRING.ToString() + data;
            }

            if (data is bool value)
            {
                return value ? Constants.Types.TRUE.ToString() : Constants.Types.FALSE.ToString();
            }

            if (data is int || data is double || data is float || data is decimal)
            {
                return Constants.Types.NUMBER.ToString() + data.ToString().Replace(',', '.');
            }

            if (data == null)
            {
                return Constants.Types.NULL.ToString();
            }

            try
            {
                return Constants.Types.OBJECT + JsonConvert.SerializeObject(data);
            }
            catch
            {
                return Constants.Types.UNDEFINED.ToString();
            }
        }

        private static string ConvertAndPrefixDataFromJToken(JToken data)
        {
            switch (data.Type)
            {
                case JTokenType.String:
                    return Constants.Types.STRING + data.ToObject<string>();
                case JTokenType.Boolean:
                    return data.ToObject<bool>() ? Constants.Types.TRUE.ToString() : Constants.Types.FALSE.ToString();
                case JTokenType.Float:
                case JTokenType.Integer:
                    return Constants.Types.NUMBER + data.ToObject<string>();
                case JTokenType.Null:
                    return Constants.Types.NULL.ToString();
                default:
                    try
                    {
                        return Constants.Types.OBJECT + JsonConvert.SerializeObject(data);
                    }
                    catch
                    {
                        return Constants.Types.UNDEFINED.ToString();
                    }
            }
        }

        public static KeyValuePair<Type, JToken> ConvertPrefixedData(string dataWithTypePrefix)
        {
            var evtData = dataWithTypePrefix.Substring(1);

            return dataWithTypePrefix[0] switch
            {
                Constants.Types.STRING => new KeyValuePair<Type, JToken>(typeof(string), JToken.FromObject(evtData)),
                Constants.Types.NUMBER => new KeyValuePair<Type, JToken>(typeof(double), JToken.Parse(evtData)),
                Constants.Types.TRUE => new KeyValuePair<Type, JToken>(typeof(bool), JToken.FromObject(true)),
                Constants.Types.FALSE => new KeyValuePair<Type, JToken>(typeof(bool), JToken.FromObject(false)),
                Constants.Types.NULL => new KeyValuePair<Type, JToken>(typeof(object), JValue.CreateNull()),
                Constants.Types.OBJECT => new KeyValuePair<Type, JToken>(typeof(object), JToken.Parse(evtData)),
                _ => new KeyValuePair<Type, JToken>(typeof(string), JToken.Parse(evtData)),
            };
        }

        private static readonly Dictionary<Type, JTokenType> TypeMappings = new()
        {
            [typeof(bool)] = JTokenType.Boolean,
            [typeof(string)] = JTokenType.String,
            [typeof(Guid)] = JTokenType.Guid,
            [typeof(DateTime)] = JTokenType.Date,
            [typeof(TimeSpan)] = JTokenType.TimeSpan,
            [typeof(double)] = JTokenType.Float,
            [typeof(float)] = JTokenType.Float,
            [typeof(decimal)] = JTokenType.Float,
            [typeof(int)] = JTokenType.Integer,
            [typeof(short)] = JTokenType.Integer,
            [typeof(byte)] = JTokenType.Integer
        };

        public static bool IsJTokenTypeEqualNetType(JTokenType tokenType, Type type)
        {
            if (TypeMappings.ContainsKey(type))
            {
                return TypeMappings[type] == tokenType;
            }

            return tokenType == JTokenType.Object || tokenType == JTokenType.Null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsNumeric(Type type)
        {
            if (type == null)
            {
                return false;
            }

            switch (Type.GetTypeCode(type))
            {
                case TypeCode.Byte:
                case TypeCode.Decimal:
                case TypeCode.Double:
                case TypeCode.Int16:
                case TypeCode.Int32:
                case TypeCode.Int64:
                case TypeCode.SByte:
                case TypeCode.Single:
                case TypeCode.UInt16:
                case TypeCode.UInt32:
                case TypeCode.UInt64:
                    return true;
                case TypeCode.Object:
                    if (type.GetTypeInfo().IsGenericType && type.GetGenericTypeDefinition() == NullableType)
                    {
                        return IsNumeric(type.GetGenericArguments()[0]);
                    }
                    return false;
            }
            return false;
        }

        private static readonly Type NullableType = typeof(Nullable<>);

        public static string CreateUid() => Guid.NewGuid().ToString("N");
    }
}