using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using Jil;

namespace DeepStreamNet
{
    static class Utils
    {
        public static string BuildCommand(Topic topic, Action action, params object[] args)
        {
            var sb = new StringBuilder(4 + args.Length * 2);

            sb.Append(topic);
            sb.Append(Constants.RecordSeperator);
            sb.Append(action);
            sb.Append(Constants.RecordSeperator);

            for (int i = 0; i < args.Length; i++)
            {
                sb.Append(args[i]);
                sb.Append(Constants.RecordSeperator);
            }

            sb[sb.Length - 1] = Constants.GroupSeperator;

            return sb.ToString();
        }

        public static string ConvertAndPrefixData<T>(T data)
        {
            if (data is string)
            {
                return Constants.Types.STRING + data.ToString();
            }
            else if (data is bool)
            {
                if (bool.Parse(data.ToString()))
                    return Constants.Types.TRUE.ToString();
                else
                    return Constants.Types.FALSE.ToString();
            }
            else if (data is int || data is double || data is float || data is decimal)
            {
                return Constants.Types.NUMBER + data.ToString().Replace(',', '.');
            }
            else if ((data as object) == null)
            {
                return Constants.Types.NULL.ToString();
            }
            else
            {
                try
                {
                    return Constants.Types.OBJECT + JSON.Serialize(data, Options.ISO8601);
                }
                catch
                {
                    return Constants.Types.UNDEFINED.ToString();
                }
            }
        }

        public static KeyValuePair<Type, object> ConvertPrefixedData(string dataWithTypePrefix)
        {
            var evtData = dataWithTypePrefix.Substring(1);

            switch (dataWithTypePrefix[0])
            {
                case Constants.Types.STRING:
                    return new KeyValuePair<Type, object>(typeof(string), evtData);

                case Constants.Types.NUMBER:
                    return new KeyValuePair<Type, object>(typeof(double), double.Parse(evtData, CultureInfo.InvariantCulture));

                case Constants.Types.TRUE:
                    return new KeyValuePair<Type, object>(typeof(bool), true);

                case Constants.Types.FALSE:
                    return new KeyValuePair<Type, object>(typeof(bool), false);

                case Constants.Types.NULL:
                    return new KeyValuePair<Type, object>(typeof(object), null);

                case Constants.Types.OBJECT:
                    return new KeyValuePair<Type, object>(typeof(object), JSON.DeserializeDynamic(evtData, Options.RFC1123));

                default:
                    return new KeyValuePair<Type, object>(typeof(string), evtData);
            }
        }

        public static string CreateUid()
        {
            return Guid.NewGuid().ToString("N");
        }
    }
}