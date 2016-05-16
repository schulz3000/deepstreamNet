using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using Newtonsoft.Json;

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
                    return Constants.Types.OBJECT + JsonConvert.SerializeObject(data);
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
                     return new KeyValuePair<Type, object>(typeof(object), JsonConvert.DeserializeObject(evtData));
                    //return new KeyValuePair<Type, object>(typeof(object),Newtonsoft.Json.JsonConvert.DeserializeObject(evtData));

                default:
                    return new KeyValuePair<Type, object>(typeof(string), evtData);
            }
        }

        public static object GetValueByPath(string path, DeepStreamInnerRecord record)
        {
            if (string.IsNullOrWhiteSpace(path))
                throw new ArgumentNullException(nameof(path));

            if (record == null)
                throw new ArgumentNullException(nameof(record));


            var spath = path.Split(Constants.PathSplitter, StringSplitOptions.RemoveEmptyEntries);

            if (spath.Length == 1)
                return record[path];
            else
            {
                
                var max = spath.Length - 1;
                var index = -1;
                for (int i = 0; i <= max; i++)
                {
                    if (i == max)
                    {
                       return record[spath[i]];
                    }
                    else if (int.TryParse(spath[i + 1], out index))
                    {
                        var item = (record[spath[i]] as DeepStreamRecordCollection<object>)[index];                                                
                        i++;

                        if (i == max)
                            return item;
                        else 
                            record = item as DeepStreamInnerRecord;

                    }
                    else
                    {
                        record = record[spath[i]] as DeepStreamInnerRecord;
                    }
                }
            }

            return null;
        }

        public static string CreateUid()
        {
            return Guid.NewGuid().ToString("N");
        }
    }
}