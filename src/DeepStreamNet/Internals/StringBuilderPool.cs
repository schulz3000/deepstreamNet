using System;
using System.Text;

namespace DeepStreamNet
{
    static class StringBuilderPool
    {
        [ThreadStatic]
        static StringBuilder pool;

        public static StringBuilder Aquire()
        {
            var ret = pool;
            if (ret == null)
                return new StringBuilder();

            ret.Length = 0;
            pool = null;
            return ret;
        }

        public static string ToStringAndRelease(StringBuilder sb)
        {
            var ret = sb.ToString();
            pool = sb;
            return ret;
        }
    }
}
