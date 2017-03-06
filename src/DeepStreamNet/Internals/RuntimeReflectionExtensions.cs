#if NET40
namespace System.Reflection
{
    /// <summary>
    /// 
    /// </summary>
    public static class RuntimeReflectionExtensions
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="del"></param>
        /// <returns></returns>
        public static MethodInfo GetMethodInfo(this Delegate del)
        {
            if (del == null) throw new ArgumentNullException(nameof(del));

            return del.Method;
        }
    }
}
#endif
