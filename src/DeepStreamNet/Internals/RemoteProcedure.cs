using System;
using System.Reflection;

namespace DeepStreamNet
{
    internal class RemoteProcedure
    {
        public RemoteProcedure(string name, Delegate procedure)
        {
            if (string.IsNullOrWhiteSpace(name))
            {
                throw new ArgumentNullException(nameof(name));
            }

            Name = name;

            Procedure = procedure ?? throw new ArgumentNullException(nameof(procedure));

            var methodParameters = procedure.GetMethodInfo().GetParameters();

            OriginalParameterType = methodParameters[0].ParameterType;
            ParameterType = DetectDeepstreamParameterType(OriginalParameterType);
            ReturnType = methodParameters[1].ParameterType.GetGenericArguments()[0];
        }

        private static Type DetectDeepstreamParameterType(Type type)
        {
            if (Utils.IsNumeric(type))
            {
                return typeof(double);
            }

            if (type == typeof(string))
            {
                return type;
            }

            if (type == typeof(bool))
            {
                return type;
            }

            return typeof(object);
        }

        public string Name { get; }

        public Delegate Procedure { get; }

        public Type ParameterType { get; }

        public Type OriginalParameterType { get; }

        public Type ReturnType { get; }
    }
}
