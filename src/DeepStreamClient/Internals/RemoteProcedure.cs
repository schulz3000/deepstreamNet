using System;
using System.Reflection;

namespace DeepStreamNet
{
    class RemoteProcedure
    {
        public RemoteProcedure(string name, Delegate procedure)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentNullException(nameof(name));

            if (procedure == null)
                throw new ArgumentNullException(nameof(procedure));

            Name = name;
            Procedure = procedure;

            var methodInfo = procedure.GetMethodInfo();

            OriginalParameterType = methodInfo.GetParameters()[0].ParameterType;
            ParameterType = DetectDeepstreamParameterType(OriginalParameterType);
            ReturnType = methodInfo.GetParameters()[1].ParameterType.GetGenericArguments()[0];
        }

        Type DetectDeepstreamParameterType(Type type)
        {
            if (Utils.IsNumeric(type))
                return typeof(double);

            if (type == typeof(string))
                return type;

            if (type == typeof(bool))
                return type;

            return typeof(object);
        }

        public string Name { get; }

        public Delegate Procedure { get; }

        public Type ParameterType { get; }

        public Type OriginalParameterType { get; }

        public Type ReturnType { get; }
    }
}
