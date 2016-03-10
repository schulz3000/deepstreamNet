using System.Text;

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
    }
}
