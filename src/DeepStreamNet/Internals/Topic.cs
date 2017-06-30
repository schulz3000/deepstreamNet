using System;

namespace DeepStreamNet
{
    struct Topic
    {
        public static Topic Empty => new Topic(null);

        public static Topic CONNECTION => new Topic("C");

        public static Topic AUTH => new Topic("A");

        public static Topic ERROR => new Topic("X");

        public static Topic EVENT => new Topic("E");

        public static Topic RECORD => new Topic("R");

        public static Topic RPC => new Topic("P");

        public static Topic PRESENCE => new Topic("U");

        public static Topic PRIVATE => new Topic("PRIVATE/");

        readonly string Identifier;

        public Topic(string identifier) => Identifier = identifier;

        public static bool operator ==(Topic t1, Topic t2) => t1.Equals(t2);

        public static bool operator !=(Topic t1, Topic t2) => !t1.Equals(t2);

        public override bool Equals(object obj)
        {
            if (obj == null || !(obj is Topic))
                return false;

            var other = (Topic)obj;

            return string.Equals(Identifier, other.Identifier, StringComparison.Ordinal);
        }

        public override int GetHashCode() => Identifier.GetHashCode();

        public override string ToString() => Identifier;
    }
}