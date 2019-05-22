using System;

namespace DeepStreamNet
{
    readonly struct Topic : IEquatable<Topic>
    {
        public static Topic Empty => new Topic(string.Empty);

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
            => obj is Topic other && Equals(other);

        public override int GetHashCode() => Identifier.GetHashCode();

        public override string ToString() => Identifier;

        public bool Equals(Topic other)
            => Identifier.Equals(other.Identifier, StringComparison.Ordinal);
    }
}