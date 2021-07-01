using System;

namespace DeepStreamNet
{
    internal readonly struct Topic : IEquatable<Topic>
    {
        public static Topic Empty => new(string.Empty);

        public static Topic CONNECTION => new("C");

        public static Topic AUTH => new("A");

        public static Topic ERROR => new("X");

        public static Topic EVENT => new("E");

        public static Topic RECORD => new("R");

        public static Topic RPC => new("P");

        public static Topic PRESENCE => new("U");

        public static Topic PRIVATE => new("PRIVATE/");

        private readonly string Identifier;

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