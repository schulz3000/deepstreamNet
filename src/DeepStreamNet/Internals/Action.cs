using System;

namespace DeepStreamNet
{
    internal readonly struct Action : IEquatable<Action>
    {
        public static Action Empty => new(string.Empty);
        public static Action PING => new("PI");
        public static Action PONG => new("PO");
        public static Action ACK => new("A");
        public static Action REDIRECT => new("RED");
        public static Action CHALLENGE => new("CH");
        public static Action CHALLENGE_RESPONSE => new("CHR");
        public static Action READ => new("R");
        public static Action CREATE => new("C");
        public static Action UPDATE => new("U");
        public static Action PATCH => new("P");
        public static Action DELETE => new("D");
        public static Action SUBSCRIBE => new("S");
        public static Action UNSUBSCRIBE => new("US");
        public static Action HAS => new("H");
        public static Action SNAPSHOT => new("SN");
        public static Action INVOKE => new("I");
        public static Action SUBSCRIPTION_FOR_PATTERN_FOUND => new("SP");
        public static Action SUBSCRIPTION_FOR_PATTERN_REMOVED => new("SR");
        public static Action SUBSCRIPTION_HAS_PROVIDER => new("SH");
        public static Action LISTEN => new("L");
        public static Action UNLISTEN => new("UL");
        public static Action LISTEN_ACCEPT => new("LA");
        public static Action LISTEN_REJECT => new("LR");
        public static Action PROVIDER_UPDATE => new("PU");
        public static Action QUERY => new("Q");
        public static Action CREATEORREAD => new("CR");
        public static Action CREATEANDUPDATE => new("CU");
        public static Action EVENT => new("EVT");
        public static Action ERROR => new("E");
        public static Action REQUEST => new("REQ");
        public static Action RESPONSE => new("RES");
        public static Action REJECTION => new("REJ");
        public static Action PRESENCE_JOIN => new("PNJ");
        public static Action PRESENCE_LEAVE => new("PNL");
        public static Action WRITE_ACKNOWLEDGEMENT => new("WA");

        private readonly string Identifier;

        public Action(string identifier) => Identifier = identifier;

        public static bool operator ==(Action a1, Action a2) => a1.Equals(a2);

        public static bool operator !=(Action a1, Action a2) => !a1.Equals(a2);

        public override bool Equals(object obj)
            => obj is Action other && Equals(other);

        public override int GetHashCode() => Identifier.GetHashCode();

        public override string ToString() => Identifier;

        public bool Equals(Action other)
            => Identifier.Equals(other.Identifier, StringComparison.Ordinal);
    }
}