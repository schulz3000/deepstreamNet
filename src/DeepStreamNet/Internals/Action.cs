using System;

namespace DeepStreamNet
{
    readonly struct Action : IEquatable<Action>
    {
        public static Action Empty => new Action(string.Empty);
        public static Action PING => new Action("PI");
        public static Action PONG => new Action("PO");
        public static Action ACK => new Action("A");
        public static Action REDIRECT => new Action("RED");
        public static Action CHALLENGE => new Action("CH");
        public static Action CHALLENGE_RESPONSE => new Action("CHR");
        public static Action READ => new Action("R");
        public static Action CREATE => new Action("C");
        public static Action UPDATE => new Action("U");
        public static Action PATCH => new Action("P");
        public static Action DELETE => new Action("D");
        public static Action SUBSCRIBE => new Action("S");
        public static Action UNSUBSCRIBE => new Action("US");
        public static Action HAS => new Action("H");
        public static Action SNAPSHOT => new Action("SN");
        public static Action INVOKE => new Action("I");
        public static Action SUBSCRIPTION_FOR_PATTERN_FOUND => new Action("SP");
        public static Action SUBSCRIPTION_FOR_PATTERN_REMOVED => new Action("SR");
        public static Action SUBSCRIPTION_HAS_PROVIDER => new Action("SH");
        public static Action LISTEN => new Action("L");
        public static Action UNLISTEN => new Action("UL");
        public static Action LISTEN_ACCEPT => new Action("LA");
        public static Action LISTEN_REJECT => new Action("LR");
        public static Action PROVIDER_UPDATE => new Action("PU");
        public static Action QUERY => new Action("Q");
        public static Action CREATEORREAD => new Action("CR");
        public static Action CREATEANDUPDATE => new Action("CU");
        public static Action EVENT => new Action("EVT");
        public static Action ERROR => new Action("E");
        public static Action REQUEST => new Action("REQ");
        public static Action RESPONSE => new Action("RES");
        public static Action REJECTION => new Action("REJ");
        public static Action PRESENCE_JOIN => new Action("PNJ");
        public static Action PRESENCE_LEAVE => new Action("PNL");
        public static Action WRITE_ACKNOWLEDGEMENT => new Action("WA");

        readonly string Identifier;

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