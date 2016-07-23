using System;

namespace DeepStreamNet
{
    struct Action
    {
        public static Action Empty { get { return new Action(null); } }
        public static Action ACK { get { return new Action("A"); } }
        public static Action READ { get { return new Action("R"); } }
        public static Action CREATE { get { return new Action("C"); } }
        public static Action UPDATE { get { return new Action("U"); } }
        public static Action PATCH { get { return new Action("P"); } }
        public static Action DELETE { get { return new Action("D"); } }
        public static Action HAS { get { return new Action("H"); } }
        public static Action SNAPSHOT { get { return new Action("SN"); } }
        public static Action SUBSCRIBE { get { return new Action("S"); } }
        public static Action UNSUBSCRIBE { get { return new Action("US"); } }
        public static Action INVOKE { get { return new Action("I"); } }
        public static Action SUBSCRIPTION_FOR_PATTERN_FOUND { get { return new Action("SP"); } }
        public static Action SUBSCRIPTION_FOR_PATTERN_REMOVED { get { return new Action("SR"); } }
        public static Action LISTEN { get { return new Action("L"); } }
        public static Action UNLISTEN { get { return new Action("UL"); } }
        public static Action PROVIDER_UPDATE { get { return new Action("PU"); } }
        public static Action QUERY { get { return new Action("Q"); } }
        public static Action CREATEORREAD { get { return new Action("CR"); } }
        public static Action EVENT { get { return new Action("EVT"); } }
        public static Action ERROR { get { return new Action("E"); } }
        public static Action REQUEST { get { return new Action("REQ"); } }
        public static Action RESPONSE { get { return new Action("RES"); } }
        public static Action REJECTION { get { return new Action("REJ"); } }

        readonly string Identifier;

        public Action(string identifier)
        {
            Identifier = identifier;
        }

        public static bool operator ==(Action a1, Action a2)
        {
            return a1.Equals(a2);
        }

        public static bool operator !=(Action a1, Action a2)
        {
            return !a1.Equals(a2);
        }

        public override bool Equals(object obj)
        {
            if (obj == null || !(obj is Action))
                return false;

            var other = (Action)obj;

            return string.Equals(Identifier, other.Identifier, StringComparison.Ordinal);
        }

        public override int GetHashCode()
        {
            return Identifier.GetHashCode();
        }

        public override string ToString()
        {
            return Identifier;
        }
    }
}