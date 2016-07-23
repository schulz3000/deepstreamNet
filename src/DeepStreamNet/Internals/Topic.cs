using System;

namespace DeepStreamNet
{
    struct Topic
    {
        public static Topic Empty
        {
            get
            {
                return new Topic(null);
            }
        }

        public static Topic AUTH
        {
            get
            {
                return new Topic("A");
            }
        }

        public static Topic ERROR
        {
            get
            {
                return new Topic("X");
            }
        }

        public static Topic EVENT
        {
            get
            {
                return new Topic("E");
            }
        }

        public static Topic RECORD
        {
            get
            {
                return new Topic("R");
            }
        }

        public static Topic RPC
        {
            get
            {
                return new Topic("P");
            }
        }

        public static Topic WEBRTC
        {
            get
            {
                return new Topic("W");
            }
        }

        public static Topic PRIVATE
        {
            get
            {
                return new Topic("PRIVATE/");
            }
        }

        readonly string Identifier;

        public Topic(string identifier)
        {
            Identifier = identifier;
        }

        public static bool operator ==(Topic t1, Topic t2)
        {
            return t1.Equals(t2);
        }

        public static bool operator !=(Topic t1, Topic t2)
        {
            return !t1.Equals(t2);
        }

        public override bool Equals(object obj)
        {
            if (obj == null || !(obj is Topic))
                return false;

            var other = (Topic)obj;

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