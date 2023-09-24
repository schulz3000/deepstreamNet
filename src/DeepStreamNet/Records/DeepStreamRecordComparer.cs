﻿using System;
using System.Collections.Generic;

namespace DeepStreamNet
{
    internal class DeepStreamRecordComparer : IEqualityComparer<IDeepStreamRecordWrapper>
    {
        public bool Equals(IDeepStreamRecordWrapper x, IDeepStreamRecordWrapper y)
        {
            // If reference same object including null then return true
            if (ReferenceEquals(x, y))
            {
                return true;
            }

            // If one object null the return false
            if (x is null || y is null)
            {
                return false;
            }

            return x.RecordName.Equals(y.RecordName, StringComparison.OrdinalIgnoreCase);
        }

        public int GetHashCode(IDeepStreamRecordWrapper obj)
            => obj.RecordName.GetHashCode();
    }
}