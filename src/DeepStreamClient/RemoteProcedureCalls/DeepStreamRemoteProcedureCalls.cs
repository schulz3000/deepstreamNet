using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DeepStreamNet.Contracts;

namespace DeepStreamNet
{
    class DeepStreamRemoteProcedureCalls : DeepStreamBase, IDeepStreamRemoteProcedureCalls
    {
        public DeepStreamRemoteProcedureCalls(Connection con) 
            : base(con)
        {

        }
    }
}
