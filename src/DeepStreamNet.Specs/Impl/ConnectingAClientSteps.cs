using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs
{
    [Binding]
    public class ConnectingAClientSteps
    {
        [Then(@"the server has (.*) active connections")]
        public void ThenTheServerHasActiveConnections(int activeConnections)
        {
            //has active connections
        }
    }
}
