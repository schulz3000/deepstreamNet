using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.RPC
{
    [Binding]
    public class RPCConnectivitySteps
    {
        [When(@"the client provides a RPC called ""(.*)""")]
        public void WhenTheClientProvidesARPCCalled(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message P\|A\|S\|toUppercase\+")]
        public void WhenTheServerSendsTheMessagePASToUppercase()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is P\|S\|toUppercase\+")]
        public void ThenTheLastMessageTheServerRecievedIsPSToUppercase()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server received the message P\|S\|toUppercase\+")]
        public void ThenTheServerReceivedTheMessagePSToUppercase()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
