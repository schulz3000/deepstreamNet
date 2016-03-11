using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.RPC
{
    [Binding]
    public class RequestingAnRPCSteps
    {
        [When(@"the client requests RPC ""(.*)"" with data ""(.*)""")]
        public void WhenTheClientRequestsRPCWithData(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message P\|A\|REQ\|(.*)\+")]
        public void WhenTheServerSendsTheMessagePAREQ(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message P\|RES\|toUppercase\|(.*)\|SABC\+")]
        public void WhenTheServerSendsTheMessagePRESToUppercaseSABC(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message P\|E\|RPC Error Message\|toUppercase\|(.*)\+")]
        public void WhenTheServerSendsTheMessagePERPCErrorMessageToUppercase(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is P\|REQ\|toUppercase\|(.*)\|Sabc\+")]
        public void ThenTheLastMessageTheServerRecievedIsPREQToUppercaseSabc(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client recieves a successful RPC callback for ""(.*)"" with data ""(.*)""")]
        public void ThenTheClientRecievesASuccessfulRPCCallbackForWithData(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client recieves an error RPC callback for ""(.*)"" with the message ""(.*)""")]
        public void ThenTheClientRecievesAnErrorRPCCallbackForWithTheMessage(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
    }
}
