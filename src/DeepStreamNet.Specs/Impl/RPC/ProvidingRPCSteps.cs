using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.RPC
{
    [Binding]
    public class ProvidingRPCSteps
    {
        [When(@"the server sends the message P\|REQ\|toUppercase\|(.*)\|Sabc\+")]
        public void WhenTheServerSendsTheMessagePREQToUppercaseSabc(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client responds to the RPC ""(.*)"" with data ""(.*)""")]
        public void WhenTheClientRespondsToTheRPCWithData(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client responds to the RPC ""(.*)"" with the error ""(.*)""")]
        public void WhenTheClientRespondsToTheRPCWithTheError(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client rejects the RPC ""(.*)""")]
        public void WhenTheClientRejectsTheRPC(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message P\|REQ\|unSupported\|(.*)\|Sabc\+")]
        public void WhenTheServerSendsTheMessagePREQUnSupportedSabc(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client stops providing a RPC called ""(.*)""")]
        public void WhenTheClientStopsProvidingARPCCalled(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message P\|A\|US\|toUppercase\+")]
        public void WhenTheServerSendsTheMessagePAUSToUppercase()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is P\|A\|toUppercase\|(.*)\+")]
        public void ThenTheLastMessageTheServerRecievedIsPAToUppercase(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client recieves a request for a RPC called ""(.*)"" with data ""(.*)""")]
        public void ThenTheClientRecievesARequestForARPCCalledWithData(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is P\|RES\|toUppercase\|(.*)\|SABC\+")]
        public void ThenTheLastMessageTheServerRecievedIsPRESToUppercaseSABC(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is P\|E\|An Error Occured\|toUppercase\|(.*)\+")]
        public void ThenTheLastMessageTheServerRecievedIsPEAnErrorOccuredToUppercase(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is P\|REJ\|toUppercase\|(.*)\+")]
        public void ThenTheLastMessageTheServerRecievedIsPREJToUppercase(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is P\|REJ\|unSupported\|(.*)\+")]
        public void ThenTheLastMessageTheServerRecievedIsPREJUnSupported(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is P\|US\|toUppercase\+")]
        public void ThenTheLastMessageTheServerRecievedIsPUSToUppercase()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
