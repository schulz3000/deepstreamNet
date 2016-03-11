using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs
{
    [Binding]
    public class ParsingMessagesSteps
    {
        [Given(@"the client logs in with username ""(.*)"" and password ""(.*)""")]
        public void GivenTheClientLogsInWithUsernameAndPassword(string username, string password)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message A\|A\+")]
        public void GivenTheServerSendsTheMessageAA()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message I only have one part\+")]
        public void WhenTheServerSendsTheMessageIOnlyHaveOnePart()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message B\|R\+")]
        public void WhenTheServerSendsTheMessageBR()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|XXX\+")]
        public void WhenTheServerSendsTheMessageRXXX()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
