using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Events
{
    [Binding]
    public class EventsMiscSteps
    {
        [When(@"the client subscribes to an event named ""(.*)""")]
        public void WhenTheClientSubscribesToAnEventNamed(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message E\|A\|S\|test(.*)\+")]
        public void WhenTheServerSendsTheMessageEASTest(int p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server received the message E\|S\|test(.*)\+")]
        public void ThenTheServerReceivedTheMessageESTest(int p0)
        {
            ScenarioContext.Current.Pending();
        }
    }
}
