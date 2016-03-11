using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs
{
    [Binding]
    public class EventsSteps
    {
        [Given(@"the client subscribes to an event named ""(.*)""")]
        public void GivenTheClientSubscribesToAnEventNamed(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message E\|A\|S\|test(.*)\+")]
        public void GivenTheServerSendsTheMessageEASTest(int p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message E\|A\|US\|test(.*)\+")]
        public void GivenTheServerSendsTheMessageEAUSTest(int p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message E\|EVT\|test(.*)\|SsomeValue\+")]
        public void WhenTheServerSendsTheMessageEEVTTestSsomeValue(int p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message E\|EVT\|test(.*)\|SanotherValue\+")]
        public void WhenTheServerSendsTheMessageEEVTTestSanotherValue(int p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client publishes an event named ""(.*)"" with data ""(.*)""")]
        public void WhenTheClientPublishesAnEventNamedWithData(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client unsubscribes from an event named ""(.*)""")]
        public void WhenTheClientUnsubscribesFromAnEventNamed(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client received the event ""(.*)"" with data ""(.*)""")]
        public void ThenTheClientReceivedTheEventWithData(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server received the message E\|EVT\|test(.*)\|SyetAnotherValue\+")]
        public void ThenTheServerReceivedTheMessageEEVTTestSyetAnotherValue(int p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server received the message E\|US\|test(.*)\+")]
        public void ThenTheServerReceivedTheMessageEUSTest(int p0)
        {
            ScenarioContext.Current.Pending();
        }
    }
}
