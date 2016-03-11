using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Events
{
    [Binding]
    public class EventListenSteps
    {
        [Given(@"the server sends the message E\|A\|L\|eventPrefix/\.\*\+")]
        public void GivenTheServerSendsTheMessageEALEventPrefix_()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message E\|SP\|eventPrefix/\.\*\|eventPrefix/foundAMatch\+")]
        public void GivenTheServerSendsTheMessageESPEventPrefix_EventPrefixFoundAMatch()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message E\|SR\|eventPrefix/\.\*\|eventPrefix/foundAMatch\+")]
        public void GivenTheServerSendsTheMessageESREventPrefix_EventPrefixFoundAMatch()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message E\|A\|UL\|eventPrefix/\.\*\+")]
        public void GivenTheServerSendsTheMessageEAULEventPrefix_()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the client throws a ""(.*)"" error with message ""(.*)""")]
        public void GivenTheClientThrowsAErrorWithMessage(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client listens to events matching ""(.*)""")]
        public void WhenTheClientListensToEventsMatching(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client unlistens to events matching ""(.*)""")]
        public void WhenTheClientUnlistensToEventsMatching(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is E\|L\|eventPrefix/\.\*\+")]
        public void ThenTheLastMessageTheServerRecievedIsELEventPrefix_()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client will be notified of new event match ""(.*)""")]
        public void ThenTheClientWillBeNotifiedOfNewEventMatch(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client will be notified of event match removal ""(.*)""")]
        public void ThenTheClientWillBeNotifiedOfEventMatchRemoval(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is E\|UL\|eventPrefix/\.\*\+")]
        public void ThenTheLastMessageTheServerRecievedIsEULEventPrefix_()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
