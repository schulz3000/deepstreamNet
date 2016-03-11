using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Events
{
    [Binding]
    public class EventsConnectivitySteps
    {
        [Given(@"two seconds later")]
        public void GivenTwoSecondsLater()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the connection to the server is lost")]
        public void WhenTheConnectionToTheServerIsLost()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the connection to the server is reestablished")]
        public void WhenTheConnectionToTheServerIsReestablished()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is E\|S\|test(.*)\+")]
        public void ThenTheLastMessageTheServerRecievedIsESTest(int p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server did not recieve any messages")]
        public void ThenTheServerDidNotRecieveAnyMessages()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server received the message E\|L\|eventPrefix/\.\*\+")]
        public void ThenTheServerReceivedTheMessageELEventPrefix_()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
