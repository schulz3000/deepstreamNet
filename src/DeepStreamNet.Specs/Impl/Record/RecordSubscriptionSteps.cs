using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Record
{
    [Binding]
    public class RecordSubscriptionSteps
    {
        [Given(@"the client unsubscribes to the entire record ""(.*)"" changes")]
        public void GivenTheClientUnsubscribesToTheEntireRecordChanges(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the client unsubscribes to ""(.*)"" for the record ""(.*)""")]
        public void GivenTheClientUnsubscribesToForTheRecord(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|A\|S\|subscribeRecord\+")]
        public void WhenTheServerSendsTheMessageRASSubscribeRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|R\|subscribeRecord\|(.*)\|\{""(.*)"":""(.*)"",""(.*)"":\[\{""(.*)"":""(.*)"",""(.*)"":""(.*)"",""(.*)"":(.*)}]}\+")]
        public void WhenTheServerSendsTheMessageRRSubscribeRecord(int p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client subscribes to the entire record ""(.*)"" changes")]
        public void WhenTheClientSubscribesToTheEntireRecordChanges(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|U\|subscribeRecord\|(.*)\|\{""(.*)"":""(.*)"",""(.*)"":\[\{""(.*)"":""(.*)"",""(.*)"":""(.*)"",""(.*)"":(.*)}]}\+")]
        public void WhenTheServerSendsTheMessageRUSubscribeRecord(int p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|P\|subscribeRecord\|(.*)\|pets\.(.*)\.name\|SRuffusTheSecond\+")]
        public void WhenTheServerSendsTheMessageRPSubscribeRecordPets_NameSRuffusTheSecond(int p0, int p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client subscribes to ""(.*)"" for the record ""(.*)""")]
        public void WhenTheClientSubscribesToForTheRecord(string p0, string p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|P\|subscribeRecord\|(.*)\|name\|SJohn Smith\+")]
        public void WhenTheServerSendsTheMessageRPSubscribeRecordNameSJohnSmith(int p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|U\|subscribeRecord\|(.*)\|\{""(.*)"":""(.*)"", ""(.*)"": (.*), ""(.*)"": \[\{""(.*)"":""(.*)"", ""(.*)"":""(.*)"",""(.*)"":(.*)}]}\+")]
        public void WhenTheServerSendsTheMessageRUSubscribeRecord(int p0, string p1, string p2, string p3, int p4, string p5, string p6, string p7, string p8, string p9, string p10, int p11)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|P\|subscribeRecord\|(.*)\|pets\.(.*)\.age\|N(.*)\+")]
        public void WhenTheServerSendsTheMessageRPSubscribeRecordPets_AgeN(int p0, int p1, int p2)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|CR\|subscribeRecord\+")]
        public void ThenTheLastMessageTheServerRecievedIsRCRSubscribeRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client will not be notified of the record change")]
        public void ThenTheClientWillNotBeNotifiedOfTheRecordChange()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client will be notified of the record change")]
        public void ThenTheClientWillBeNotifiedOfTheRecordChange()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client will be notified of the partial record change")]
        public void ThenTheClientWillBeNotifiedOfThePartialRecordChange()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client will be notified of the second record change")]
        public void ThenTheClientWillBeNotifiedOfTheSecondRecordChange()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
