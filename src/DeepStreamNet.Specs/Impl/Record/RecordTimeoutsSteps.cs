using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Record
{
    [Binding]
    public class RecordTimeoutsSteps
    {
        [Given(@"the server sends the message R\|A\|S\|unhappyRecord\+")]
        public void GivenTheServerSendsTheMessageRASUnhappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message R\|R\|unhappyRecord\|(.*)\|\{""(.*)"":\[""(.*)""]}\+")]
        public void GivenTheServerSendsTheMessageRRUnhappyRecord(int p0, string p1, string p2)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|A\|S\|unhappyRecord\+")]
        public void WhenTheServerSendsTheMessageRASUnhappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|R\|unhappyRecord\|(.*)\|\{""(.*)"":\[""(.*)""]}\+")]
        public void WhenTheServerSendsTheMessageRRUnhappyRecord(int p0, string p1, string p2)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client sets the record ""(.*)"" to \{""(.*)"":\[""(.*)""]}")]
        public void WhenTheClientSetsTheRecordTo(string p0, string p1, string p2)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|E\|CACHE_RETRIEVAL_TIMEOUT\|unhappyRecord\+")]
        public void WhenTheServerSendsTheMessageRECACHE_RETRIEVAL_TIMEOUTUnhappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|E\|STORAGE_RETRIEVAL_TIMEOUT\|unhappyRecord\+")]
        public void WhenTheServerSendsTheMessageRESTORAGE_RETRIEVAL_TIMEOUTUnhappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client discards the record named ""(.*)""")]
        public void WhenTheClientDiscardsTheRecordNamed(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client deletes the record named ""(.*)""")]
        public void WhenTheClientDeletesTheRecordNamed(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|U\|unhappyRecord\|(.*)\|\{""(.*)"":\[""(.*)""]}\+")]
        public void ThenTheLastMessageTheServerRecievedIsRUUnhappyRecord(int p0, string p1, string p2)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|US\|unhappyRecord\+")]
        public void ThenTheLastMessageTheServerRecievedIsRUSUnhappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|D\|unhappyRecord\+")]
        public void ThenTheLastMessageTheServerRecievedIsRDUnhappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
