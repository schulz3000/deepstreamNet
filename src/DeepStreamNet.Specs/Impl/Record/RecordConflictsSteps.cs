using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Record
{
    [Binding]
    public class RecordConflictsSteps
    {
        [Given(@"the client creates a record named ""(.*)""")]
        public void GivenTheClientCreatesARecordNamed(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|A\|S\|mergeRecord\+")]
        public void WhenTheServerSendsTheMessageRASMergeRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|R\|mergeRecord\|(.*)\|\{""(.*)"":""(.*)""}\+")]
        public void WhenTheServerSendsTheMessageRRMergeRecord(int p0, string p1, string p2)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|U\|mergeRecord\|(.*)\|\{""(.*)"":""(.*)""}\+")]
        public void WhenTheServerSendsTheMessageRUMergeRecord(int p0, string p1, string p2)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client sets the record ""(.*)"" ""(.*)"" to ""(.*)""")]
        public void WhenTheClientSetsTheRecordTo(string p0, string p1, string p2)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|E\|VERSION_EXISTS\|mergeRecord\|(.*)\+")]
        public void WhenTheServerSendsTheMessageREVERSION_EXISTSMergeRecord(int p0)
        {
            ScenarioContext.Current.Pending();
        }
    }
}
