using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Record
{
    [Binding]
    public class RecordListenSteps
    {
        [Given(@"the server sends the message R\|SP\|recordPrefix/\.\*\|recordPrefix/foundAMatch\+")]
        public void GivenTheServerSendsTheMessageRSPRecordPrefix_RecordPrefixFoundAMatch()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message R\|SR\|recordPrefix/\.\*\|recordPrefix/foundAMatch\+")]
        public void GivenTheServerSendsTheMessageRSRRecordPrefix_RecordPrefixFoundAMatch()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client will be notified of new record match ""(.*)""")]
        public void ThenTheClientWillBeNotifiedOfNewRecordMatch(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client will be notified of record match removal ""(.*)""")]
        public void ThenTheClientWillBeNotifiedOfRecordMatchRemoval(string p0)
        {
            ScenarioContext.Current.Pending();
        }
    }
}
