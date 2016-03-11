using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Record
{
    [Binding]
    public class RecordConnectivitySteps
    {
        [Given(@"the server sends the message R\|A\|S\|connectionRecord\+")]
        public void GivenTheServerSendsTheMessageRASConnectionRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message R\|R\|connectionRecord\|(.*)\|\{""(.*)"":""(.*)"", ""(.*)"": \[\{""(.*)"":""(.*)"", ""(.*)"":""(.*)"",""(.*)"":(.*)}]}\+")]
        public void GivenTheServerSendsTheMessageRRConnectionRecord(int p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message R\|A\|L\|recordPrefix/\.\*\+")]
        public void GivenTheServerSendsTheMessageRALRecordPrefix_()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client listens to a record matching ""(.*)""")]
        public void WhenTheClientListensToARecordMatching(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|CR\|connectionRecord\+")]
        public void ThenTheLastMessageTheServerRecievedIsRCRConnectionRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|L\|recordPrefix/\.\*\+")]
        public void ThenTheLastMessageTheServerRecievedIsRLRecordPrefix_()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server received the message R\|CR\|connectionRecord\+")]
        public void ThenTheServerReceivedTheMessageRCRConnectionRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server received the message R\|L\|recordPrefix/\.\*\+")]
        public void ThenTheServerReceivedTheMessageRLRecordPrefix_()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server received the message R\|P\|connectionRecord\|(.*)\|pets\.(.*)\.name\|SMax\+")]
        public void ThenTheServerReceivedTheMessageRPConnectionRecordPets_NameSMax(int p0, int p1)
        {
            ScenarioContext.Current.Pending();
        }
    }
}
