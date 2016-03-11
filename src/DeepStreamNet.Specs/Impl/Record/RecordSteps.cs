using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Record
{
    [Binding]
    public class RecordSteps
    {
        [Given(@"the server sends the message R\|A\|S\|happyRecord\+")]
        public void GivenTheServerSendsTheMessageRASHappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the server sends the message R\|R\|happyRecord\|(.*)\|\{""(.*)"":""(.*)"", ""(.*)"": \[\{""(.*)"":""(.*)"", ""(.*)"":""(.*)"",""(.*)"":(.*)}]}\+")]
        public void GivenTheServerSendsTheMessageRRHappyRecord(int p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Given(@"the client deletes the record named ""(.*)""")]
        public void GivenTheClientDeletesTheRecordNamed(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|R\|happyRecord\|(.*)\|\{""(.*)"":""(.*)"", ""(.*)"": \[\{""(.*)"":""(.*)"", ""(.*)"":""(.*)"",""(.*)"":(.*)}]}\+")]
        public void WhenTheServerSendsTheMessageRRHappyRecord(int p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|P\|happyRecord\|(.*)\|pets\.(.*)\.age\|N(.*)\+")]
        public void WhenTheServerSendsTheMessageRPHappyRecordPets_AgeN(int p0, int p1, int p2)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|U\|happyRecord\|(.*)\|\{""(.*)"":""(.*)"", ""(.*)"": \[\{""(.*)"":""(.*)"", ""(.*)"":""(.*)"",""(.*)"":(.*)}]}\+")]
        public void WhenTheServerSendsTheMessageRUHappyRecord(int p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the client sets the record ""(.*)"" to \{""(.*)"":""(.*)"",""(.*)"":\[\{""(.*)"":""(.*)"",""(.*)"":""(.*)"",""(.*)"":(.*)}]}")]
        public void WhenTheClientSetsTheRecordTo(string p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|A\|US\|happyRecord\+")]
        public void WhenTheServerSendsTheMessageRAUSHappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|A\|D\|happyRecord\+")]
        public void WhenTheServerSendsTheMessageRADHappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|CR\|happyRecord\+")]
        public void ThenTheLastMessageTheServerRecievedIsRCRHappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client record ""(.*)"" data is \{""(.*)"":""(.*)"", ""(.*)"": \[\{""(.*)"":""(.*)"", ""(.*)"":""(.*)"",""(.*)"":(.*)}]}")]
        public void ThenTheClientRecordDataIs(string p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|P\|happyRecord\|(.*)\|pets\.(.*)\.name\|SMax\+")]
        public void ThenTheLastMessageTheServerRecievedIsRPHappyRecordPets_NameSMax(int p0, int p1)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|U\|happyRecord\|(.*)\|\{""(.*)"":""(.*)"",""(.*)"":\[\{""(.*)"":""(.*)"",""(.*)"":""(.*)"",""(.*)"":(.*)}]}\+")]
        public void ThenTheLastMessageTheServerRecievedIsRUHappyRecord(int p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|US\|happyRecord\+")]
        public void ThenTheLastMessageTheServerRecievedIsRUSHappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|D\|happyRecord\+")]
        public void ThenTheLastMessageTheServerRecievedIsRDHappyRecord()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
