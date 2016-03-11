using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Record
{
    [Binding]
    public class RecordStepsMisc
    {
        [When(@"the client creates a record named ""(.*)""")]
        public void WhenTheClientCreatesARecordNamed(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|A\|S\|doubleRecord\+")]
        public void WhenTheServerSendsTheMessageRASDoubleRecord()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message R\|R\|doubleRecord\|(.*)\|\{""(.*)"":""(.*)"", ""(.*)"": \[\{""(.*)"":""(.*)"", ""(.*)"":""(.*)"",""(.*)"":(.*)}]}\+")]
        public void WhenTheServerSendsTheMessageRRDoubleRecord(int p0, string p1, string p2, string p3, string p4, string p5, string p6, string p7, string p8, int p9)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|CR\|doubleRecord\+")]
        public void ThenTheLastMessageTheServerRecievedIsRCRDoubleRecord()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
