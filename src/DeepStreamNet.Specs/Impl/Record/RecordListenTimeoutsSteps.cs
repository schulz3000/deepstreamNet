using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Record
{
    [Binding]
    public class RecordListenTimeoutsSteps
    {
        [When(@"the client unlistens to a record matching ""(.*)""")]
        public void WhenTheClientUnlistensToARecordMatching(string p0)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is R\|UL\|recordPrefix/\.\*\+")]
        public void ThenTheLastMessageTheServerRecievedIsRULRecordPrefix_()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
