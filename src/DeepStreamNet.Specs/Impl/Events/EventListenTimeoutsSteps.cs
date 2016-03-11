using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Events
{
    [Binding]
    public class EventListenTimeoutsSteps
    {
        [When(@"some time passes")]
        public void WhenSomeTimePasses()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
