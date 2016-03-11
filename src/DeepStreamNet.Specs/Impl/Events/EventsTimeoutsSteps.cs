using System;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs.Impl.Events
{
    [Binding]
    public class EventsTimeoutsSteps
    {
        [Given(@"some time passes")]
        public void GivenSomeTimePasses()
        {
            ScenarioContext.Current.Pending();
        }
    }
}
