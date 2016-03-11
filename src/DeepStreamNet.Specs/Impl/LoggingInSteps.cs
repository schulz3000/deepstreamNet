using System;
using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TechTalk.SpecFlow;

namespace DeepStreamNet.Specs
{
    [Binding]
    public class LoggingInSteps
    {
        DeepStreamClient client;

        [Given(@"the test server is ready")]
        public void GivenTheTestServerIsReady()
        {
            //ready
        }
        
        [Given(@"the client is initialised")]
        public void GivenTheClientIsInitialised()
        {
            client = new DeepStreamClient("localhost",6021);
        }
        
        [Given(@"the server resets its message count")]
        public void GivenTheServerResetsItsMessageCount()
        {
            //reseted
        }
        
        [When(@"the client logs in with username ""(.*)"" and password ""(.*)""")]
        public async Task WhenTheClientLogsInWithUsernameAndPassword(string username, string password)
        {
            var result = await client.LoginAsync(username,password);
            Assert.IsTrue(result);
        }
        
        [When(@"the server sends the message A\|A\+")]
        public void WhenTheServerSendsTheMessageAA()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message A\|E\|INVALID_AUTH_MSG\|Sinvalid authentication message\+")]
        public void WhenTheServerSendsTheMessageAEINVALID_AUTH_MSGSinvalidAuthenticationMessage()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message A\|E\|INVALID_AUTH_DATA\|Sinvalid authentication data\+")]
        public void WhenTheServerSendsTheMessageAEINVALID_AUTH_DATASinvalidAuthenticationData()
        {
            ScenarioContext.Current.Pending();
        }
        
        [When(@"the server sends the message A\|E\|TOO_MANY_AUTH_ATTEMPTS\|Stoo many authentication attempts\+")]
        public void WhenTheServerSendsTheMessageAETOO_MANY_AUTH_ATTEMPTSStooManyAuthenticationAttempts()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last message the server recieved is A\|REQ\|\{""(.*)"":""(.*)"",""(.*)"":""(.*)""}\+")]
        public void ThenTheLastMessageTheServerRecievedIsAREQ(string p0, string p1, string p2, string p3)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the clients connection state is ""(.*)""")]
        public void ThenTheClientsConnectionStateIs(string state)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last login was successful")]
        public void ThenTheLastLoginWasSuccessful()
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the last login failed with error ""(.*)"" and message ""(.*)""")]
        public void ThenTheLastLoginFailedWithErrorAndMessage(string error, string message)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the server has received (.*) messages")]
        public void ThenTheServerHasReceivedMessages(int n)
        {
            ScenarioContext.Current.Pending();
        }
        
        [Then(@"the client throws a ""(.*)"" error with message ""(.*)""")]
        public void ThenTheClientThrowsAErrorWithMessage(string error, string message)
        {
            ScenarioContext.Current.Pending();
        }
    }
}
