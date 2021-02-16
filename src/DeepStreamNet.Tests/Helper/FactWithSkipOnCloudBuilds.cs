using System;
using Xunit;

namespace DeepStreamNet.Tests.Helper
{
    public sealed class FactWithSkipOnCloudBuilds : FactAttribute
    {
        public FactWithSkipOnCloudBuilds()
        {
            if(IsAppVeyor() || IsTravis() || IsGithubAction())
            {
                Skip = "Ignore on Cloud Builds";
            }
        }

        private static bool IsAppVeyor()
           => Environment.GetEnvironmentVariable("APPVEYOR") != null;

        private static bool IsTravis()
          => Environment.GetEnvironmentVariable("TRAVIS") != null;

        private static bool IsGithubAction()
          => Environment.GetEnvironmentVariable("GITHUB_WORKFLOW") != null;
    }
}
