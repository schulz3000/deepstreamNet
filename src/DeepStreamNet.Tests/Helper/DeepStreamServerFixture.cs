using System;
using System.Diagnostics;
using System.IO;
using System.Threading;

namespace DeepStreamNet.Tests.Helper
{
    public class DeepStreamServerFixture : IDisposable
    {
        private static int ProcessId = -1;

        private readonly Process process;

        private readonly string DeepstreamServerStartFile;

        public DeepStreamServerFixture()
        {
            if (bool.Parse(TestHelper.Config["useLocalInstance"]))
            {
                process = new Process();

                DeepstreamServerStartFile = Path.Combine(TestHelper.Config["deepStreamServerDirectory"], "start_deepstream_server.js");

                if (!File.Exists(DeepstreamServerStartFile))
                {
                    File.WriteAllText(DeepstreamServerStartFile, Properties.Resources.start_deepstream);
                }
            }
        }

        public void StartServer()
        {
            if (!bool.Parse(TestHelper.Config["useLocalInstance"]))
            {
                return;
            }

            if (ProcessId != -1 && IsProcessRunning(ProcessId))
            {
                return;
            }

            process.StartInfo = new ProcessStartInfo
            {
                FileName = "node",
                WorkingDirectory = TestHelper.Config["deepStreamServerDirectory"],
                Arguments = DeepstreamServerStartFile,
                CreateNoWindow = true
            };

            process.Start();
            ProcessId = process.Id;
            Thread.Sleep(5000);
        }

        private static bool IsProcessRunning(int processId)
        {
            try
            {
                Process.GetProcessById(processId);
            }
            catch
            {
                return false;
            }

            return true;
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                process?.Kill();
                process?.Dispose();

                if (File.Exists(DeepstreamServerStartFile))
                {
                    File.Delete(DeepstreamServerStartFile);
                }
            }
        }
    }
}
