using System;
using System.Diagnostics;
using System.IO;
using System.Threading;

namespace DeepStreamNet.Tests
{
    public class DeepStreamServerFixture : IDisposable
    {
        static int ProcessId = -1;

        readonly Process process;

        readonly string DeepstreamServerStartFile;

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
                return;

            if (ProcessId != -1 && IsProcessRunning(ProcessId))
                return;

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

        static bool IsProcessRunning(int processId)
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
            process?.Kill();
            process?.Dispose();

            if (File.Exists(DeepstreamServerStartFile))
            {
                File.Delete(DeepstreamServerStartFile);
            }
        }
    }
}
