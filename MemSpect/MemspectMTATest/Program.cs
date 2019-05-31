using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using MemSpect;
using System.Threading;

namespace MemspectMTATest
{
    class Program
    {
        static int Main(string[] args)
        {
            bool success = true;

            if (args.Length != 1)
            {
                PrintUsage();
                return -1;
            }

            MemspectDriver driver = new MemspectDriver();
            MemSpect.Common.StatusMessageEvent += (o, e) =>
            {
                if (e.MsgType == Common.StatusMessageType.LogEntry)
                {
                    Console.WriteLine(e.Message);
                }
            };

            for (int i = 0; i < 14 && success; i++)
            {
                System.Threading.ParameterizedThreadStart GetSeqNoDelegate = new System.Threading.ParameterizedThreadStart(driver.GetSeqNo);
                System.Threading.Thread tempThread = new System.Threading.Thread(GetSeqNoDelegate);
                tempThread.Start(int.Parse(args[0]));
                while (tempThread.IsAlive)
                {
                    System.Threading.Thread.Sleep(1000);
                }
                tempThread.Abort();
                var lastSeqNo = driver.LastSeqNo;
                System.Console.WriteLine("SeqNo: " + driver.LastSeqNo.ToString());
                success = lastSeqNo > 0;
                driver.LastSeqNo = 0;

                //tempThread = new System.Threading.Thread(GetSeqNoDelegate);
                //tempThread.Start(int.Parse(args[0]));
                //while (tempThread.IsAlive)
                //{
                //    System.Threading.Thread.Sleep(1000);
                //}
                //tempThread.Abort();
                //lastSeqNo = driver.LastSeqNo;
                //System.Console.WriteLine("SeqNo: " + driver.LastSeqNo.ToString());
                //success = lastSeqNo > 0;
            }

            if (success)
                return 0;
            else
                return -1;
        }

        static void PrintUsage()
        {
            System.Console.WriteLine("GetSequenceNum.exe <PID>");
        }
    }

    public class MemspectDriver
    {
        public uint LastSeqNo = 0;

        public void GetSeqNo(object param)
        {
            int pid = (int)param;
            var res = ProcComm.InitComm(new string[] { "", pid.ToString() });
            if (!string.IsNullOrEmpty(res))
            {
                Console.WriteLine("Error connecting " + res);
            }
            
            var seqNo = Common.GetGlobalPassCount();
            ProcComm.CloseComm();

            LastSeqNo = seqNo;
        }
    }
}
