using System;
using System.Diagnostics;
using System.Text.RegularExpressions;
using System.Xml;
using System.Globalization;

namespace Stacks
{
    public class XmlStackSourceWriter
    {
        public static void WriteStacks(XmlWriter writer, StackSource source)
        {
            writer.WriteStartElement("StackSource");
            writer.WriteStartElement("Frames");
            writer.WriteAttributeString("Count", source.CallFrameIndexLimit.ToString());
            for (int i = 0; i < source.CallFrameIndexLimit; i++)
            {
                writer.WriteStartElement("Frame");
                writer.WriteAttributeString("ID", i.ToString());
                var frameName = source.GetFrameName((StackSourceFrameIndex)i, true);

                writer.WriteString(frameName);
                writer.WriteEndElement();   // Frame
            }
            writer.WriteEndElement();   // Frames

            writer.WriteStartElement("Stacks");
            writer.WriteAttributeString("Count", source.CallStackIndexLimit.ToString());
            for (int i = 0; i < source.CallStackIndexLimit; i++)
            {
                writer.WriteStartElement("Stack");
                writer.WriteAttributeString("ID", i.ToString());
                var callerID = source.GetCallerIndex((StackSourceCallStackIndex)i);
                var FrameID = source.GetFrameIndex((StackSourceCallStackIndex)i);
                writer.WriteAttributeString("CallerID", ((int)callerID).ToString());
                writer.WriteAttributeString("FrameID", ((int)FrameID).ToString());
                writer.WriteEndElement();   // Stack
            }
            writer.WriteEndElement();   // Stacks

            writer.WriteStartElement("Samples");
            writer.WriteAttributeString("Count", source.SampleIndexLimit.ToString());
            // We use the invarient culture, otherwise if we encode in france and decode 
            // in english we get parse errors (this happened!);
            var invariantCulture = CultureInfo.InvariantCulture;
            source.ProduceSamples(delegate(StackSourceSample sample)
            {
                // <Sample ID="1" Time="3432.23" StackID="2" Metric="1" EventKind="CPUSample" />
                writer.WriteStartElement("Sample");
                writer.WriteAttributeString("ID", ((int)sample.SampleIndex).ToString());
                writer.WriteAttributeString("Time", sample.TimeRelMSec.ToString("f3", invariantCulture));
                writer.WriteAttributeString("StackID", ((int)sample.StackIndex).ToString());
                if (sample.Metric != 1)
                {
                    var asInt = (int)sample.Metric;
                    if (sample.Metric == asInt)
                        writer.WriteAttributeString("Metric", asInt.ToString());
                    else
                        writer.WriteAttributeString("Metric", sample.Metric.ToString("f3", invariantCulture));
                }
                writer.WriteEndElement();
            });
            writer.WriteEndElement(); // Samples
            writer.WriteEndElement(); // StackSource
        }
    }

    /// <summary>
    /// Reads a very reasonable XML encoding of a stack source. 
    /// </summary>
    class XmlStackSource : StackSource
    {
        public XmlStackSource(string fileName)
        {
            XmlReaderSettings settings = new XmlReaderSettings();
            settings.IgnoreComments = true;
            settings.IgnoreWhitespace = true;

            using (XmlReader reader = XmlTextReader.Create(fileName, settings))
            {
                Read(reader);
            }
        }
        public XmlStackSource(XmlReader reader)
        {
            Read(reader);
        }

        // TODO intern modules 
        public override void ProduceSamples(Action<StackSourceSample> callback)
        {
            for (int i = 0; i < m_samples.Length; i++)
                callback(m_samples[i]);
        }
        public override StackSourceCallStackIndex GetCallerIndex(StackSourceCallStackIndex callStackIndex)
        {
            return (StackSourceCallStackIndex)m_stacks[(int)callStackIndex].callerID;
        }
        public override StackSourceFrameIndex GetFrameIndex(StackSourceCallStackIndex callStackIndex)
        {
            return (StackSourceFrameIndex)m_stacks[(int)callStackIndex].frameID;
        }
        public override string GetFrameName(StackSourceFrameIndex frameIndex, bool verboseName)
        {
            var ret = m_frames[(int)frameIndex];
            if (!verboseName)
            {
                var m = Regex.Match(ret, @"([^\\]+!.*)");
                if (m.Success)
                    ret = m.Groups[1].Value;
            }
            return ret;
        }
        public override int CallStackIndexLimit
        {
            get { return m_stacks.Length; }
        }
        public override int CallFrameIndexLimit
        {
            get { return m_frames.Length; }
        }
        public override int SampleIndexLimit
        {
            get { return m_samples.Length; }
        }
        public override double SampleTimeRelMSecLimit
        {
            get { return m_maxTime; }
        }
        public override StackSourceSample GetSampleByIndex(StackSourceSampleIndex sampleIndex)
        {
            return m_samples[(int)sampleIndex];
        }

        #region private
        private void Read(XmlReader reader)
        {
            // We use the invarient culture, otherwise if we encode in france and decode 
            // in english we get parse errors (this happened!);
            var invariantCulture = CultureInfo.InvariantCulture;
            while (reader.Read())
            {
                switch (reader.NodeType)
                {
                    case XmlNodeType.Element:
                        if (reader.Name == "Sample")
                        {
                            var sample = new StackSourceSample(this);
                            sample.Metric = 1;
                            if (reader.MoveToFirstAttribute())
                            {
                                do
                                {
                                    if (reader.Name == "ID")
                                        sample.SampleIndex = (StackSourceSampleIndex)reader.ReadContentAsInt();
                                    else if (reader.Name == "Time")
                                        sample.TimeRelMSec = double.Parse(reader.ReadContentAsString(), invariantCulture);
                                    else if (reader.Name == "StackID")
                                        sample.StackIndex = (StackSourceCallStackIndex)reader.ReadContentAsInt();
                                    else if (reader.Name == "Metric")
                                        sample.Metric = float.Parse(reader.ReadContentAsString(), invariantCulture);
                                } while (reader.MoveToNextAttribute());
                            }
                            m_samples[m_curSample++] = sample;
                            if (sample.TimeRelMSec > m_maxTime)
                                m_maxTime = sample.TimeRelMSec;
                        }
                        if (reader.Name == "Stack")
                        {
                            var stackID = -1;
                            var callerID = -1;
                            var frameID = -1;
                            if (reader.MoveToFirstAttribute())
                            {
                                do
                                {
                                    if (reader.Name == "ID")
                                        stackID = reader.ReadContentAsInt();
                                    else if (reader.Name == "FrameID")
                                        frameID = reader.ReadContentAsInt();
                                    else if (reader.Name == "CallerID")
                                        callerID = reader.ReadContentAsInt();
                                } while (reader.MoveToNextAttribute());
                            }
                            m_stacks[stackID].frameID = frameID;
                            m_stacks[stackID].callerID = callerID;
                        }
                        else if (reader.Name == "Frame")
                        {
                            var frameID = -1;
                            if (reader.MoveToFirstAttribute())
                            {
                                do
                                {
                                    if (reader.Name == "ID")
                                        frameID = reader.ReadContentAsInt();
                                } while (reader.MoveToNextAttribute());
                            }
                            reader.Read();      // Move on to body of the element
                            var frameName = reader.ReadContentAsString();
                            m_frames[frameID] = frameName;
                        }
                        else if (reader.Name == "Frames")
                        {
                            var count = reader.GetAttribute("Count");
                            m_frames = new string[int.Parse(count)];
                        }
                        else if (reader.Name == "Stacks")
                        {
                            var count = reader.GetAttribute("Count");
                            m_stacks = new Frame[int.Parse(count)];
#if DEBUG
                            for (int i = 0; i < m_stacks.Length; i++)
                            {
                                m_stacks[i].frameID = int.MinValue;
                                m_stacks[i].callerID = int.MinValue;
                            }
#endif
                        }
                        else if (reader.Name == "Samples")
                        {
                            var count = reader.GetAttribute("Count");
                            m_samples = new StackSourceSample[int.Parse(count)];
                        }
                        break;
                    case XmlNodeType.EndElement:
                    case XmlNodeType.Text:
                    default:
                        break;
                }
            }

#if DEBUG
            Debug.Assert(m_samples != null && m_frames != null && m_stacks != null);
            for (int i = 0; i < m_samples.Length; i++)
                Debug.Assert(m_samples[i] != null);
            for (int i = 0; i < m_frames.Length; i++)
                Debug.Assert(m_frames[i] != null);
            for (int i = 0; i < m_stacks.Length; i++)
            {
                Debug.Assert(m_stacks[i].frameID >= 0);
                Debug.Assert(m_stacks[i].callerID >= -1);
            }
#endif
        }

        struct Frame
        {
            public int callerID;
            public int frameID;
        }

        string[] m_frames;
        Frame[] m_stacks;
        StackSourceSample[] m_samples;
        int m_curSample;
        double m_maxTime;
        #endregion
    }
}