package cks.test;

import cakoose.util.app.CommandLineLauncher;
import cks.io.BinaryReader;
import cks.io.BinaryWriter;
import cks.io.CksTextParser;
import cks.io.Formatter;
import cks.io.ProblemException;
import cks.io.TextReader;
import cks.io.TextWriter;
import cks.tool.Impls;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;

public class HarnessForGenerated
{
    public static <T> void run(
        String[] args,
        final BinaryReader<T> binaryReader,
        final BinaryWriter<T> binaryWriter,
        final TextReader<T> textReader,
        final TextWriter<T> textWriter)
    {
        Impls.Reader<T> textReaderW = new Impls.Reader.Text<T>() {
            @Override
            public T read(java.io.Reader in) throws IOException, ProblemException
            {
                return textReader.read(new CksTextParser(in));
            }
        };

        Impls.Reader<T> binaryReaderW = new Impls.Reader<T>() {
            @Override
            public T read(InputStream in) throws IOException, Impls.Reader.Exception
            {
                try {
                    return binaryReader.read(new BufferedInputStream(in));
                }
                catch (BinaryReader.FormatException ex) {
                    throw new Impls.Reader.Exception(ex.getMessage());
                }
            }
        };

        Impls.Writer<T> textWriterW = new Impls.Writer.CksText<T>() {
            @Override
            public void write(Formatter f, T value) throws IOException
            {
                textWriter.write(f, value);
            }
        };

        Impls.Writer<T> textIndentedWriterW = new Impls.Writer.CksTextIndented<T>() {
            @Override
            public void write(Formatter f, T value) throws IOException
            {
                textWriter.write(f, value);
            }
        };

        Impls.Writer<T> binaryWriterW = new Impls.Writer.Binary<T>() {
            @Override
            public void writeForReal(OutputStream out, T value) throws IOException
            {
                binaryWriter.write(out, value);
            }
        };

        final Impls<T> impls = new Impls<T>(
            new Impls.Writers<T>(binaryWriterW, textWriterW, textIndentedWriterW),
            new Impls.Readers<T>(binaryReaderW, textReaderW));

        CommandLineLauncher.run(new CommandLineLauncher.TextOutput() {
            @Override
            protected void run(PrintWriter out, InputStream in, PrintWriter err, String[] args)
            {
                Harness.run(out, in, err, args, "COMMAND ", impls);
            }
        }, args);
    }
}
