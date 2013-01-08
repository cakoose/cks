package cks.tool;

import cakoose.util.Either;
import cakoose.util.app.CommandLineLauncher;
import cks.io.BinaryReader;
import cks.io.CksTextFormatter;
import cks.io.CksTextIndentedFormatter;
import cks.io.Formatter;
import cks.io.IndentPrinter;
import cks.io.ProblemException;
import cks.io.StringUtil;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;

public final class Impls<T>
{
    public final Writers<T> writers;
    public final Readers<T> readers;

    public Impls(Writers<T> writers, Readers<T> readers)
    {
        this.writers = writers;
        this.readers = readers;
    }

    public static abstract class Writer<T>
	{
		public abstract void write(OutputStream out, T value)
			throws IOException;

		public static abstract class CksText<T> extends Writer<T>
		{
			public final void write(OutputStream out, T value) throws IOException
			{
				CksTextFormatter tf = new CksTextFormatter(out);
				write(tf, value);
				tf.flush();
			}

			public abstract void write(Formatter f, T value) throws IOException;
		}

		public static abstract class CksTextIndented<T> extends Writer<T>
		{
			public final void write(OutputStream out, T value) throws IOException
			{
				IndentPrinter ip = new IndentPrinter(out);
				write(new CksTextIndentedFormatter(ip), value);
				ip.flush();
			}

			public abstract void write(Formatter f, T value) throws IOException;
		}

		public static abstract class Binary<T> extends Writer<T>
		{
			public final void write(OutputStream out, T value) throws IOException
			{
				BufferedOutputStream bout = new BufferedOutputStream(out);
				writeForReal(bout, value);
				bout.flush();
			}

			public abstract void writeForReal(OutputStream out, T value) throws IOException;
		}
	}

    public static abstract class Reader<T>
	{
		public abstract T read(InputStream in)
			throws IOException, Reader.Exception;

		public static final class Exception extends java.lang.Exception
		{
			public Exception(String message)
			{
				super(message);
			}
		}

		public static abstract class Binary<T> extends Reader<T>
		{
			public final T read(InputStream in) throws IOException, Reader.Exception
			{
				try {
					return readForReal(new BufferedInputStream(in));
				}
				catch (BinaryReader.FormatException ex) {
					throw new Reader.Exception(ex.getMessage());
				}
			}

			public abstract T readForReal(InputStream in) throws IOException, BinaryReader.FormatException;
		}

		public static abstract class Text<T> extends Reader<T>
		{
			public final T read(InputStream in) throws IOException, Reader.Exception
			{
				try {
					return read(StringUtil.bufferedUtf8Reader(in));
				}
				catch (ProblemException ex) {
					throw new Impls.Reader.Exception(ex.getMessage());
				}
			}

			public abstract T read(java.io.Reader in) throws IOException, ProblemException;
		}
	}

    public static final class Writers<T>
	{
		public final Writer<T> binary;
		public final Writer<T> text;
		public final Writer<T> textIndented;

		public Writers(Writer<T> binary, Writer<T> text, Writer<T> textIndented)
		{
			this.binary = binary;
			this.text = text;
			this.textIndented = textIndented;
		}
	}

    public static final class Readers<T>
	{
		public final Reader<T> binary;
		public final Reader<T> text;

		public Readers(Reader<T> binary, Reader<T> text)
		{
			this.binary = binary;
			this.text = text;
		}
	}

	public void convert(PrintWriter err, String inputString, String outputString)
	{
		InputSpec input = parseInputSpec(err, inputString, "input-spec");
		OutputSpec output = parseOutputSpec(err, outputString, "output-spec");

		T value = readValue(err, input);
		writeValue(err, output, value);
	}

	public void compare(PrintWriter err, PrintWriter out, String inputString1, String inputString2)
	{
		InputSpec input1 = parseInputSpec(err, inputString1, "first input-spec");
		InputSpec input2 = parseInputSpec(err, inputString2, "second input-spec");

		T value1 = readValue(err, input1);
		T value2 = readValue(err, input2);

		out.println((value1.equals(value2) ? "equal" : "not equal"));
	}

	public static InputSpec parseInputSpec(PrintWriter err, String spec, String name)
	{
		Either<InputSpec,String> result = InputSpec.parse(spec);

		if (result.isLeft()) return result.getLeft();
		String error = result.getRight();

		err.println("Invalid " + name + ": " + error);
		throw new CommandLineLauncher.Exit(1);
	}

	public static OutputSpec parseOutputSpec(PrintWriter err, String spec, String name)
	{
		Either<OutputSpec,String> result = OutputSpec.parse(spec);

		if (result.isLeft()) return result.getLeft();
		String error = result.getRight();

		err.println("Invalid " + name + ": " + error);
		throw new CommandLineLauncher.Exit(1);
	}

	public T readValue(PrintWriter err, InputSpec input)
	{
		Impls.Reader<T> reader;
		if (input.format == InputSpec.Format.Text) {
			reader = readers.text;
		}
		else if (input.format == InputSpec.Format.Binary) {
			reader = readers.binary;
		}
		else {
			throw new AssertionError("bad type: " + input.format);
		}

		InputStream in;
		try {
			in = new FileInputStream(input.file);
			try {
				return reader.read(in);
			}
			finally {
				try { in.close(); } catch (IOException ex) { /* ignore */ }
			}
		}
		catch (IOException ex) {
			err.println("Error reading from file \"" + input.file + "\": " + ex.getMessage());
			throw new CommandLineLauncher.Exit(2);
		}
		catch (Impls.Reader.Exception ex) {
			err.println("Error in file \"" + input.file + "\": " + ex.getMessage());
			throw new CommandLineLauncher.Exit(2);
		}
	}

	public void writeValue(PrintWriter err, OutputSpec output, T value)
	{
		Impls.Writer<T> writer;
		if (output.format == OutputSpec.Format.Text) {
			writer = writers.text;
		}
		else if (output.format == OutputSpec.Format.TextIndented) {
			writer = writers.textIndented;
		}
		else if (output.format == OutputSpec.Format.Binary) {
			writer = writers.binary;
		}
		else {
			throw new AssertionError("bad type: " + output.format);
		}

		try {
			OutputStream out = new FileOutputStream(output.file);
			try {
				writer.write(out, value);
			}
			finally {
				out.close();
			}
		}
		catch (IOException ex) {
			err.println("Error writing to file \"" + output.file + "\": " + ex.getMessage());
			throw new CommandLineLauncher.Exit(2);
		}
	}
}
