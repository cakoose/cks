using Console = System.Console;
using IO = System.IO;
using Cks.Data;
using Cks.Text.Writer;
using Cks.Text.Reader;
using Cks.Binary.Writer;
using Cks.Binary.Reader;
using Cks.Text.Reader.Model;

public class Harness
{
	private sealed class InputSpec
	{
		public readonly FormatT Format;
		public readonly string File;

		public enum FormatT { Text, Binary, }

		public InputSpec(FormatT Format, string File)
		{
			this.Format = Format;
			this.File = File;
		}
	}

	private sealed class OutputSpec
	{
		public readonly FormatT Format;
		public readonly string File;

		public enum FormatT { Text, TextIndented, Binary, }

		public OutputSpec(FormatT Format, string File)
		{
			this.Format = Format;
			this.File = File;
		}
	}

	public static void Run<T>(
		string[] Args,
		BinaryReader<T> BinaryReader,
		BinaryWriter<T> BinaryWriter,
		TextReader<T> TextReader,
		TextWriter<T> TextWriter)
	{
		if (Args.Length != 0) {
			Console.Error.WriteLine("! Expecting zero arguments.");
			System.Environment.Exit(1);
		}

		while (true) {
			string Command = Console.ReadLine();
			if (Command == null) break;

			try {
				if (Command.Equals("convert")) {
					string InputString = Console.ReadLine();
					string OutputString = Console.ReadLine();

					InputSpec Input = ParseInputSpec(InputString, "input-spec");
					OutputSpec Output = ParseOutputSpec(OutputString, "output-spec");

					T Value = ReadValue(Console.Error, Input, BinaryReader, TextReader);
					WriteValue(Console.Error, Output, BinaryWriter, TextWriter, Value);
				}
				else if (Command.Equals("compare")) {
					string InputString1 = Console.ReadLine();
					string InputString2 = Console.ReadLine();

					InputSpec Input1 = ParseInputSpec(InputString1, "first input-spec");
					InputSpec Input2 = ParseInputSpec(InputString2, "second input-spec");


					T Value1 = ReadValue(Console.Error, Input1, BinaryReader, TextReader);
					T Value2 = ReadValue(Console.Error, Input2, BinaryReader, TextReader);

					Console.Out.WriteLine("> " + (Value1.Equals(Value2) ? "equal" : "not equal"));
				}
				else {
					Console.Error.WriteLine("! Unknown command: \"" + Command + "\"");
					System.Environment.Exit(1); break;
				}
			}
			catch (ExitException) {
				// Ignore it.
			}
			finally {
				Console.Out.WriteLine(">.");
				Console.Error.WriteLine("!.");
			}
		}
	}

	private static InputSpec ParseInputSpec(string Spec, string Name)
	{
		int ColonPos = Spec.IndexOf(':');
		if (ColonPos < 0) {
			Console.Error.WriteLine("! Invalid " + Name + ": missing colon");
			throw Exit(1);
		}

		string Format = Spec.Substring(0, ColonPos);
		string File = Spec.Substring(ColonPos+1);

		InputSpec.FormatT f;

		if (Format.Equals("text")) {
			f = InputSpec.FormatT.Text;
		}
		else if (Format.Equals("binary")) {
			f = InputSpec.FormatT.Binary;
		}
		else {
			Console.Error.WriteLine("! Invalid " + Name + ": \"" + Format + "\" is not a valid input format");
			throw Exit(1);
		}

		return new InputSpec(f, File);
	}

	private static OutputSpec ParseOutputSpec(string Spec, string Name)
	{
		int ColonPos = Spec.IndexOf(':');
		if (ColonPos < 0) {
			Console.Error.WriteLine("! Invalid " + Name + ": missing colon");
			throw Exit(1);
		}

		string Format = Spec.Substring(0, ColonPos);
		string File = Spec.Substring(ColonPos+1);

		OutputSpec.FormatT f;

		if (Format.Equals("text")) {
			f = OutputSpec.FormatT.Text;
		}
		else if (Format.Equals("texti")) {
			f = OutputSpec.FormatT.TextIndented;
		}
		else if (Format.Equals("binary")) {
			f = OutputSpec.FormatT.Binary;
		}
		else {
			Console.Error.WriteLine("! Invalid " + Name + ": \"" + Format + "\" is not a valid output format");
			throw Exit(1);
		}

		return new OutputSpec(f, File);
	}

	private static T ReadValue<T>(IO.TextWriter Err, InputSpec Input, BinaryReader<T> BinaryReader, TextReader<T> TextReader)
	{
		T Value;
		if (Input.Format == InputSpec.FormatT.Text) {
			Value = ReadText(Console.Error, Input.File, TextReader);
		}
		else if (Input.Format == InputSpec.FormatT.Binary) {
			Value = ReadBinary(Console.Error, Input.File, BinaryReader);
		}
		else {
			throw new System.ApplicationException("bad type: " + Input.Format);
		}
		return Value;
	}

	private static void WriteValue<T>(IO.TextWriter Err, OutputSpec Output, BinaryWriter<T> BinaryWriter, TextWriter<T> TextWriter, T Value)
	{
		if (Output.Format == OutputSpec.FormatT.Text) {
			WriteText(Console.Error, Output.File, TextWriter, Value);
		}
		else if (Output.Format == OutputSpec.FormatT.TextIndented) {
			WriteTextIndented(Console.Error, Output.File, TextWriter, Value);
		}
		else if (Output.Format == OutputSpec.FormatT.Binary) {
			WriteBinary(Console.Error, Output.File, BinaryWriter, Value);
		}
		else {
			throw new System.ApplicationException("bad type: " + Output.Format);
		}
	}

	private static T ReadBinary<T>(IO.TextWriter Err, string File, BinaryReader<T> BinaryReader)
	{
		try {
			IO.FileStream fs = IO.File.OpenRead(File);
			using (IO.BinaryReader In = new IO.BinaryReader(fs)) {
				T v = BinaryReader.Read(In);
				if (In.Read() != -1) {
					int ExtraBytes = 1;
					while (In.Read() != -1) ExtraBytes++;
					Err.WriteLine("! Error: extra bytes after payload: " + ExtraBytes + ".");
					throw Exit(2);
				}
				return v;
			}
		}
		catch (IO.IOException e) {
			Err.WriteLine("! Error reading from file \"" + File + "\": " + e.Message);
			throw Exit(2);
		}
		catch (BinaryFormatException e) {
			Err.WriteLine("! Binary format error: " + e.Message);
			throw Exit(2);
		}
	}

	private static T ReadText<T>(IO.TextWriter Err, string File, TextReader<T> TextReader)
	{
		try {
			using (IO.TextReader In = new IO.StreamReader(File)) {
				return TextReader.Read(In);
			}
		}
		catch (ProblemException e) {
			Err.WriteLine("! Marshal error: " + e.Problem);
			throw Exit(2);
		}
	}

	private static void WriteText<T>(IO.TextWriter Err, string File, TextWriter<T> TextWriter, T Value)
	{
		try {
			using (IO.TextWriter Out = new IO.StreamWriter(File)) {
				TextWriter.Write(Out, Value);
			}
		}
		catch (IO.IOException e) {
			Err.WriteLine("! Error writing to file \"" + File + "\": " + e.Message);
			throw Exit(2);
		}
	}

	private static void WriteTextIndented<T>(IO.TextWriter Err, string File, TextWriter<T> TextWriter, T Value)
	{
		try {
			using (IO.TextWriter Out = new IO.StreamWriter(File)) {
				TextWriter.WriteIndented(new IndentPrinter(Out), Value);
			}
		}
		catch (IO.IOException e) {
			Err.WriteLine("! Error writing to file \"" + File + "\": " + e.Message);
			throw Exit(2);
		}
	}

	private static void WriteBinary<T>(IO.TextWriter Err, string File, BinaryWriter<T> BinaryWriter, T Value)
	{
		try {
			IO.FileStream fs = IO.File.Open(File, IO.FileMode.Create, IO.FileAccess.Write);
			using (IO.BinaryWriter Out = new IO.BinaryWriter(fs)) {
				BinaryWriter.Write(Out, Value);
			}
		}
		catch (IO.IOException e) {
			Err.WriteLine("! Error writing to file \"" + File + "\": " + e.Message);
			throw Exit(2);
		}
	}

	private static System.ApplicationException Exit(int Code)
	{
		return new ExitException(Code);
	}

	private class ExitException : System.ApplicationException
	{
		public readonly int Code;
		public ExitException(int Code)
		{
			this.Code = Code;
		}
	}

}
