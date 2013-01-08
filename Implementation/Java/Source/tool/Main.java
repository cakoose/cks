package cks.tool;

import cks.io.*;

import cks.gen.haskell.HaskellGenerator;
import cks.gen.java.JavaGenerator;
import cks.gen.csharp.CSharpGenerator;

import cks.surface.SurfaceParser;
import cks.surface.model.Value;
import cks.dynamic.GBinaryReader;
import cks.dynamic.GSurfaceConverter;
import cks.dynamic.GValue;

import cks.type.model.*;
import cks.type.parser.TypeParser;

import cks.Maybe;
import cakoose.util.app.CommandLineLauncher;
import cakoose.util.app.CommandLineLauncher.Exit;
import cks.type.printer.TypePrinter;
import cks.type.resolver.TypeResolver;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.Closeable;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.io.PrintWriter;
import java.io.IOException;
import java.io.File;
import java.io.InputStream;

public final class Main extends CommandLineLauncher.TextOutput
{
	public static void printUsage(PrintWriter out)
	{
		out.println("CKS Tool");
		out.println();
		out.println("Usage: cks <sub-command> [sub-command args...]");
		out.println();
		out.println("Check CKS types:");
		out.println("  check-type <type-file>");
		out.println();
		out.println("Read/write CKS values:");
		out.println("  parse <text-input-file>");
		out.println("  check   <type-file> <type-name> <input-spec>");
		out.println("  convert <type-file> <type-name> <input-spec> <output-spec>");
		out.println("  compare <type-file> <type-name> <input-spec-1> <input-spec-2>");
		out.println();
		out.println("  input-spec: \"text:<file>\" or \"binary:<file>\"");
		out.println("  output-spec: \"text:<file>\" or \"texti:<file>\" or \"binary:<file>\"");
		out.println();
		out.println("Generate language-specific bindings for a CKS type definition:");
		out.println("  gen-java, gen-c#, gen-haskell");
		out.println();
	}

	public static void printUsageJava(PrintWriter out)
	{
		out.println();
		out.println("Usage: cks gen-java [options] <type-file> <package-name> <out-dir>");
		out.println();
		out.println("Options:");
		out.println("  -null-for-maybe: Use nullable references to denote optional values");
		out.println("     (instead of using the 'Maybe' data type).");
		out.println("  -no-binary: Don't generate code to read/write the binary format.");
		out.println("  -no-text: Don't generate code to read/write to text-based formats.");
		out.println("  -no-text-reader: Don't generate code to read text-based formats.");
		out.println("  -no-builder: Don't generate Builder classes for records.");
		out.println("  -surface: Generate code to convert from the surface AST.");
		out.println();
	}

	public static void printUsageCSharp(PrintWriter out)
	{
		out.println();
		out.println("Usage: cks gen-c# [options] <type-file> <namespace> <out-file>");
		out.println();
		out.println("Options:");
		out.println("  -null-for-maybe: Use nullable references to denote optional values");
		out.println("     (instead of using the 'Maybe' data type).");
		out.println();
	}

	public static void printUsageHaskell(PrintWriter out)
	{
		out.println();
		out.println("Usage: cks gen-haskell <type-file> <module-name> <out-file>");
		out.println();
	}

	public static void main(String[] args)
	{
		CommandLineLauncher.run(new Main(), args);
	}

	public void run(PrintWriter out, InputStream in, PrintWriter err, String[] args)
	{
		if (args.length == 0) {
			printUsage(err);
			throw new Exit(0);
		}

		String subCommand = args[0];
		if (subCommand.equals("parse-type")) {
			checkAdditional(err, args, 1);
			doParseType(err, out, args[1]);
		}
		else if (subCommand.equals("check-type")) {
			checkAdditional(err, args, 1);
			doCheckType(err, out, args[1]);
		}
		else if (subCommand.equals("parse")) {
			checkAdditional(err, args, 1);
			doParseTextValue(err, out, args[1]);
		}
		else if (subCommand.equals("check")) {
			checkAdditional(err, args, 3);
			doCheck(err, args[1], args[2], args[3]);
		}
		else if (subCommand.equals("convert")) {
			checkAdditional(err, args, 4);
			doConvert(err, args[1], args[2], args[3], args[4]);
		}
		else if (subCommand.equals("compare")) {
			checkAdditional(err, args, 4);
			doCompare(err, out, args[1], args[2], args[3], args[4]);
		}
		else if (subCommand.equals("gen-java")) {
			doGenJava(err, args);
		}
		else if (subCommand.equals("gen-c#")) {
			doGenCSharp(err, args);
		}
		else if (subCommand.equals("gen-haskell")) {
			doGenHaskell(err, args);
		}
		else {
			err.println("Invalid sub-command \"" + subCommand + "\".");
			err.println("Run the command with no arguments for more help.");
			throw exit(1);
		}

	}

	private static void checkAdditional(PrintWriter err, String[] args, int numRequired)
		throws Exit
	{
		int numAvailable = args.length - 1;
		if (numRequired != numAvailable) {
			err.println("Error: Bad usage of \"" + args[0] + "\" sub-command.  Required arguments: " + numRequired + "; Given arguments: " + numAvailable + ".");
			throw exit(1);
		}
	}

	private static void doParseType(PrintWriter err, PrintWriter out, String fileName)
		throws Exit
	{
		TModule module = parseType(err, fileName);
		try {
			TypePrinter.print(new cks.io.IndentPrinter(out), module);
		}
		catch (IOException ex) {
			err.println("Error writing to standard output: " + ex.getMessage());
			throw exit(1);
		}
	}

	private static void doCheckType(PrintWriter err, PrintWriter out, String fileName)
		throws Exit
	{
		TModule module = parseAndCheckType(err, fileName);
		try {
			TypePrinter.print(new cks.io.IndentPrinter(out), module);
		}
		catch (IOException ex) {
			err.println("Error writing to standard output: " + ex.getMessage());
			throw exit(1);
		}
	}

	private static void doParseTextValue(PrintWriter err, PrintWriter out, String fileName)
		throws Exit
	{
		Value value = parseTextValue(err, fileName);
		value.dump(out);
	}

	private static void doCheck(PrintWriter err, String typeFileName, String typeName, String inputSpecString)
	{
		Impls<GValue> impls = getImpls(err, typeFileName, typeName);
		InputSpec inputSpec = Impls.parseInputSpec(err, inputSpecString, "input-spec");

        impls.readValue(err, inputSpec);
	}

	public static Impls<GValue> getImpls(PrintWriter err, String typeFileName, String typeName)
	{
		TModule module = parseAndCheckType(err, typeFileName);

		// A module may define many types.  Figure out which one they want to check against.
		// TODO: Right now you can only check against an unparameterized top-level type.  We need
		// to allow them to specify any type expression.

		TDef def = module.defs.get(typeName);
		if (def == null) {
			err.println("No top-level type named \"" + typeName + "\" is defined in \"" + typeFileName + "\"");
			throw exit(2);
		}
		if (!def.getKind().equals(TKind.Base)) {
			err.println("Can't use the top-level type \"" + def.name + "\".  It requires type arguments.");
			throw exit(2);
		}

		// Create a reference to the type they requested.
		final TExpr expr = new TExpr(new TExpr.Ref.Direct(new SourcePos(1, 1), def));

		Impls.Reader<GValue> textReader = new Impls.Reader.Text<GValue>() {
			@Override
			public GValue read(Reader in) throws IOException, ProblemException
			{
                Value parsedValue = SurfaceParser.parse(new CksTextTokenizer(in));
                return GSurfaceConverter.read(expr, parsedValue);
			}
		};

		Impls.Reader<GValue> binaryReader = new Impls.Reader.Binary<GValue>() {
			@Override
			public GValue readForReal(InputStream in) throws IOException, BinaryReader.FormatException
			{
				return GBinaryReader.read(expr, in);
			}
		};

		Impls.Writer<GValue> textWriter = new Impls.Writer.CksText<GValue>() {
			@Override
			public void write(Formatter f, GValue value) throws IOException
			{
				value.write(f);
			}
		};

		Impls.Writer<GValue> textIndentedWriter = new Impls.Writer.CksTextIndented<GValue>() {
			@Override
			public void write(Formatter f, GValue value) throws IOException
			{
				value.write(f);
			}
		};

		Impls.Writer<GValue> binaryWriter = new Impls.Writer.Binary<GValue>() {
			@Override
			public void writeForReal(OutputStream out, GValue value) throws IOException
			{
				value.writeBinary(out);
			}
		};

		return new Impls<GValue>(
			new Impls.Writers<GValue>(binaryWriter, textWriter, textIndentedWriter),
			new Impls.Readers<GValue>(binaryReader, textReader));
	}

	private static void doConvert(PrintWriter err, String typeFileName, String typeName, String inputSpec, String outputSpec)
	{
		getImpls(err, typeFileName, typeName).convert(err, inputSpec, outputSpec);
	}

	private static void doCompare(PrintWriter err, PrintWriter out, String typeFileName, String typeName, String input1, String input2)
	{
		Impls<GValue> impls = getImpls(err, typeFileName, typeName);
		impls.compare(err, out, input1, input2);
	}

	private static void doGenJava(PrintWriter err, String[] args)
	{
		String command = args[0];
		if (args.length == 1) {
			printUsageJava(err);
			throw exit(1);
		}

		boolean nullForMaybe = false;
		boolean noBinary = false;
		boolean noBuilder = false;
		boolean surfaceConverter = false;
		JavaGenerator.Options.TextSupport textSupport = JavaGenerator.Options.TextSupport.ReadWrite;

		ArrayList<String> nonOptions = new ArrayList<String>();

		for (int i = 1; i < args.length; i++) {
			String arg = args[i];
			if (arg.startsWith("-")) {
				if (arg.equals("-null-for-maybe")) {
					nullForMaybe = true;
				} else if (arg.equals("-no-binary")) {
					if (noBinary) {
						err.println("Duplicate -no-binary option.");
						throw exit(1);
					}
					noBinary = true;
				} else if (arg.equals("-no-builder")) {
					if (noBuilder) {
						err.println("Duplicate -no-builder option.");
						throw exit(1);
					}
					noBuilder = true;
				} else if (arg.equals("-surface")) {
					if (surfaceConverter) {
						err.println("Duplicate -surface option.");
						throw exit(1);
					}
					surfaceConverter = true;
				} else if (arg.equals("-no-text-reader")) {
					if (textSupport != JavaGenerator.Options.TextSupport.ReadWrite) {
						err.println("Duplicate -no-text or -no-text-reader option.");
						throw exit(1);
					}
					textSupport = JavaGenerator.Options.TextSupport.WriteOnly;
				} else if (arg.equals("-no-text")) {
					if (textSupport != JavaGenerator.Options.TextSupport.ReadWrite) {
						err.println("Duplicate -no-text or -no-text-reader option.");
						throw exit(1);
					}
					textSupport = JavaGenerator.Options.TextSupport.None;
				} else {
					err.println("Unknown option: \"" + arg + "\".");
					err.println("Run \"cks " + command + "\" for help.");
					throw exit(1);
				}
			}
			else {
				nonOptions.add(arg);
			}
		}

		if (nonOptions.size() != 3) {
			err.println("Expecting 3 non-option arguments, found " + nonOptions.size() + ".");
			err.println("Run \"cks " + command + "\" for help.");
			throw exit(1);
		}

		String fileName = nonOptions.get(0);
		String packageName = nonOptions.get(1);
		String outDir = nonOptions.get(2);

		TModule module = parseAndCheckType(err, fileName);

		File f = new File(outDir);
		if (!f.exists()) {
			boolean ok = f.mkdirs();
			if (!ok) {
				err.println("Unable to create output directory \"" + f.getPath() + "\"");
				throw exit(2);
			}
		}
		else if (!f.isDirectory()) {
			err.println("Output directory \"" + f.getPath() + "\" already exists but isn't a directory.");
			throw exit(2);
		}

		JavaGenerator.Options options = new JavaGenerator.Options(nullForMaybe, !noBinary, textSupport, surfaceConverter, !noBuilder);
		try {
			Maybe<List<Problem>> generationProblems = JavaGenerator.generate(options, f, packageName, module);
			checkProblems(err, generationProblems);
		}
		catch (IOException ex) {
			err.println("Error writing out Java binding files: " + ex.getMessage());
			throw exit(2);
		}
	}

	private static void doGenCSharp(final PrintWriter err, String[] args)
	{
		String command = args[0];
		if (args.length == 1) {
			printUsageCSharp(err);
			throw exit(1);
		}

		boolean nullForMaybe = false;

		ArrayList<String> nonOptions = new ArrayList<String>();

		for (int i = 1; i < args.length; i++) {
			String arg = args[i];
			if (arg.startsWith("-")) {
				if (arg.equals("-null-for-maybe")) {
					nullForMaybe = true;
				} else {
					err.println("Unknown option: \"" + arg + "\".");
					err.println("Run \"cks " + command + "\" for help.");
					throw exit(1);
				}
			}
			else {
				nonOptions.add(arg);
			}
		}

		if (nonOptions.size() != 3) {
			err.println("Expecting 3 non-option arguments, found " + nonOptions.size() + ".");
			err.println("Run \"cks " + command + "\" for help.");
			throw exit(1);
		}

		String fileName = nonOptions.get(0);
		final String namespaceName = nonOptions.get(1);
		String outFile = nonOptions.get(2);

		final TModule module = parseAndCheckType(err, fileName);

		final CSharpGenerator.Options options = new CSharpGenerator.Options(nullForMaybe);

		withFile(err, "output file \"" + outFile + "\"", new Opener.FileWriter(outFile), new IOAction<Writer,Void>() {
			public Void run(Writer w) throws IOException
			{
				Maybe<List<Problem>> generationProblems = CSharpGenerator.generate(options, w, namespaceName, module);
				checkProblems(err, generationProblems);
				return null;
			}
		});
	}

	private static void doGenHaskell(final PrintWriter err, String[] args)
	{
		String command = args[0];
		if (args.length == 1) {
			printUsageHaskell(err);
			throw exit(1);
		}

		ArrayList<String> nonOptions = new ArrayList<String>();

		for (int i = 1; i < args.length; i++) {
			String arg = args[i];
			if (arg.startsWith("-")) {
				err.println("Unknown option: \"" + arg + "\".");
				err.println("Run \"cks " + command + "\" for help.");
				throw exit(1);
			}
			else {
				nonOptions.add(arg);
			}
		}

		if (nonOptions.size() != 3) {
			err.println("Expecting 3 non-option arguments, found " + nonOptions.size() + ".");
			err.println("Run \"cks " + command + "\" for help.");
			throw exit(1);
		}

		String fileName = nonOptions.get(0);
		final String moduleName = nonOptions.get(1);
		String outFile = nonOptions.get(2);

		final TModule module = parseAndCheckType(err, fileName);

		withFile(err, "output file \"" + outFile + "\"", new Opener.FileWriter(outFile), new IOAction<Writer, Void>()
		{
			public Void run(Writer w)
				throws IOException
			{
				Maybe<List<Problem>> generationProblems = HaskellGenerator.generate(w, moduleName, module);
				checkProblems(err, generationProblems);
				return null;
			}
		});
	}

	private static void checkProblems(PrintWriter err, Maybe<List<Problem>> maybeProblems)
		throws Exit
	{
		if (maybeProblems.isNothing()) return;
		printProblems(err, maybeProblems.getJust());
		throw exit(2);
	}

	private static void printProblems(PrintWriter err, List<Problem> problems)
		throws Exit
	{
		assert !problems.isEmpty();
		for (Problem problem : problems) {
			err.println(problem.toString());
		}
	}

	private static TModule parseType(PrintWriter err, String fileName)
		throws Exit
	{
		return processFile(err, "type definition", fileName, new TokenProcessor<TModule>() {
			public TModule run(CksTextTokenizer ts) throws IOException, ProblemException {
				return TypeParser.parse(ts);
			}
		});
	}

	public static TModule parseAndCheckType(PrintWriter err, String fileName)
		throws Exit
	{
		TModule module = parseType(err, fileName);

		// Try resolving.
		Maybe<List<Problem>> resolutionProblems = TypeResolver.run(TOpaque.StandardContext, module);
		checkProblems(err, resolutionProblems);

		// If we get here, everything succeeded.
		return module;
	}

	private static Value parseTextValue(PrintWriter err, String fileName)
		throws Exit
	{
		return processFile(err, "value definition", fileName, new TokenProcessor<Value>()
		{
			public Value run(CksTextTokenizer ts)
				throws IOException, ProblemException
			{
				return SurfaceParser.parse(ts);
			}
		});
	}

	private static <R> R processFile(final PrintWriter err, String fileContents, String fileName, final TokenProcessor<R> processor)
		throws Exit
	{
		return withFile(err, fileContents + " file \"" + fileName + "\"", new Opener.FileReader(fileName), new IOAction<Reader,R>() {
			public R run(Reader in) throws IOException
			{
				try {
					CksTextTokenizer ts = new CksTextTokenizer(in);
					return processor.run(ts);
				}
				catch (ProblemException ex) {
					printProblems(err, ex.problems);
					throw exit(1);
				}
			}
		});
	}

	private interface TokenProcessor<R>
	{
		R run(CksTextTokenizer ts) throws IOException, ProblemException;
	}

	private static <F extends Closeable,R> R withFile(PrintWriter err, String name, Opener<F> opener, IOAction<? super F,R> action)
	{
		F file;
		try {
			file = opener.run();
		}
		catch (IOException ex) {
			err.println("Error opening " + name + ": " + ex.getMessage());
			throw exit(1);
		}

		boolean closeError = false;
		R ret;
		try {
			ret = action.run(file);
		}
		catch (IOException ex) {
			err.println("Error accessing " + name + ": " + ex.getMessage());
			throw exit(1);
		}
		finally {
			try {
				file.close();
			}
			catch (IOException ex) {
				err.println("Error closing " + name + ": " + ex.getMessage());
				closeError = true;
			}
		}
		if (closeError) throw exit(1);
		return ret;
	}

	private static abstract class Opener<F extends Closeable>
	{
		public abstract F run() throws IOException;

		public static final class FileWriter extends Opener<java.io.Writer> {
			public final String name;
			public FileWriter(String name) { this.name = name; }
			public java.io.Writer run() throws IOException { return new java.io.OutputStreamWriter(new java.io.FileOutputStream(name), "UTF-8"); }
		}

		public static final class FileReader extends Opener<java.io.Reader> {
			public final String name;
			public FileReader(String name) { this.name = name; }
			public java.io.Reader run() throws IOException { return new java.io.InputStreamReader(new java.io.FileInputStream(name), "UTF-8"); }
		}
	}

	private static abstract class IOAction<F,T>
	{
		public abstract T run(F file) throws IOException;
	}
}
