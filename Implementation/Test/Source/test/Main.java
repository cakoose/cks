package cks.test;

import cakoose.util.app.CommandLineLauncher;
import cakoose.util.Maybe;
import cakoose.util.Pair;
import cakoose.util.text.StringUtil;
import cakoose.util.text.VersionComparator;
import static cakoose.util.LangUtil.badType;

import java.io.PrintWriter;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Set;
import java.util.TreeMap;

public class Main extends CommandLineLauncher.TextOutput
{
	public static void printUsage(PrintWriter out)
	{
		out.println();
		out.println("Usage: cks-test <impl-dir> <test-case-dir> <output-dir> <impls> [tests...]");
		out.println();
		out.println("impl-dir - The directory that contains the various CKS");
		out.println("   implementations for various languages.");
		out.println();
		out.println("test-case-dir - A directory with one subdirectory for each");
		out.println("   test group.  Each subdirectory should contain a single");
		out.println("   CKS type file and any number of CKS value files.");
		out.println();
		out.println("output-dir - The directory to use for test output.");
		out.println();
		out.println("impls - A comma-separated list of implementations to test.");
		out.println("   Available implementations: " + sepList(",", Implementations.keySet()));
		out.println();
		out.println("tests - A list of tests to run.  By default, all tests are run.");
		out.println("   Examples: \"Ints\", \"Ints/1,3,9\"");
		out.println();
	}

	public static void main(String[] args)
	{
		CommandLineLauncher.run(new Main(), args);
	}

	public static String sepList(String sep, Iterable<String> list)
	{
		StringBuilder buf = new StringBuilder();
		String m = "";
		for (String s : list) {
			buf.append(m); m = sep;
			buf.append(s);
		}
		return buf.toString();
	}

	private static final Map<String,Impl> Implementations = new LinkedHashMap<String,Impl>();
	static {
		Implementations.put("Java", Java.Impl);
		Implementations.put("Haskell", Haskell.Impl);
		Implementations.put("C#", CSharp.Impl);
		Implementations.put("Java-Null", Java.NullImpl);
		Implementations.put("Java-Dynamic", Java.DynamicImpl);
	}

	protected void run(PrintWriter out, InputStream in, PrintWriter err, String[] args)
	{
		if (args.length == 0) {
			printUsage(out);
			throw exit(1);
		}
		if (args.length < 4) {
			err.println("Expecting at least 4 arguments.  Run with no arguments for help.");
			throw exit(1);
		}

		// Make sure impl-dir exists.
		File implDir = new File(args[0]);
		if (!implDir.exists()) {
			err.println("impl-dir \"" + implDir.getPath() + "\" does not exist.");
			throw exit(1);
		}
		if (!implDir.isDirectory()) {
			err.println("impl-dir \"" + implDir.getPath() + "\" is not a directory.");
			throw exit(1);
		}

		List<String> cksToolCommand = Arrays.asList(implDir.getPath() + File.separatorChar + "Java" + File.separatorChar + "cks");
		checkCksTool(cksToolCommand);

		// Make sure test-case-dir exists.
		File testCaseDir = new File(args[1]);
		if (!testCaseDir.exists()) {
			err.println("test-case-dir \"" + testCaseDir.getPath() + "\" does not exist.");
			throw exit(1);
		}
		if (!testCaseDir.isDirectory()) {
			err.println("test-case-dir \"" + testCaseDir.getPath() + "\" is not a directory.");
			throw exit(1);
		}

		// Get the list of implementations they wanted to test.
		String[] implNames = args[3].split(",");
		Map<String, Generator> chosenImpls = new LinkedHashMap<String, Generator>();
		for (String implName : implNames) {
			Impl impl = Implementations.get(implName);
			if (impl == null) {
				err.println("Unknown implementation \"" + implName + "\".");
				throw exit(1);
			}

			Generator g = impl.makeGenerator(err, implDir);
			if (g == null) {
				throw exit(2);
			}

			Object displaced = chosenImpls.put(implName, g);
			if (displaced != null) {
				err.println("Implementation \"" + implName + "\" specified multiple times.");
				throw exit(1);
			}
		}

		// Prepare output directory.
		File outDir = new File(args[2]);
		prepareOutDir(err, outDir);

		File programBaseDir = new File(outDir, "Programs");
		Util.mkdirs(err, programBaseDir);

		File testOutDir = new File(outDir, "TestOutput");
		Util.mkdirs(err, testOutDir);

		// Determine the specified tests.
		Map<String,Maybe<Set<String>>> specifiedTests;
		if (args.length == 4) {
			specifiedTests = null; // Indicates: run all tests.
		} else {
			specifiedTests = new LinkedHashMap<String,Maybe<Set<String>>>();
			for (int argI = 4; argI < args.length; argI++) {
				String arg = args[argI];
				if (arg.length() == 0) {
					err.println("Invalid test spec \"" + arg + "\"");
					throw exit(1);
				}
				int slashPos = arg.indexOf('/');
				if (slashPos < 0) {
					String group = arg;
					specifiedTests.put(group, Maybe.<Set<String>>Nothing());
				}
				else {
					String group = arg.substring(0, slashPos);
					String testList = arg.substring(slashPos+1);
					if (group.length() == 0 || testList.length() == 0) {
						err.println("Invalid test spec \"" + arg + "\".");
						throw exit(1);
					}
					String[] testArray = testList.split(",");
					Maybe<Set<String>> maybeSpec = specifiedTests.get(group);
					if (maybeSpec == null) {
						Set<String> tests = new LinkedHashSet<String>();
						tests.addAll(Arrays.asList(testArray));
						specifiedTests.put(group, Maybe.Just(tests));
					} else if (maybeSpec.isJust()) {
						Set<String> tests = maybeSpec.getJust();
						tests.addAll(Arrays.asList(testArray));
					} else if (maybeSpec.isNothing()){
						// Do nothing.  The entire group is already included.
					} else {
						throw badType(maybeSpec);
					}
				}
			}
		}

		boolean allPass = runTests(out, err, cksToolCommand, testOutDir, programBaseDir, testCaseDir, chosenImpls, specifiedTests);

		boolean error = false;
		if (specifiedTests != null) {
			// List the tests that weren't found.
			for (Map.Entry<String,Maybe<Set<String>>> e : specifiedTests.entrySet()) {
				String groupName = e.getKey();
				Maybe<Set<String>> mTests = e.getValue();
				if (mTests.isNothing()) {
					err.println("Error: Couldn't find test group \"" + groupName + "\"");
					error = true;
				}
				else if (mTests.isJust()) {
					Set<String> tests = mTests.getJust();
					if (!tests.isEmpty()) {
						for (String test : tests) {
							err.println("Error: Couldn't find test \"" + groupName + "/" + test + "\"");
							error = true;
						}
					}
				}
			}
		}

		if (error) throw exit(1);

		if (allPass) {
			out.println("All tests passed.");
		} else {
			out.println("Some tests failed.");
		}
	}

	private void sortByName(File[] files)
	{
		TreeMap<String,File> sorter = new TreeMap<String,File>(VersionComparator.Strict);
		for (File f : files) {
			sorter.put(f.getName(), f);
		}
		int i = 0;
		for (File f : sorter.values()) {
			files[i++] = f;
		}
	}

	private boolean runTests(PrintWriter out, PrintWriter err, List<String> cksToolCommand, File testOutDir, File programBaseDir, File testCaseBaseDir, Map<String, Generator> impls, Map<String,Maybe<Set<String>>> filter)
	{
		File[] testGroupDirs = testCaseBaseDir.listFiles();
		sortByName(testGroupDirs);

		boolean allPass = true;

		for (File testGroupDir : testGroupDirs) {
			if (testGroupDir.isDirectory()) {
				allPass &= runTestGroup(out, err, cksToolCommand, testOutDir, programBaseDir, impls, testGroupDir, filter);
			}
		}

		return allPass;
	}

	private boolean runTestGroup(PrintWriter out, PrintWriter err, List<String> cksToolCommand, File testOutDir, File programBaseDir, Map<String, Generator> impls, File testGroupDir, Map<String,Maybe<Set<String>>> filter)
	{
		String groupName = testGroupDir.getName();

		Set<String> testFilter = null;
		if (filter != null) {
			Maybe<Set<String>> mTestFilter = filter.get(groupName);
			if (mTestFilter == null) {
				// Skip this group.
				return true;
			}
			if (mTestFilter.isNothing()) {
				// Run all tests in the group.
				filter.remove(groupName); // Remember the fact that we ran this test group.
			}
			else if (mTestFilter.isJust()) {
				// Run specified tests in the group.
				testFilter = mTestFilter.getJust();
			}
			else throw badType(mTestFilter);
		}

		File typeFile = new File(testGroupDir, groupName + ".tcks");
		if (!typeFile.exists()) {
			err.println("Expecting file \"" + typeFile + "\".");
			return false;
		}

		out.println(groupName);

		File testGroupProgramDir = new File(programBaseDir, groupName);

		boolean allPass = true;

		List<Pair<String,Processor.Factory>> runners = new ArrayList<Pair<String,Processor.Factory>>();

		// Generate bindings for this particular .tcks file.
		for (Map.Entry<String, Generator> e : impls.entrySet()) {
			String implName = e.getKey();
			Generator g = e.getValue();
			File implProgramDir = new File(testGroupProgramDir, implName);
			Util.mkdirs(err, implProgramDir);
			Processor.Factory processorFactory = g.generate(err, cksToolCommand, typeFile, groupName, implProgramDir);
			if (processorFactory != null) {
				runners.add(Pair.mk(implName, processorFactory));
			} else {
				allPass = false;
			}
		}

		if (runners.isEmpty()) return allPass;

		// Create the output directory.
		File testGroupOutDir = new File(testOutDir, groupName);
		Util.mkdirs(err, testGroupOutDir);

		// Run all the .cks files through the generated bindings.
		File[] dataFiles = testGroupDir.listFiles();
		sortByName(dataFiles);

		List<Pair<String,Processor>> processors = new ArrayList<Pair<String,Processor>>();
		for (Pair<String,Processor.Factory> e : runners) {
			String implName = e.left;
			Processor.Factory processorFactory = e.right;
			Processor processor;
			try {
				processor = processorFactory.create();
			}
			catch (IOException ex) {
				err.println("Unable to launch processor for \"" + implName + "\": " + ex.getMessage());
				continue;
			}
			processors.add(Pair.mk(implName, processor));
		}

		for (File dataFile : dataFiles) {
			String inputName = dataFile.getName();
			if (!dataFile.getName().endsWith(".cks")) continue;

			String inputBaseName = inputName.substring(0, inputName.length() - ".cks".length());

			if (testFilter != null) {
				boolean wasPresent = testFilter.remove(inputBaseName); // remove to remember the fact that we ran this test.
				if (!wasPresent) continue;
			}

			out.println("  " + inputBaseName);

            ArrayList<String[]> errorOutputs = new ArrayList<String[]>();

			for (Pair<String,Processor> e : processors) {
				String implName = e.left;
				Processor processor = e.right;

				out.print("    " + implName + ":"); out.flush();

				File textOutFile = new File(testGroupOutDir, inputBaseName + "." + implName + ".txt");
				File indentOutFile = new File(testGroupOutDir, inputBaseName + "." + implName + ".txti");
				File binaryOutFile = new File(testGroupOutDir, inputBaseName + "." + implName + ".bin");

				out.print(" text"); out.flush();
				boolean textOk = convertAndCheck(errorOutputs, out, processor, "text", dataFile, "text", textOutFile, "text");
				out.print(" texti"); out.flush();
				boolean textiOk = convertAndCheck(errorOutputs, out, processor, "text", dataFile, "texti", indentOutFile, "text");
				out.print(" binary"); out.flush();
				boolean binaryOk = convertAndCheck(errorOutputs, out, processor, "text", dataFile, "binary", binaryOutFile, "binary");

				allPass &= textOk;
				allPass &= textiOk;
				allPass &= binaryOk;

				File indentRoundtripFile = new File(testGroupOutDir, inputBaseName + "." + implName + ".txti.txt");
				File binaryRoundtripFile = new File(testGroupOutDir, inputBaseName + "." + implName + ".bin.txt");

				if (textOk) {
					if (textiOk) {
						out.print(" rt:texti"); out.flush();
						boolean rtTextOk = convertAndCheckAndSize(errorOutputs, out, processor, "text", indentOutFile, "text", indentRoundtripFile, "text", textOutFile);
						allPass &= rtTextOk;
					}
					if (binaryOk) {
						out.print(" rt:binary"); out.flush();
						boolean rtBinaryOk = convertAndCheckAndSize(errorOutputs, out, processor, "binary", binaryOutFile, "text", binaryRoundtripFile, "text", textOutFile);
						allPass &= rtBinaryOk;
					}
				}

				out.println();

                for (String[] lines : errorOutputs) {
                    for (String line : lines) {
                        out.println("E: " + line);
                    }
                }
			}

		}

		for (Pair<String,Processor> e : processors) {
			Processor processor = e.right;
			processor.kill();
		}

		return allPass;
	}

	private static boolean convertAndCheckAndSize(ArrayList<String[]> errorOutputs, PrintWriter out, Processor processor, String inputFormat, File inputFile, String outputFormat, File outputFile, String reloadFormat, File original)
	{
		boolean ok = convertAndCheck(errorOutputs, out, processor, inputFormat, inputFile, outputFormat, outputFile, reloadFormat);
		if (!ok) return false;

		// Make sure the output file and the original file are the same size.
		long outputFileSize = outputFile.length();
		long originalFileSize = original.length();

		if (outputFileSize != originalFileSize) {
			out.print(" [FAIL: size]");
			return false;
		}

		return true;
	}

	private static boolean convertAndCheck(ArrayList<String[]> errorOutputs, PrintWriter out, Processor processor, String inputFormat, File inputFile, String outputFormat, File outputFile, String reloadFormat)
	{
		Processor.Result convert = processor.convert(inputFormat + ":" + inputFile.getPath(), outputFormat + ":" + outputFile.getPath());
        if (!resultOk(errorOutputs, out, "convert", convert)) return false;

		Processor.Result compare = processor.compare(inputFormat + ":" + inputFile.getPath(), reloadFormat + ":" + outputFile.getPath());
        if (!resultOk(errorOutputs, out, "compare", compare)) return false;

		if (compare.out.length != 1) {
			out.print(" [FAIL: compare: incorrect number of lines: " + compare.out.length + "]");
			return false;
		}

		String r = compare.out[0];
		if (r.equals("equal")) {
			// Success!
			return true;
		}
		else if (r.equals("not equal")) {
			out.print(" [FAIL: inequal]");
			return false;
		}
		else {
			out.println(" [FAIL: compare: bad result string: " + StringUtil.jq(r) + "]");
			return false;
		}
	}

    private static boolean resultOk(ArrayList<String[]> errorOutputs, PrintWriter out, String action, Processor.Result result)
    {
        if (result.err.length > 0) {
            out.print(" [FAIL: " + action + ": non-empty stderr]");
            errorOutputs.add(result.err);
            return false;
        }
        if (result.done) {
            out.print(" [FAIL: convert: processor quit]");
            return false;
        }
        return true;
    }

	private static final String MarkerFileContents = "This file marks the fact that this directory was created by the CKS test runner and may be deleted/overwritten by the CKS test runner.";

	private static void prepareOutDir(PrintWriter err, File outDir)
	{
		File marker = new File(outDir, "Marker.txt");

		if (outDir.exists()) {
			if (!outDir.isDirectory()) {
				err.println("output-dir path \"" + outDir.getPath() + "\" already exists, but is not");
				err.println("a directory.");
				throw exit(2);
			}

			// Check if it contains a valid marker file.
			try {
				boolean matched = Util.expectFileContents(marker, MarkerFileContents);
				if (!matched) {
					err.println("output-dir \"" + outDir.getPath() + "\" already exists.  Because it");
					err.println("doesn't appear that this directory was created by a previous run of this");
					err.println("program, it will not be automatically overwritten.");
					throw exit(2);
				}
			} catch (IOException ex) {
				err.println("Error trying to read from output-dir marker file \"" + marker.getPath() + "\": " + ex.getMessage());
				throw exit(2);
			}

			// Delete the old contents.
			Maybe<File> mErr = Util.deleteContents(outDir);
			if (mErr.isJust()) {
				err.println("Unable to remove previous contents of output-dir \"" + outDir.getPath() + "\"");
				err.println("Error deleting: " + mErr.getJust().getPath());

				// Put the marker file back (hacky; a better solution: don't delete the marker file until the end)
				try {
					Util.writeFile(marker, MarkerFileContents);
				} catch (IOException ex) {
					err.println("Error trying to write to output-dir marker file \"" + marker.getPath() + "\": " + ex.getMessage());
				}

				throw exit(2);
			}

			// Write out a new marker file (since we deleted it in the previous step).
			try {
				Util.writeFile(marker, MarkerFileContents);
			} catch (IOException ex) {
				err.println("Error trying to write to output-dir marker file \"" + marker.getPath() + "\": " + ex.getMessage());
				throw exit(2);
			}
		}
		else {
			boolean created = outDir.mkdirs();
			if (!created) {
				err.println("Unable to create output-dir \"" + outDir.getPath() + "\"");
				throw exit(2);
			}

			try {
				Util.writeFile(marker, MarkerFileContents);
			} catch (IOException ex) {
				err.println("Error trying to write to output-dir marker file \"" + marker.getPath() + "\": " + ex.getMessage());
				throw exit(2);
			}
		}
	}

	private static void checkCksTool(List<String> cmd)
	{
        // Make sure the 'cks' command-line tool works.
	}
}
