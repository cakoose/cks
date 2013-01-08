package cks.test;

import cakoose.util.text.IndentPrinter;

import java.io.File;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

public class Java
{
	static final String[] LibraryModules = {
		"core", "binary", "text-api", "text-reader", "text-writer",
		"dynamic", "surface", "type", "tool",
		"test",
	};

	public static final Impl Impl = new GeneratedImpl();
	public static final Impl NullImpl = new GeneratedImpl("-null-for-maybe");
	public static final Impl DynamicImpl = new DynamicImpl();

	public static final class GeneratedImpl extends Impl
	{
		public final String[] options;
		public GeneratedImpl(String... options) {
			this.options = options;
		}

		public Generator makeGenerator(PrintWriter err, File implsBaseDir)
		{
			final String classPath = check(err, implsBaseDir);
			if (classPath == null) return null;

			return new Generator() {
				public Processor.Factory generate(PrintWriter err, List<String> cksToolCommand, File typeFile, String typeName, File baseDir)
				{
					File sourceDir = new File(baseDir, "Source");
					File outputDir = new File(baseDir, "Output");
					File mainJava = new File(sourceDir, "Main.java");
					File testPackage = new File(sourceDir, "test");

					Util.mkdirs(err, sourceDir);
					Util.mkdirs(err, outputDir);
					Util.mkdirs(err, testPackage);

					// Generate bindings code.
					{
						List<String> command = new ArrayList<String>();
						command.addAll(cksToolCommand);
						command.add("gen-java");
						Collections.addAll(command, options);
						command.add(typeFile.getPath());
						command.add("test");
						command.add(testPackage.getPath());

						boolean ok = Util.exec(err, new Command(command), "generate Java bindings");
						if (!ok) return null;
					}

					// Write out "main" code.
					FileWriter fout;
					try {
						fout = new FileWriter(mainJava);
						try {
							IndentPrinter out = new IndentPrinter(fout);
							out.println("public class Main");
							out.println("{"); out.indent();

							// Main function
							out.println("public static void main(String[] args)");
							out.println("{"); out.indent();
							out.println("cks.test.HarnessForGenerated.run(args, test." + typeName + "._BinaryReader, test." + typeName + "._BinaryWriter, test." + typeName + "._TextReader, test." + typeName + "._TextWriter);");
							out.dedent(); out.println("}");

							out.dedent(); out.println("}");
						}
						finally {
							fout.close();
						}
					}
					catch (IOException ex) {
						err.println("Error trying to write to file \"" + mainJava.getPath() + "\": " + ex.getMessage());
						return null;
					}

					// Run 'javac'
					{
						List<String> command = new ArrayList<String>();
						command.add("javac");
						command.add("-Xlint:unchecked");
						command.add("-classpath"); command.add(classPath);
						command.add("-d"); command.add(outputDir.getPath());
						command.add("-sourcepath"); command.add(sourceDir.getPath());
						command.add(sourceDir.getPath() + File.separatorChar + "Main.java");

						boolean ok = Util.exec(err, new Command(command), "compile Java bindings");
						if (!ok) return null;
					}

					return new Processor.Factory.Persistent(new Command(
						"java", "-ea",
							"-classpath", outputDir.getPath() + File.pathSeparator + classPath,
							"Main", "persistent"));
				}
			};
		}
	}

	public static final class DynamicImpl extends Impl
	{
		public final String[] options;
		public DynamicImpl(String... options) {
			this.options = options;
		}

		public Generator makeGenerator(PrintWriter err, File implsBaseDir)
		{
			final String classPath = check(err, implsBaseDir);
			if (classPath == null) return null;

			return new Generator() {
				public Processor.Factory generate(PrintWriter err, List<String> cksToolCommand, File typeFile, String typeName, File baseDir)
				{
					return new Processor.Factory.Persistent(new Command(
						"java", "-ea", "-classpath", classPath, "cks.test.HarnessForDynamic", typeFile.getPath(), typeName, "persistent"));
				}
			};
		}
	}

	private static String check(PrintWriter err, File implsBaseDir)
	{
		// Check for correct dirs.
		File implDir = new File(implsBaseDir, "Java");
		File outputDir = new File(implDir, "Output");
		File bytecodeDir = new File(outputDir, "Bytecode");

		if (!implDir.exists() || !implDir.isDirectory()) {
            err.println("Couldn't find Java implementation at \"" + implDir.getPath() + "\"");
			return null;
        }

		if (!bytecodeDir.exists() || !bytecodeDir.isDirectory()) {
            err.println("Couldn't find Java compiled output in \"" + bytecodeDir.getPath() + "\"");
			return null;
        }

		StringBuilder cp = new StringBuilder();
		String sep = "";

		for (String m : LibraryModules) {
            cp.append(sep); sep = File.pathSeparator;
            File mFile = new File(bytecodeDir, m);
            if (!mFile.exists() || !mFile.isDirectory()) {
                err.println("Couldn't find bytecode for Java runtime support module in \"" + mFile.getPath() + "\"");
				return null;
            }
            cp.append(mFile.getPath());
        }

		File libraryDir = new File(implDir, "Libraries");
		File cakooseUtilJar = new File(libraryDir, "cakoose-util.jar");

		if (!cakooseUtilJar.exists() || !cakooseUtilJar.isFile()) {
            err.println("Could't find 'cakoose-util' Jar file at \"" + cakooseUtilJar.getPath() + "\".");
			return null;
        }
		cp.append(File.pathSeparator).append(cakooseUtilJar.getPath());
		return cp.toString();
	}

}
