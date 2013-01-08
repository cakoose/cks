package cks.test;

import cakoose.util.text.IndentPrinter;

import java.io.File;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

public class CSharp
{
	public static final Impl Impl = new Impl()
	{
		public cks.test.Generator makeGenerator(PrintWriter err, File implsBaseDir)
		{
			// Check for correct dirs.
			File implDir = new File(implsBaseDir, "C#");
			File outputDir = new File(implDir, "Output");
			File[] dlls = {
				new File(outputDir, "Cks.Core.dll"),
				new File(outputDir, "Cks.Text.Reader.dll"),
				new File(outputDir, "Cks.Text.Writer.dll"),
			};
			File harnessFile = new File(implsBaseDir, "Test/Harness/C#/Harness.cs");

			if (!implDir.exists() || !implDir.isDirectory()) {
				err.println("Couldn't find C# implementation at \"" + implDir.getPath() + "\"");
				return null;
			}

			for (File dll : dlls) {
				if (!dll.exists() || !dll.isFile()) {
					err.println("Couldn't find C# runtime DLL at \"" + dll.getPath() + "\"");
					return null;
				}
			}

			if (!harnessFile.exists() || !harnessFile.isFile()) {
				err.println("Coudln't find C# test harness at \"" + harnessFile.getPath() + "\"");
				return null;
			}

			return new Generator(dlls, harnessFile);
		}
	};

	public static final class Generator extends cks.test.Generator
	{
		private final File[] dlls;
		private final File harnessFile;

		public Generator(File[] dlls, File harnessFile)
		{
			this.dlls = dlls;
			this.harnessFile = harnessFile;
		}

		public Processor.Factory generate(PrintWriter err, List<String> cksToolCommand, File typeFile, String typeName, File baseDir)
		{
			File sourceDir = new File(baseDir, "Source");
			File outputDir = new File(baseDir, "Output");
			File mainSource = new File(sourceDir, "Entrypoint.cs");
			File bindingsSource = new File(sourceDir, "Test.cs");

			File outputExe = new File(outputDir, "Test.exe");

			Util.mkdirs(err, sourceDir);
			Util.mkdirs(err, outputDir);

			// Generate bindings code.
			{
				List<String> command = new ArrayList<String>();
				command.addAll(cksToolCommand);
				command.add("gen-c#");
				command.add(typeFile.getPath());
				command.add("Test");
				command.add(bindingsSource.getPath());

				boolean ok = Util.exec(err, new Command(command), "generate C# bindings");
				if (!ok) return null;
			}

			// TODO: Copy test harness code.

			// Write out "main" code.
			FileWriter fout;
			try {
				fout = new FileWriter(mainSource);
				try {
					IndentPrinter out = new IndentPrinter(fout);
					out.println();
					out.println("public class Entrypoint");
					out.println("{"); out.indent();

					// Main function
					out.println("public static void Main(string[] Args)");
					out.println("{"); out.indent();
						out.println("Harness.Run(Args, Test." + typeName + "._BinaryReader, Test." + typeName + "._BinaryWriter, Test." + typeName + "._TextReader, Test." + typeName + "._TextWriter);");
						out.dedent();
					out.println("}");

					out.dedent(); out.println("}"); // class
				}
				finally {
					fout.close();
				}
			}
			catch (IOException ex) {
				err.println("Error trying to write to file \"" + mainSource.getPath() + "\": " + ex.getMessage());
				return null;
			}

			// Run CSC
			{
				List<String> command = new ArrayList<String>();
				command.add("gmcs");
				command.add("-debug+");
				for (File dll : dlls) {
					command.add("-reference:" + dll.getPath());
				}
				command.add("-out:" + outputExe.getPath());
				command.add("-main:Entrypoint");
				command.add(mainSource.getPath());
				command.add(bindingsSource.getPath());
				command.add(harnessFile.getPath());

				boolean ok = Util.exec(err, new Command(command), "compile C# bindings");
				if (!ok) return null;
			}

			Command command = new Command("mono", "--debug", outputExe.getPath());
			command.env.put("MONO_PATH", "../C#/Output");
			return new Processor.Factory.Persistent(command);
		}
	}
}
