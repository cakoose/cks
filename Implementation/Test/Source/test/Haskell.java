package cks.test;

import cakoose.util.text.IndentPrinter;

import java.io.File;
import java.io.PrintWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
import java.util.ArrayList;

public class Haskell
{
	public static final Impl Impl = new Impl()
	{
		public cks.test.Generator makeGenerator(PrintWriter err, File implsBaseDir)
		{
			File implDir = new File(implsBaseDir, "Haskell");
			File outputDir = new File(implDir, "Output");
			File harnessFile = new File(implsBaseDir, "Test/Harness/Haskell/Harness.hs");

			if (!implDir.exists() || !implDir.isDirectory()) {
				err.println("Couldn't find Haskell implementation at \"" + implDir.getPath() + "\"");
				return null;
			}

			if (!outputDir.exists() || !outputDir.isDirectory()) {
				err.println("Couldn't find Haskell compiled output in \"" + outputDir.getPath() + "\"");
				return null;
			}

			if (!harnessFile.exists() || !harnessFile.isFile()) {
				err.println("Coudln't find Haskell test harness at \"" + harnessFile.getPath() + "\"");
				return null;
			}

			return new Generator(implDir, harnessFile);
		}
	};

	public static final class Generator extends cks.test.Generator
	{
		private final File implDir;
		private final File harnessFile;

		public Generator(File implDir, File harnessFile)
		{
			this.implDir = implDir;
			this.harnessFile = harnessFile;
		}

		public Processor.Factory generate(PrintWriter err, List<String> cksToolCommand, File typeFile, String typeName, File baseDir)
		{
			File sourceDir = new File(baseDir, "Source");
			File outputDir = new File(baseDir, "Output");
			File objectDir = new File(outputDir, "Object");
			File interfaceDir = new File(outputDir, "Interface");
			File bindingsHs = new File(sourceDir, "Test.hs");
			File mainHs = new File(sourceDir, "Main.hs");

			Util.mkdirs(err, sourceDir);
			Util.mkdirs(err, outputDir);
			Util.mkdirs(err, objectDir);
			Util.mkdirs(err, interfaceDir);

			// Generate bindings code.
			{
				List<String> command = new ArrayList<String>();
				command.addAll(cksToolCommand);
				command.add("gen-haskell");
				command.add(typeFile.getPath());
				command.add("Test");
				command.add(bindingsHs.getPath());

				boolean ok = Util.exec(err, new Command(command), "generate Haskell bindings");
				if (!ok) return null;
			}

			// Write out "main" code.
			FileWriter fout;
			try {
				fout = new FileWriter(mainHs);
			}
			catch (IOException ex) {
				err.println("Unable to write to file \"" + mainHs.getPath() + "\": " + ex.getMessage());
				return null;
			}

			try {
				IndentPrinter out = new IndentPrinter(fout);
				out.println("module Main (main) where");
				out.println("import qualified Test as Test");
				out.println("import qualified Harness as Harness");
				out.println("main :: IO ()");
				out.println("main = Harness.run Test.readBinary_" + typeName + " Test.writeBinary_" + typeName + " Test.marshalText_" + typeName + " Test.writeText_" + typeName + " Test.writeTextIndented_" + typeName);
			}
			catch (IOException ex) {
				err.println("Unable to write to file \"" + mainHs.getPath() + "\": " + ex.getMessage());
				return null;
			}
			finally {
				try {
					fout.close();
				} catch (IOException ex) {
					err.println("Unable to write to file \"" + mainHs.getPath() + "\": " + ex.getMessage());
					return null;
				}
			}

			// TODO: Right now, we just copy all the library sources.  We need to change this to
			// do proper separate compilation.
			File libSources = new File(implDir, "Source");

			// Run GHC
			File executable = new File(outputDir, "test");
			{
				List<String> command = new ArrayList<String>();
				command.add("ghc");
				command.add("-v0"); // Don't dump build progress on stderr
				command.add("--make");
				command.add("-odir"); command.add(objectDir.getPath());
				command.add("-hidir"); command.add(interfaceDir.getPath());
				//command.add("-fhpc");
				command.add("-XFlexibleInstances");
				command.add("-XScopedTypeVariables");
				command.add("-o"); command.add(executable.getPath());
				command.add("-i" + libSources.getPath());
				command.add(mainHs.getPath());
				command.add(harnessFile.getPath());
				command.add(bindingsHs.getPath());

				boolean ok = Util.exec(err, new Command(command), "compile Haskell bindings");
				if (!ok) return null;
			}

			return new Processor.Factory.Persistent(new Command(executable.getPath()));
		}
	}
}
