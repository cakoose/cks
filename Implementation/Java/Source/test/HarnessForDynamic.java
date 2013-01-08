package cks.test;

import java.io.InputStream;
import java.io.PrintWriter;

import cakoose.util.app.CommandLineLauncher;
import cks.tool.Impls;
import cks.tool.Main;

public class HarnessForDynamic extends CommandLineLauncher.TextOutput
{
	public static void main(String[] args)
	{
		CommandLineLauncher.run(new HarnessForDynamic(), args);
	}

	public void run(PrintWriter out, InputStream in, PrintWriter err, String[] args)
	{
		String commandPrefix = "COMMAND <type-file> <type-name> ";

		if (args.length < 2) {
			Harness.printUsage(err, commandPrefix);
			System.exit(1);
		}

		String typeFileName = args[0];
		String typeName = args[1];

		Impls<?> impls = Main.getImpls(err, typeFileName, typeName);

		String[] remainingArgs = new String[args.length-2];
		System.arraycopy(args, 2, remainingArgs, 0, remainingArgs.length);

		Harness.run(out, in, err, remainingArgs, commandPrefix, impls);
	}
}
