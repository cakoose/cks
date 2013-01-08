package cks.test;

import java.io.*;

import cakoose.util.app.CommandLineLauncher;
import cks.io.StringUtil;
import cks.tool.Impls;

public class Harness
{

	public static void printUsage(PrintWriter out, String commandPrefix)
	{
		out.println("Usage:");
		out.println("  " + commandPrefix + "persistent");
		out.println("  " + commandPrefix + "convert <input-spec> <output-spec>");
		out.println("  " + commandPrefix + "compare <input-spec-1> <input-spec-1>");
	}

	public static <T> void run(PrintWriter out, InputStream in, PrintWriter err,
                               String[] args, String commandPrefix, Impls<T> impls)
	{
        if (args.length == 0) {
			printUsage(err, commandPrefix);
            throw new CommandLineLauncher.Exit(1);
        }

        String mode = args[0];
        if (mode.equals("persistent")) {
            if (args.length != 1) {
                err.println("Error: \"persistent\" mode doesn't accept any additional command-line arguments");
                throw new CommandLineLauncher.Exit(1);
            }
            runPersistent(err, out, in, impls);
        }
        else if (mode.equals("convert")) {
            if (args.length != 3) {
                err.println("Error: \"compare\" mode requires exactly two extra arguments");
            }
            String inputString = args[1];
            String outputString = args[2];

            impls.convert(err, inputString, outputString);
        }
        else if (mode.equals("compare")) {
            if (args.length != 3) {
                err.println("Error: \"persistent\" mode requires exactly two extra arguments");
            }
            String inputString1 = args[1];
            String inputString2 = args[2];

            impls.compare(err, out, inputString1, inputString2);
        }
    }

	public static <T> void runPersistent(PrintWriter origErr, PrintWriter origOut, InputStream origIn, Impls<T> impls)
    {
        PrintWriter out = new PrintWriter(new PrefixWriter("> ", origOut));
        PrintWriter err = new PrintWriter(new PrefixWriter("! ", origErr));

		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(origIn, StringUtil.UTF8));

			while (true) {
				String line = in.readLine();
				if (line == null) break;

				try {
					if (line.equals("convert")) {
						String inputString = in.readLine();
						String outputString = in.readLine();

						impls.convert(err, inputString, outputString);
					}
					else if (line.equals("compare")) {
						String inputString1 = in.readLine();
						String inputString2 = in.readLine();

						impls.compare(err, out, inputString1, inputString2);
					}
					else {
						err.println("Invalid command: " + line);
                        throw new CommandLineLauncher.Exit(1);
					}
				}
                catch (CommandLineLauncher.Exit ex) {
                    // Ignore and continue.
                }
                catch (Throwable ex) {
                    ex.printStackTrace(err);
                    throw new CommandLineLauncher.Exit(1);
                }
				finally {
                    out.flush();
                    err.flush();
                    origOut.println(">.");
                    origErr.println("!.");
                    origOut.flush();
                    origErr.flush();
				}
			}
		}
		catch (Throwable ex) {
			ex.printStackTrace(err);
            throw new CommandLineLauncher.Exit(1);
		}
	}

    private static final class PrefixWriter extends OutputStream
    {
        private final String prefix;
        private final Writer out;
        private boolean todo = true;

        private PrefixWriter(String prefix, Writer out)
        {
            this.prefix = prefix;
            this.out = out;
        }

        public void write(int b) throws IOException
        {
            if (todo) {
                out.write(prefix);
                todo = false;
            }
            out.write(b);
            if (b == '\n') {
                todo = true;
            }
        }

        public void flush() throws IOException
        {
            out.flush();
        }

        public void close() throws IOException
        {
            out.close();
        }
    }
}
