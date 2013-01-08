package cks.test;

import java.io.PrintWriter;
import java.io.File;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.FileWriter;
import java.util.List;
import java.nio.charset.CharacterCodingException;

import cakoose.util.app.CommandLineLauncher.Exit;
import cakoose.util.exec.Exec;
import cakoose.util.text.StringUtil;
import cakoose.util.Maybe;

public class Util
{
	private Util() {}

	public static void mkdirs(PrintWriter err, File path)
		throws Exit
	{
		boolean ok = path.mkdirs();
		if (!ok) {
			err.println("Unable to create directory \"" + path.getPath() + "\"");
			throw new Exit(2);
		}
	}

	public static boolean exec(PrintWriter err, Command command, String name)
	{
		Exec.StringOutputCollector stdout = new Exec.StringOutputCollector();
		return exec(err, command, name, Exec.EmptyInput, stdout);
	}

	public static boolean exec(PrintWriter err, Command command, String name, Exec.InputGenerator stdin, Exec.OutputConsumer stdout)
	{
		Exec.StringOutputCollector stderr = new Exec.StringOutputCollector();

		try {
			Exec exec = new Exec(command.command);
			exec.environment().putAll(command.env);
			Exec.Handle h = exec.launch(stdin, stdout, stderr);
			int ret = h.waitFor(ExecTimeout);

			if (ret < 0) {
				err.println("Error running: " + name);
				err.println("Execution timed out.");
				err.println("Command: " + commandToString(command.command));
				return false;
			}

			boolean ok = (ret == 0);

			String stderrString;
			try {
				stderrString = stderr.getString();
			}
			catch (CharacterCodingException ex) {
				err.println("Error running: " + name);
				err.println("Command: " + commandToString(command.command));
				err.println("Unable to convert the process' stderr output to text: " + ex.getMessage());
				err.println("Command: " + commandToString(command.command));
				return false;
			}

			if (!ok || stderrString.length() > 0) {
				err.println("Error running: " + name);
				if (!ok) {
					err.println("Non-zero exit code (" + ret + ")");
				}
				else {
					err.println("Non-empty standard error");
				}
				err.println("Command: " + commandToString(command.command));
				if (stderrString.length() > 0) {
					err.println("Error Output:");
					err.println(stderrString);
				}
			}

			return ok;
		}
		catch (InterruptedException ex) {
			ex.printStackTrace(err); // ???
			return false;
		}
		catch (IOException ex) {
			err.println("Error running: " + name);
			err.println(ex.getMessage());
			err.println("Command: " + commandToString(command.command));
			return false;
		}
	}

	public static final int ExecTimeout = 20 * 1000; // 20 second timeout

	public static String commandToString(Iterable<String> args)
	{
		StringBuilder buf = new StringBuilder();
		String sep = "";
		for (String arg : args) {
			buf.append(sep); sep = " ";
			if (argNeedsQuotes(arg)) {
				buf.append(StringUtil.jq(arg));
			} else {
				buf.append(arg);
			}
		}
		return buf.toString();
	}

	public static boolean argNeedsQuotes(String arg)
	{
		for (int i = 0; i < arg.length(); i++) {
			char c = arg.charAt(i);
			if (Character.isSpaceChar(c)) return true;
			if (Character.isISOControl(c)) return true;
			if (c == '"') return true;
		}
		return false;
	}

	public static boolean exec(PrintWriter err, Command command, String name, File input, File output)
	{
		FileInputStream fin;
		Exec.InputGenerator stdin;
		Exec.OutputConsumer stdout;

		try {
			fin = new FileInputStream(input);
		}
		catch (IOException ex) {
			err.println("Unable to open file \"" + input.getPath() + "\" for reading.");
			return false;
		}

		try {
			stdin = new Exec.InputStreamRelay(fin);

			FileOutputStream fout;
			try {
				fout = new FileOutputStream(output);
			}
			catch (IOException ex) {
				err.println("Unable to open file \"" + output.getPath() + "\" for writing.");
				return false;
			}

			try {
				stdout = new Exec.OutputStreamRelay(fout);
				return exec(err, command, name, stdin, stdout);
			}
			finally {
				try { fout.close(); }
				catch (IOException ex) {
					err.println("Error writing to output file \"" + output.getPath() + "\".");
					return false;
				}
			}
		}
		finally {
			try { fin.close(); } catch (IOException ex) {}
		}
	}

	public static boolean fileEq(PrintWriter err, File f1, File f2)
	{
		FileInputStream fin1;
		try {
			fin1 = new FileInputStream(f1);
		}
		catch (IOException ex) {
			err.println("Unable to read from file \"" + f1.getPath() + "\": " + ex.getMessage());
			return false;
		}

		try {
			FileInputStream fin2;
			try {
				fin2 = new FileInputStream(f2);
			}
			catch (IOException ex) {
				err.println("Unable to read from file \"" + f2.getPath() + "\": " + ex.getMessage());
				return false;
			}

			try {
				byte[] buf1 = new byte[1024];
				byte[] buf2 = new byte[1024];
				while (true) {
					int n1, n2;

					try {
						n1 = readFully(fin1, buf1, 0, buf1.length);
					}
					catch (IOException ex) {
						err.println("Unable to read from file \"" + f1.getPath() + "\": " + ex.getMessage());
						return false;
					}

					try {
						n2 = readFully(fin2, buf2, 0, buf2.length);
					}
					catch (IOException ex) {
						err.println("Unable to read from file \"" + f2.getPath() + "\": " + ex.getMessage());
						return false;
					}

					if (n1 != n2) return false;
					if (n1 == -1) return true;

					for (int i = 0; i < n1; i++) {
						if (buf1[i] != buf2[i]) return false;
					}
				}
			}
			finally {
				try { fin2.close(); } catch (IOException ex) {}
			}
		}
		finally {
			try { fin1.close(); } catch (IOException ex) {}
		}
	}

	static int readFully(InputStream in, byte[] buffer, int start, int length)
		throws IOException
	{
		int pos = start;
		int remaining = length;
		while (remaining > 0) {
			int read = in.read(buffer, pos, remaining);
			assert read != 0;
			if (read == -1) {
				if (remaining == length) {
					return -1; // EOF on first try.
				} else {
					return length - remaining;
				}
			} else {
				pos += read;
				remaining -= read;
			}
		}
		return length;
	}

	public static boolean expectFileContents(File file, String expected)
		throws IOException
	{
		FileReader fin = new FileReader(file);
		try {
			BufferedReader in = new BufferedReader(fin);
			String line = in.readLine();
			return line.equals(expected);
		}
		finally {
			try { fin.close(); } catch (IOException ex) {}
		}
	}

	// If success, returns Maybe.Nothing.
	// If failure, returns Maybe.Just(the file that couldn't be deleted).
	public static Maybe<File> deleteContents(File outDir)
	{
		File[] subs = outDir.listFiles();
		for (File sub : subs) {
			if (sub.isDirectory()) {
				Maybe<File> mErr = deleteContents(sub);
				if (mErr.isJust()) return mErr;
			}
			boolean ok = sub.delete();
			if (!ok) return Maybe.Just(sub);
		}
		return Maybe.Nothing();
	}

	public static void writeFile(File file, String line)
		throws IOException
	{
		FileWriter fout = new FileWriter(file);
		try {
			fout.write(line);
		}
		finally {
			fout.close();
		}
	}
}
