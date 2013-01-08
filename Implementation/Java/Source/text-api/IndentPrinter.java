package cks.io;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 * <p>
 * Printer that manages indentation.  Helps when pretty-printing a
 * hierarchy.  For example:
 * </p>
 *
 * <pre>
 * IndentPrinter p = new IndentPrinter(out, "  ");
 * p.println("Root 1");
 * p.indent();
 * p.println("Child 1");
 * p.println("Child 2");
 * p.dedent();
 * p.println("Root 2");
 * </pre>
 *
 * <p>
 * Yields:
 * </p>
 *
 * <pre>
 * Root 1
 *   Child 1
 *   Child 2
 * Root 2
 * </pre>
 *
 * <p>
 * This class never buffers any data, so you don't need to use {@link #flush()}
 * to force anything out.  Calling {@link #flush()} will just flush on the
 * underlying Writer.
 * </p>
 */
public final class IndentPrinter extends Writer implements Appendable
{
	private final Writer out;
	private final String prefix;
	private final String indent;
	private int numIndents;

	private boolean nl;     ///< If we're currently at the beginning of a new line

	/** Two spaces. */
	public static final String DefaultIndent = "  ";

	/**
	 * @param indent
	 *    The string to add to the prefix for each additional indent
	 *    level.
	 *
	 * @param prefix
	 *    The indentation to prefix to the beginning of each line.
	 *
	 * @param freshLine
	 *    If we're starting on a "fresh" line, we need to add indentation
	 *    to the very first thing printed.  If false, then we'll only start
	 *    adding indentation after the first 'println'.
	 */
	public IndentPrinter(Writer out, String indent, String prefix, boolean freshLine)
	{
		this.out = out;
		this.indent = indent;
		this.prefix = prefix;
		this.numIndents = 0;
		this.nl = freshLine;
	}

   /**
	* Equivalent to: <tt>IndentPrinter(out, indent, prefix, true)</tt>.
	*/
	public IndentPrinter(Writer out, String indent, String prefix)
	{
		this(out, indent, prefix, true);
	}

   /**
	* Equivalent to: <tt>IndentPrinter(out, indent, "")</tt>.
	*/
	public IndentPrinter(Writer out, String indent)
	{
		this(out, indent, "");
	}

   /**
	* Equivalent to: <tt>IndentPrinter(out, IndentPrinter.DefaultIndent)</tt>.
	*/
	public IndentPrinter(Writer out)
	{
		this(out, DefaultIndent);
	}

	public IndentPrinter(OutputStream out, String indent, String prefix, boolean freshLine)
	{
		this(StringUtil.bufferedUtf8Writer(out), indent, prefix, freshLine);
	}

	public IndentPrinter(OutputStream out, String indent, String prefix)
	{
		this(out, indent, prefix, true);
	}

	public IndentPrinter(OutputStream out, String indent)
	{
		this(out, indent, "");
	}

	public IndentPrinter(OutputStream out)
	{
		this(out, DefaultIndent);
	}

	// -----------------------------------------
	// indentation control

	/**
	 * Increase the indentation level by 'numIndents' indentations.
	 */
	public void indent(int numIndents)
	{
		this.numIndents += numIndents;
	}

	/**
	 * Increase the indentation level by one.
	 */
	public void indent()
	{
		this.numIndents += 1;
	}

	/**
	 * Decrease the indentation level by 'numIndents' indentations.
	 */
	public void dedent(int numIndents)
	{
		if (numIndents > this.numIndents) throw new IllegalArgumentException("attempt to decrease indent by " + numIndents + ", but indent level is currently " + this.numIndents);
		this.numIndents -= numIndents;
	}

	/**
	 * Decrease the indentation level by one.
	 */
	public void dedent()
	{
		if (this.numIndents < 1) throw new IllegalArgumentException("attempt to decrease indent, but indent level is currently " + this.numIndents);
		this.numIndents--;
	}

	// -----------------------------------------
	// print functions

	public void print(String v)  throws IOException { pre(); out.write(v == null ? "null" : v); }
	public void print(char[] v)  throws IOException { pre(); out.write(v); }
	public void print(Object v)  throws IOException { pre(); out.write(String.valueOf(v)); }
	public void print(char v)    throws IOException { pre(); out.write(v); }
	public void print(int v)     throws IOException { pre(); out.write(String.valueOf(v)); }
	public void print(long v)    throws IOException { pre(); out.write(String.valueOf(v)); }
	public void print(double v)  throws IOException { pre(); out.write(String.valueOf(v)); }
	public void print(float v)   throws IOException { pre(); out.write(String.valueOf(v)); }
	public void print(boolean v) throws IOException { pre(); out.write(String.valueOf(v)); }
	public void print(CharSequence v) throws IOException { pre(); out.write(v == null ? "null" : v.toString()); }

	public IndentPrinter append(CharSequence v) throws IOException
	{
		print(v);
		return this;
	}

	public IndentPrinter append(CharSequence v, int start, int end) throws IOException
	{
		print(v.subSequence(start, end));
		return this;
	}

	public IndentPrinter append(char c)
		throws IOException
	{
		print(c);
		return this;
	}

	// -----------------------------------------
	// print-ln functions

	public void println(String v)  throws IOException { this.print(v); nl(); }
	public void println(char[] v)  throws IOException { this.print(v); nl(); }
	public void println(Object v)  throws IOException { this.print(v); nl(); }
	public void println(char v)    throws IOException { this.print(v); nl(); }
	public void println(int v)     throws IOException { this.print(v); nl(); }
	public void println(long v)    throws IOException { this.print(v); nl(); }
	public void println(double v)  throws IOException { this.print(v); nl(); }
	public void println(float v)   throws IOException { this.print(v); nl(); }
	public void println(boolean v) throws IOException { this.print(v); nl(); }
	public void println(CharSequence v) throws IOException { out.write(v == null ? "null" : v.toString()); nl(); }

	public void println() throws IOException { nl(); }

	// -----------------------------------------
	// raw write functions
	
	public void write(char[] buf)                   throws IOException { pre(); out.write(buf); }
	public void write(char[] buf, int off, int len) throws IOException { pre(); out.write(buf, off, len); }
	public void write(char c)                       throws IOException { pre(); out.write(c); }
	public void write(String s)                     throws IOException { pre(); out.write(s); }
	public void write(String s, int off, int len)   throws IOException { pre(); out.write(s, off, len); }

	// -----------------------------------------
	// indentation functions

	private void pre() throws IOException
	{
		if (nl) {
			out.write(prefix);
			for (int i = 0; i < this.numIndents; i++) {
				out.write(indent);
			}

			nl = false;
		}
	}

	public static final String NL = System.getProperty("line.separator");

	private void nl() throws IOException
	{
		out.write(NL);
		nl = true;
	}

	// -----------------------------------------
	// java.io.Writer functions

	public void flush() throws IOException { out.flush(); }
	public void close() throws IOException { out.close(); }
}
