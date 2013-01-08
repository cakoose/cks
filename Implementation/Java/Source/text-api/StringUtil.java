package cks.io;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;

public class StringUtil
{
	private StringUtil() {} // Just a namespace.  Can't instantiate.

	public static final Charset UTF8 = Charset.forName("UTF-8");

	public static Writer bufferedUtf8Writer(OutputStream out)
	{
		return new BufferedWriter(new OutputStreamWriter(out, UTF8));
	}

	public static Reader utf8Reader(InputStream in)
	{
		return new InputStreamReader(in, UTF8);
	}

	/**
	 * NOTE: This will read ahead in the InputStream.  Only use this if you
	 * own the entire stream contents.
	 */
	public static Reader bufferedUtf8Reader(InputStream in)
	{
		return new BufferedReader(new InputStreamReader(in, UTF8));
	}

	// ----------------------------------------------------------------------
	/**
	 * <p>
	 * Human-readable representation of a character.  Most "normal" characters
	 * are simply placed between quotes.
	 * </p>
	 *
	 * <ul>
	 * <li>normal chars &rarr; "a", "b", "[", "^", etc.</li>
	 * <li>double quote &rarr; ["]</li>
	 * <li>single quote &rarr; [']</li>
	 * <li>newline      &rarr; #0a &lt;LineFeed&gt;</li>
	 * <li>space        &rarr; #20 &lt;Space&gt;</li>
	 * <li>0x124        &rarr; #124</li>
	 * </ul>
	 */
	public static String readableChar(char c)
	{
		StringBuilder buf = new StringBuilder();
		readableChar(buf, c);
		return buf.toString();
	}

	/**
	 * Equivalent to {@link #readableChar(char)}, except it appends the
	 * result to a StringBuilder instead of returning it as a string.
	 */
	public static void readableChar(StringBuilder buf, char c)
	{
		if (c == '"' || c == '\'') {
			// For quotes, surround with brackets.
			buf.append('[').append(c).append(']');
		}
		else if (c >= 32 && c <= 126) {
			// For all other printable characters, surround with double quotes.
			buf.append('"').append(c).append('"');
		}
		else {
			buf.append('#');
			hex(buf, c);

			// Add an aditional 
			int pos = FriendlyChars.indexOf(c);
			if (pos >= 0) {
				// Lookup special character name.
				buf.append(' ');
				buf.append('<');
				buf.append(FriendlyCharNames[pos]);
				buf.append('>');
			}
		}
	}

	private static final String FriendlyChars = ""
		+ (char)0x08 // Backspace
		+ (char)0x09 // Tab
		+ (char)0x0a // NewLine
		+ (char)0x0b // VerticalTab
		+ (char)0x0c // FormFeed
		+ (char)0x0d // CarriageReturn
		+ (char)0x1b // Escape
		+ (char)0x7f // Delete
		;
	private static final String[] FriendlyCharNames = {
		"Backspace", "Tab", "NewLine", "VerticalTab", "FormFeed",
		"CarriageReturn", "Escape", "Delete",
	};
	static { assert FriendlyChars.length() == FriendlyCharNames.length; }

	// ----------------------------------------------------------------------

	public static final String Hex = "0123456789abcdef";

	/**
	 * Append the 4-digit hex value of the given character.
	 */
	public static void hex4(StringBuilder buf, char c)
	{
		int i = c;
		assert (i >= 0) && i < (1 << 16) : i;
		char h1 = Hex.charAt((c >>> 12));
		char h2 = Hex.charAt((c >>>  8) & 0xf);
		char h3 = Hex.charAt((c >>>  4) & 0xf);
		char h4 = Hex.charAt((c >>>  0) & 0xf);
		buf.append(h1).append(h2).append(h3).append(h4);
	}

	/**
	 * Append the hex value of the given character.  Will use
	 * between 1 and 4 digits (leading zeros are ommitted).
	 */
	public static void hex(StringBuilder buf, char c)
	{
		int i = c;
		assert (i >= 0) && i < (1 << 16) : i;
		int h1 = (c >>> 12);
		int h2 = (c >>>  8) & 0xf;
		int h3 = (c >>>  4) & 0xf;
		int h4 = (c >>>  0) & 0xf;

		// Avoid appending leading zeros.
		l3: {
			l2: {
				l1: {

					if (h1 == 0) {
						if (h2 == 0) {
							if (h3 == 0) {
								break l3;
							}
							break l2;
						}
						break l1;
					}

					buf.append(Hex.charAt(h1));
				}
				buf.append(Hex.charAt(h2));
			}
			buf.append(Hex.charAt(h3));
		}
		buf.append(Hex.charAt(h4));
	}
}
