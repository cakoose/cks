package cks.io;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.math.BigInteger;

public class CksTextFormatter extends Formatter
{
	protected final Writer out;

	public CksTextFormatter(OutputStream out)
	{
		this(StringUtil.bufferedUtf8Writer(out));
	}

	public CksTextFormatter(Writer out)
	{
		this.out = out;
	}

	private boolean firstEntry = false;

	@Override
	public void beginRecord() throws IOException
	{
		out.write("{");
		firstEntry = true;
	}

	@Override
	public void preField(String name) throws IOException
	{
		if (firstEntry) {
			firstEntry = false;
		} else {
			out.write(", ");
		}
		out.write(name);
		out.write(" = ");
	}

	@Override
	public void endRecord() throws IOException
	{
		firstEntry = false;
		out.write("}");
	}

	@Override
	public void voidVariant(String tag) throws IOException
	{
		out.write(tag);
	}

	@Override
	public void beginVariant(String tag) throws IOException
	{
		out.write(tag);
		out.write(": ");
	}

	@Override
	public void endVariant() throws IOException
	{
	}

	@Override
	public void beginList() throws IOException
	{
		out.write("[");
		firstEntry = true;
	}

	@Override
	public void preListElement() throws IOException
	{
		if (firstEntry) {
			firstEntry = false;
		} else {
			out.write(", ");
		}
	}

	@Override
	public void endList() throws IOException
	{
		firstEntry = false;
		out.write("]");
	}

	@Override
	public void beginSet() throws IOException
	{
		out.write("[");
		firstEntry = true;
	}

	@Override
	public void preSetElement() throws IOException
	{
		if (firstEntry) {
			firstEntry = false;
		} else {
			out.write(", ");
		}
	}

	@Override
	public void endSet() throws IOException
	{
		firstEntry = false;
		out.write("]");
	}

	@Override
	public void beginMap() throws IOException
	{
		out.write("[");
		firstEntry = true;
	}

	@Override
	public void preMapKey() throws IOException
	{
		if (firstEntry) {
			firstEntry = false;
		} else {
			out.write(", ");
		}
	}

	@Override
	public void preMapValue() throws IOException
	{
		out.write(" -> ");
	}

	@Override
	public void endMap() throws IOException
	{
		firstEntry = false;
		out.write("]");
	}

	@Override
	public void writeMaybeNone() throws IOException
	{
		out.write("None");
	}

	@Override
	public void beginMaybeSet() throws IOException
	{
		out.write("Set: ");
	}

	@Override
	public void endMaybeSet() throws IOException
	{
	}

	@Override
	public void writeInt32(int value) throws IOException
	{
		out.write(Integer.toString(value));
	}

	@Override
	public void writeInt64(long value) throws IOException
	{
		out.write(Long.toString(value));
	}

	@Override
	public void writeInt(BigInteger value) throws IOException
	{
		out.write(value.toString());
	}

	private static final String Hex = "0123456789abcdef";


	@Override
	public void writeString(String value) throws IOException
	{
		writeString(out, value);
	}

	public static void writeString(Writer out, String value) throws IOException
	{
		out.write('"');
		char[] array = null;
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			switch (c) {
				case '"': out.write("\\\""); break;
				case '\\': out.write("\\\\"); break;
				case '\n': out.write("\\n"); break;
				case '\t': out.write("\\t"); break;
				case '\r': out.write("\\r"); break;
				case '\0': out.write("\\0"); break;
				default:
					if (Character.isISOControl(c)) {
						// Encode as: "x" + two hex digits.
						if (array == null) {
							array = new char[4];
							array[0] = '\\';
						}
						array[1] = 'x';
						array[3] = Hex.charAt(c & 0xf);
						c >>= 4;
						array[2] = Hex.charAt(c & 0xf);
						out.write(array, 0, 4);
					}
					else if (Character.isHighSurrogate(c)) {
						// Surrogate pair
						i++;
						if (i >= value.length()) {
							throw new IllegalArgumentException("high surrogate not followed by anything");
						}
						char c2 = value.charAt(i);
						if (!Character.isLowSurrogate(c2)) {
							throw new IllegalArgumentException("high surrogate not followed by low surrogate");
						}
						out.write(value, i-1, 2);
					}
					else if (Character.isLowSurrogate(c)) {
						throw new IllegalArgumentException("low surrogate without preceding high surrogate");
					}
					else {
						// Basic Multilingual Plane (16 bits)
						out.write(c);
					}
			}
		}
		out.write('"');
	}

	@Override
	public void writeBool(boolean value) throws IOException
	{
		out.write(value ? "True" : "False");
	}

	@Override
	public void writeVoid() throws IOException
	{
		out.write("()");
	}

	public void flush() throws IOException { out.flush(); }
	public void close() throws IOException { out.close(); }
}
