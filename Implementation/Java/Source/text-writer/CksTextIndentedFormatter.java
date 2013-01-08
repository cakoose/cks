package cks.io;

import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.math.BigInteger;
import java.nio.charset.Charset;

public class CksTextIndentedFormatter extends Formatter
{
	private final IndentPrinter out;

	public CksTextIndentedFormatter(IndentPrinter out)
	{
		this.out = out;
	}

	private boolean firstEntry = false;

	@Override
	public void beginRecord() throws IOException
	{
		firstEntry = true;
		out.write("{");
		out.indent();
	}

	@Override
	public void preField(String name) throws IOException
	{
		firstEntry = false;
		out.println();
		out.write(name);
		out.write(" = ");
	}

	@Override
	public void endRecord() throws IOException
	{
		if (!firstEntry) out.println();
		firstEntry = false;
		out.dedent();
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
		firstEntry = true;
		out.write("[");
		out.indent();
	}

	@Override
	public void preListElement() throws IOException
	{
		firstEntry = false;
		out.println();
	}

	@Override
	public void endList() throws IOException
	{
		if (!firstEntry) out.println();
		firstEntry = false;
		out.dedent();
		out.write("]");
	}

	@Override
	public void beginSet() throws IOException
	{
		firstEntry = true;
		out.write("[");
		out.indent();
	}

	@Override
	public void preSetElement() throws IOException
	{
		firstEntry = false;
		out.println();
	}

	@Override
	public void endSet() throws IOException
	{
		if (!firstEntry) out.println();
		firstEntry = false;
		out.dedent();
		out.write("]");
	}

	@Override
	public void beginMap() throws IOException
	{
		out.write("[");
		out.indent();
	}

	@Override
	public void preMapKey() throws IOException
	{
		out.println();
	}

	@Override
	public void preMapValue() throws IOException
	{
		out.write(" -> ");
	}

	@Override
	public void endMap() throws IOException
	{
		if (!firstEntry) out.println();
		firstEntry = false;
		out.dedent();
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
		CksTextFormatter.writeString(out, value);
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
}

