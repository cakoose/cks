package cks.io;

import cks.io.SourcePos;

import java.math.BigInteger;

public class Token {

	public final SourcePos loc;
	public final int type;

	public Token(SourcePos loc, int type)
	{
		this.loc = loc;
		this.type = type;
	}

	public static class LitInt extends Token
	{
		public final BigInteger value;
		public LitInt(SourcePos loc, BigInteger value)
		{
			super(loc, Codes.LitInt);
			this.value = value;
		}

		public void toStringContent(StringBuilder buf)
		{
			super.toStringContent(buf);
			buf.append(' ');
			buf.append(value);
		}
	}

	public static class LitString extends Token
	{
		public final String value;
		public LitString(SourcePos loc, String value)
		{
			super(loc, Codes.LitString);
			this.value = value;
		}

		public void toStringContent(StringBuilder buf)
		{
			super.toStringContent(buf);
			buf.append(" \"");
			escapedString(buf, value, '"');
			buf.append("\"");
		}
	}

	public static class Text extends Token
	{
		public final String value;
		public Text(SourcePos loc, String value)
		{
			super(loc, Codes.Text);
			this.value = value;
		}

		public void toStringContent(StringBuilder buf)
		{
			super.toStringContent(buf);
			buf.append("<>");
			escapedString(buf, value, '<');
			buf.append("</>");
		}
	}

	public static void escapedString(StringBuilder buf, String value, char endChar)
	{
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			switch (c) {
				case '\\':
					buf.append('\\');
					buf.append(c);
					break;
				case '\t': buf.append("\\t"); break;
				case '\n': buf.append("\\n"); break;
				case '\r': buf.append("\\r"); break;
				case '\0': buf.append("\\0"); break;
				default:
					if (c == endChar) {
						buf.append("\\");
					}
					buf.append(c);
					// TODO: unicode escapes
					break;
			}
		}
	}

	public static class Ident extends Token
	{
		public final String text;
		public Ident(SourcePos loc, String text)
		{
			super(loc, Codes.Ident);
			this.text = text;
		}

		public void toStringContent(StringBuilder buf)
		{
			super.toStringContent(buf);
			buf.append(' ');
			buf.append(text);
		}
	}

	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		toString(builder);
		return builder.toString();
	}

	public void toString(StringBuilder buf)
	{
		buf.append("[");
		loc.toString(buf);
		buf.append("]");
		toStringContent(buf);
	}

	public String toStringContent()
	{
		StringBuilder builder = new StringBuilder();
		toStringContent(builder);
		return builder.toString();
	}

	public void toStringContent(StringBuilder buf)
	{
		toStringContent(buf, this.type);
	}

	public static void toStringContent(StringBuilder buf, int type)
	{
		if (type < Codes.FirstCode) {
			if (type == '"' || type == '\'') {
				buf.append('(');
				buf.append((char) type);
				buf.append(')');
			} else {
				buf.append('"');
				buf.append((char) type);
				buf.append('"');
			}
		} else {
			int index = type - Codes.FirstCode;
			Codes.Entry e = Codes.Entries[index];
			buf.append(e.name);
		}
	}
}
