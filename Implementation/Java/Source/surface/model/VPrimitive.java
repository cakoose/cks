package cks.surface.model;

import cks.io.SourcePos;

import java.math.BigInteger;

public abstract class VPrimitive extends Value
{
	public VPrimitive(SourcePos loc)
	{
		super(loc);
	}

	public static class Void extends VPrimitive
	{
		public Void(SourcePos loc) { super(loc); }
	}

	public static class String extends VPrimitive
	{
		public final java.lang.String value;

		public String(SourcePos loc, java.lang.String value)
		{
			super(loc);

			assert value != null;
			this.value = value;
		}

		public void toString(StringBuilder b)
		{
			super.toString(b);
			b.append(": ");
			b.append('\"');
			toJavaString(b, this.value);
			b.append('\"');
		}
	}

	public static void toJavaString(StringBuilder b, java.lang.String s)
	{
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if (c >= 32 && c <= 126) {
				b.append(c); // normal ASCII character
			} else if (c == '\n') {
				b.append("\\n");
			} else if (c == '\r') {
				b.append("\\r");
			} else if (c == '\t') {
				b.append("\\t");
			} else if (c == '"') {
				b.append("\\\"");
			} else {
				b.append("\\u"); // weird forin character
				final java.lang.String HEX = "0123456789abcdef";
				int code = c;
				b.append(HEX.charAt((code >> 12) & 0xf));
				b.append(HEX.charAt((code >>  8) & 0xf));
				b.append(HEX.charAt((code >>  4) & 0xf));
				b.append(HEX.charAt((code >>  0) & 0xf));
			}
		}
	}


	public static class Int extends VPrimitive
	{
		public final BigInteger value;

		public Int(SourcePos loc, BigInteger value)
		{
			super(loc);
			this.value = value;
		}

		public void toString(StringBuilder b)
		{
			super.toString(b);
			b.append(": ");
			b.append(this.value);
		}
	}

	public static class Bool extends VPrimitive
	{
		public final boolean value;

		public Bool(SourcePos loc, boolean value)
		{
			super(loc);
			this.value = value;
		}

		public void toString(StringBuilder b)
		{
			super.toString(b);
			b.append(": ");
			b.append(this.value);
		}
	}

	public static class Maybe extends VPrimitive
	{
		public final Value value; // null -> Nothing

		public Maybe(SourcePos loc, Value value)
		{
			super(loc);
			this.value = value;
		}
	}
}
