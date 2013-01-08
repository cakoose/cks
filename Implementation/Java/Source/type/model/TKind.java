package cks.type.model;

import java.util.Arrays;

public abstract class TKind
{
	private TKind() {}

	public String toString()
	{
		StringBuilder b = new StringBuilder();
		toString(b);
		return b.toString();
	}

	public abstract void toString(StringBuilder b);

	public static final class Func extends TKind
	{
		public final TKind[] params;

		public Func(TKind... params)
		{
			assert params != null;
			assert params.length >= 1;

			this.params = params;
		}

		public boolean equals(Object o)
		{
			return getClass().equals(o.getClass()) && equals((TKind.Func)o);
		}

		public boolean equals(TKind.Func o)
		{
			return Arrays.equals(params, o.params);
		}

		public void toString(StringBuilder b)
		{
			b.append('(');
			String sep = "";
			for (TKind param : params) {
				b.append(sep); sep = ", ";
				param.toString(b);
			}
			b.append(") -> *");
		}
	}

	public static final class Base extends TKind
	{
		private String name;
		private Base(String name) { this.name = name; }

		public String toString() { return name; }
		public void toString(StringBuilder b) { b.append(toString()); }
	}

	public static final TKind Base = new Base("*");
}
