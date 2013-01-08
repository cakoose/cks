package cks.type.model;

import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

public class TOpaque extends TBinding
{
	public final TKind kind;
	public final String textName;

	private TOpaque(String name, TKind kind)
	{
		this(name, name, kind);
	}
	
	private TOpaque(String name, String textName, TKind kind)
	{
		super(name);
		this.textName = textName;
		this.kind = kind;
	}

	public TKind getKind() { return kind; }

	public static final Map<String,TBinding> StandardContext = new HashMap<String,TBinding>();
	public static final Map<String,TBinding> EmptyContext = Collections.emptyMap();

	public static final TOpaque Void = mk("()", "Void");
	public static final TOpaque List = st(mk("List", new TKind.Func(TKind.Base)));
	public static final TOpaque Set = st(mk("Set", new TKind.Func(TKind.Base)));
	public static final TOpaque Map = st(mk("Map", new TKind.Func(TKind.Base, TKind.Base)));
	public static final TOpaque Maybe = mk("?", "Maybe", new TKind.Func(TKind.Base));
	public static final TOpaque String = st(mk("String"));
	public static final TOpaque Bool = st(mk("Bool"));
	public static final TOpaque Any = st(mk("Any"));

	public static final TOpaque Nat = st(new IntType(false, 0));
	public static final TOpaque Int = st(new IntType(true, 0));

	public static final TOpaque Nat64 = st(new IntType(false, 64));
	public static final TOpaque Nat32 = st(new IntType(false, 32));
	public static final TOpaque Nat16 = st(new IntType(false, 16));
	public static final TOpaque Nat8 = st(new IntType(false, 8));
	public static final TOpaque Int64 = st(new IntType(true, 64));
	public static final TOpaque Int32 = st(new IntType(true, 32));
	public static final TOpaque Int16 = st(new IntType(true, 16));
	public static final TOpaque Int8 = st(new IntType(true, 8));

	public static final class IntType extends TOpaque
	{
		public final boolean signed;
		public final int numBits; // "0" means "unlimited"

		public IntType(boolean signed, int numBits)
		{
			super(makeName(signed, numBits), TKind.Base);
			assert numBits >= 0;
			this.signed = signed;
			this.numBits = numBits;
		}

		private static String makeName(boolean signed, int numBits)
		{
			StringBuilder buf = new StringBuilder();
			if (signed) {
				buf.append("Int");
			}
			else {
				buf.append("Nat");
			}
			if (numBits > 0) {
				buf.append(numBits);
			}
			return buf.toString();
		}
	}

	private static TOpaque mk(String name)
	{
		return mk(name, TKind.Base);
	}

	private static TOpaque mk(String name, TKind kind)
	{
		return new TOpaque(name, kind);
	}
	
	private static TOpaque mk(String name, String textName)
	{
		return mk(name, textName, TKind.Base);
	}

	private static TOpaque mk(String name, String textName, TKind kind)
	{
		return new TOpaque(name, textName, kind);
	}

	// Add the given type to StandardContext (and then return the type for convenience).
	private static TOpaque st(TOpaque b)
	{
		Object displaced = StandardContext.put(b.name, b);
		assert displaced == null : b.name;
		return b;
	}
}
