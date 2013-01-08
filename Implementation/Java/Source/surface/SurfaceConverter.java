package cks.surface;

import cks.Void;
import cks.io.CksTextTokenizer;
import cks.io.ProblemException;
import static cks.io.ProblemException.pex;
import cks.io.SourcePos;
import cks.surface.model.*;

import java.io.*;
import java.math.BigInteger;

public abstract class SurfaceConverter<T>
{
	// -------------------------------------------------
	// The only methods in the interface.

	public abstract T convert(Value v) throws ProblemException;
	public cks.Maybe<T> getDefault() { return cks.Maybe.Nothing(); } // Most types don't have default values.
	public T convertText(VTag.Text t) throws ProblemException
	{
		expectAllWhitespace(t, "value");
		return null;
	}

	// -------------------------------------------------
	// Convenience functions for clients.

	public final T read(InputStream in)
		throws IOException, ProblemException
	{
		return read(new InputStreamReader(in, "UTF-8"));
	}

	public final T read(Reader in)
		throws IOException, ProblemException
	{
		Value value = SurfaceParser.parse(new CksTextTokenizer(in));
		return convert(value);
	}

	public final T readFromByteArray(byte[] data)
		throws IOException, ProblemException
	{
		return read(new ByteArrayInputStream(data));
	}

	private static AssertionError badType(Object o)
	{
		throw new AssertionError("bad type: " + (o == null ? "null" : o.getClass().getName()));
	}

	// -------------------------------------------------
	// Helper functions for the generated code.

	public static java.util.Map<String,VEntry> getFields(Value v, String implicit)
		throws ProblemException
	{
		if (v instanceof VRecord) {
			return ((VRecord)v).fields;
		} else if (v instanceof VTag.Body) {
			VTag.Body body = (VTag.Body) v;
			java.util.Map<String,VEntry> fields = new java.util.HashMap<String,VEntry>(body.attrs);
			getFieldsFromTagBody(fields, body, implicit);
			return fields;
		} else if (v instanceof VTag.Content) {
			java.util.Map<String,VEntry> fields = new java.util.HashMap<String,VEntry>();
			getRecordFromTagContent(fields, (VTag.Content) v, implicit);
			return fields;
		} else {
			throw pex(v.loc, "expecting record value");
		}
	}

	private static void getFieldsFromTagBody(java.util.Map<String, VEntry> fields, VTag.Body body, String implicit)
		throws ProblemException
	{
		if (implicit == null) {
			getFieldsFromTagContent(fields, body.content);
		}
		else {
			if (body.content != null) {
				VEntry existing = fields.put(implicit, new VEntry(body.loc, implicit, body.content));
				if (existing != null) {
					throw pex(body.loc, "expecting leaf tag (no content) since implicit field \"" + implicit + "\" is already defined",
					          existing.loc, "previous field definition");
				}
			}
		}
	}

	private static void getFieldsFromTagContent(java.util.Map<String, VEntry> fields, VTag.Content content)
		throws ProblemException
	{
		for (VTag.Child child : content.children) {
			if (child instanceof VTag.Full) {
				VTag.Full full = (VTag.Full) child;
				VEntry existing = fields.put(full.name, new VEntry(full.loc, full.name, full.rest));
				if (existing != null) {
					throw pex(full.loc, "duplicate record field", existing.loc, "previous field definition");
				}
			}
			else if (child instanceof VTag.Body) {
				throw pex(child.getNode().loc, "expecting record field; found non-named tag");
			}
			else if (child instanceof VTag.Text) {
				SourcePos nonWhitespace = findNonWhitespace((VTag.Text)child);
				if (nonWhitespace != null) {
					throw pex(nonWhitespace, "expecting one record value (in a tag); found text content");
				}
			}
			else {
				throw badType(child);
			}
		}
	}

	private static void getRecordFromTagContent(java.util.Map<String, VEntry> fields, VTag.Content content, String implicit)
		throws ProblemException
	{
		VTag.Tag inner = null;
		for (VTag.Child child : content.children) {
			if (child instanceof VTag.Tag) {
				if (inner != null) {
					throw pex(child.getNode().loc, "expecting one record value; found extra tag");
				}
				inner = (VTag.Tag) child;
			}
			else if (child instanceof VTag.Text) {
				SourcePos nonWhitespace = findNonWhitespace((VTag.Text)child);
				if (nonWhitespace != null) {
					throw pex(nonWhitespace, "expecting one record value (in a tag); found text content");
				}
			}
			else {
				throw badType(child);
			}
		}
		if (inner == null) {
			throw pex(content.loc, "expecting a record value in tag content");
		}
		if (inner instanceof VTag.Body) {
			getFieldsFromTagBody(fields, (VTag.Body) inner, implicit);
		} else if (inner instanceof VTag.Full) {
			throw pex(inner.getNode().loc, "expecting record type (without a tag name)");
		} else {
			throw badType(inner);
		}
	}

	public static SourcePos findNonWhitespace(VTag.Text text)
	{
		int line = text.loc.line;
		int column = text.loc.column;
		String v = text.value;
		for (int i = 0; i < v.length(); i++) {
			char c = v.charAt(i);
			if (c == '\n') {
				line++;
				column = 1;
			}
			else if (c == ' ' || c == '\t') {
				column++;
			}
			else {
				return new SourcePos(line, column);
			}
		}
		return null;
	}

	public static <T> T getField(Value v, MInteger numFieldsUsed, java.util.Map<java.lang.String,VEntry> fields, java.lang.String name, SurfaceConverter<T> m)
		throws ProblemException
	{
		VEntry e = fields.get(name);

		if (e == null) {
			// Field not present.  See if we can supply a default value.
			cks.Maybe<T> mDef = m.getDefault();
			if (mDef.isJust()) {
				return mDef.getJust();
			} else {
				throw pex(v.loc, "missing entry for field \"" + name + "\"");
			}
		}

		// Field is present.  Delegate to the given converter.
		numFieldsUsed.value++;
		return m.convert(e.value);
	}

	public static void reportExtraFields(java.util.Map<String,VEntry> givenFields, String[] validFields)
		throws ProblemException
	{
		java.util.HashSet<String> given = new java.util.HashSet<String>(givenFields.keySet());
		given.removeAll(java.util.Arrays.asList(validFields));
		assert given.size() > 0; // The caller should have checked this already.
		String extraField = given.iterator().next();
		VEntry extra = givenFields.get(extraField);
		throw pex(extra.loc, "record type does not have a field named \"" + extraField + "\"");
	}

	public static VEntry getVariant(Value v)
		throws ProblemException
	{
		if (v instanceof VEntry) return ((VEntry) v);
		throw pex(v.loc, "expecting a variant value");
	}

	// -------------------------------------------------
	// Implementations for the standard types.

	public static final SurfaceConverter<Boolean> Bool = new SurfaceConverter<Boolean>()
	{
		public java.lang.Boolean convert(Value v)
			throws ProblemException
		{
			if (v instanceof VEntry) {
				VEntry e = (VEntry) v;
				java.lang.Boolean r;
				if (e.name.equals("True")) {
					r = Boolean.TRUE;
				} else if (e.name.equals("False")) {
					r = Boolean.FALSE;
				} else {
					throw pex(v.loc, "expecting a boolean value (either \"True\" or \"False\")");
				}
				if (!(e.value instanceof VPrimitive.Void)) {
					throw pex(v.loc, "option \"" + e.name + "\" requires a void value");
				}
				return r;
			}
			else {
				throw pex(v.loc, "expecting a boolean value (either \"True\" or \"False\")");
			}
		}
	};

	public static void expectAllWhitespace(VTag.Text value, String thing)
		throws ProblemException
	{
		SourcePos nonWhitespace = findNonWhitespace(value);
		if (nonWhitespace != null) {
			throw pex(nonWhitespace, "not expecting text content for " + thing);
		}
	}

	public static final class Maybe<T> extends SurfaceConverter<cks.Maybe<T>>
	{
		public final SurfaceConverter<T> mT;

		public Maybe(SurfaceConverter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Maybe<T> mk(SurfaceConverter<T> mT)
		{
			return new Maybe<T>(mT);
		}

		public cks.Maybe<T> convert(Value v)
			throws ProblemException
		{
			if (v instanceof VEntry) {
				VEntry e = (VEntry) v;
				if (e.name.equals("Set")) {
					T t = mT.convert(e.value);
					return cks.Maybe.Just(t);
				}
				else if (e.name.equals("None")) {
					if (!(e.value instanceof VPrimitive.Void)) {
						throw pex(v.loc, "option \"None\" requires a void value");
					}
					return cks.Maybe.Nothing();
				}
				else {
					throw pex(v.loc, "expecting an optional value (either \"Set\" with a value or \"None\")");
				}
			}
			else {
				throw pex(v.loc, "expecting an optional value (either \"Set\" with a value or \"None\")");
			}
		}

		public cks.Maybe<cks.Maybe<T>> getDefault()
		{
			return cks.Maybe.Just(cks.Maybe.<T>Nothing());
		}
	}

	public static final class MaybeNull<T> extends SurfaceConverter<T>
	{
		public final SurfaceConverter<T> mT;

		public MaybeNull(SurfaceConverter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> MaybeNull<T> mk(SurfaceConverter<T> mT)
		{
			return new MaybeNull<T>(mT);
		}

		public T convert(Value v)
			throws ProblemException
		{
			if (v instanceof VEntry) {
				VEntry e = (VEntry) v;
				if (e.name.equals("Set")) {
					return mT.convert(e.value);
				}
				else if (e.name.equals("None")) {
					if (!(e.value instanceof VPrimitive.Void)) {
						throw pex(v.loc, "option \"None\" requires a void value");
					}
					return null;
				}
				else {
					throw pex(v.loc, "expecting an optional value (either \"Set\" with a value or \"None\")");
				}
			}
			else {
				throw pex(v.loc, "expecting an optional value (either \"Set\" with a value or \"None\")");
			}
		}

		public cks.Maybe<T> getDefault()
		{
			return cks.Maybe.Just(null);
		}
	}

	public static final SurfaceConverter<Void> Void = new SurfaceConverter<Void>()
	{
		public cks.Void convert(Value v)
			throws ProblemException
		{
			if (v instanceof VPrimitive.Void) {
				return cks.Void.Instance;
			} else {
				throw pex(v.loc, "expecting void");
			}
		}

		public cks.Maybe<cks.Void> getDefault() { return cks.Maybe.Just(cks.Void.Instance); }
	};

	public static final SurfaceConverter<String> String = new SurfaceConverter<String>()
	{
		public java.lang.String convert(Value v)
			throws ProblemException
		{
			if (v instanceof VPrimitive.String) {
				VPrimitive.String s = (VPrimitive.String) v;
				return s.value;
			} else if (v instanceof VTag.Body) {
				return getString(((VTag.Body)v).content);
			} else if (v instanceof VTag.Content) {
				return getString((VTag.Content)v);
			} else {
				throw pex(v.loc, "expecting a string");
			}
		}

		public String convertText(VTag.Text t)
			throws ProblemException
		{
			return t.value;
		}
	};

	private static String getString(VTag.Content content)
		throws ProblemException
	{
		if (content.children.isEmpty()) {
			throw pex(content.loc, "expecting string content, found empty tag");
		}
		if (content.children.size() > 1) {
			throw pex(content.loc, "expecting string content, found mixed content");
		}

		VTag.Child child = content.children.get(0);
		if (child instanceof VTag.Text) {
			return ((VTag.Text)child).value;
		}
		else {
			throw pex(content.loc, "expecting string content, found tag");
		}
	}

	public static final class List<T> extends SurfaceConverter<java.util.List<T>>
	{
		public final SurfaceConverter<T> mT;

		public List(SurfaceConverter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> List<T> mk(SurfaceConverter<T> mT)
		{
			return new List<T>(mT);
		}

		public java.util.List<T> convert(Value v)
			throws ProblemException
		{
			if (v instanceof VCollection) {
				VCollection l = (VCollection) v;
				java.util.ArrayList<T> r = new java.util.ArrayList<T>();
				for (VCollection.Element elem : l.elements) {
					if (elem.arrow != null) {
						throw pex(elem.arrow, "expecting a list element, found a map entry");
					}
					r.add(mT.convert(elem.first));
				}
				return r;
			}
			else if (v instanceof VTag.Body) {
				return getList(((VTag.Body)v).content);
			}
			else if (v instanceof VTag.Content) {
				return getList((VTag.Content)v);
			}
			else {
				throw pex(v.loc, "expecting a list");
			}
		}

		private java.util.List<T> getList(VTag.Content v)
			throws ProblemException
		{
			java.util.ArrayList<T> l = new java.util.ArrayList<T>();
			for (VTag.Child child : v.children) {
				if (child instanceof VTag.Text) {
					T e = mT.convertText((VTag.Text) child);
					if (e != null) {
						l.add(e);
					}
				}
				else if (child instanceof VTag.Tag) {
					l.add(mT.convert((VTag.Tag) child));
				}
				else {
					throw badType(child);
				}
			}
			return l;
		}
	}

	public static final class Set<T> extends SurfaceConverter<java.util.Set<T>>
	{
		public final SurfaceConverter<T> mT;

		public Set(SurfaceConverter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Set<T> mk(SurfaceConverter<T> mT)
		{
			return new Set<T>(mT);
		}

		public java.util.Set<T> convert(Value v)
			throws ProblemException
		{
			if (v instanceof VCollection) {
				VCollection l = (VCollection) v;
				java.util.Set<T> r = new java.util.HashSet<T>();
				for (VCollection.Element elem : l.elements) {
					if (elem.arrow != null) {
						throw pex(elem.arrow, "expecting a list element, found a map entry");
					}
					boolean added = r.add(mT.convert(elem.first));
					if (!added) {
						throw pex(elem.first.loc, "duplicate entry in set");
					}
				}
				return r;
			}
			else {
				throw pex(v.loc, "expecting a set");
			}
		}
	}

	public static final class Map<K,V> extends SurfaceConverter<java.util.Map<K,V>>
	{
		public final SurfaceConverter<K> mK;
		public final SurfaceConverter<V> mV;

		public Map(SurfaceConverter<K> mK, SurfaceConverter<V> mV)
		{
			this.mK = mK;
			this.mV = mV;
		}

		public static <K,V> Map<K,V> mk(SurfaceConverter<K> mK, SurfaceConverter<V> mV)
		{
			return new Map<K,V>(mK, mV);
		}

		public java.util.Map<K,V> convert(Value v)
			throws ProblemException
		{
			if (v instanceof VCollection) {
				VCollection l = (VCollection) v;
				java.util.Map<K,V> r = new java.util.HashMap<K,V>();
				for (VCollection.Element elem : l.elements) {
					K key = mK.convert(elem.first);
					V value;
					if (elem.arrow == null) {
						cks.Maybe<V> mDef = mV.getDefault();
						if (mDef.isJust()) {
							value = mDef.getJust();
						} else {
							throw pex(elem.first.loc, "missing value for map entry");
						}
					} else {
						value = mV.convert(elem.second);
					}

					Object displaced = r.put(key, value);
					if (displaced != null) {
						throw pex(elem.first.loc, "duplicate key in map");
					}
				}
				return r;
			}
			else {
				throw pex(v.loc, "expecting a map");
			}
		}
	}

	// ------------------------------------------------------------------------
	// Int/Nat

	public static final SurfaceConverter<BigInteger> Int = new SurfaceConverter<BigInteger>()
	{
		public BigInteger convert(Value v)
			throws ProblemException
		{
			if (v instanceof VPrimitive.Int) {
				VPrimitive.Int i = (VPrimitive.Int) v;
				return i.value;
			}
			else {
				throw pex(v.loc, "expecting an integer");
			}
		}
	};

	public static final SurfaceConverter<BigInteger> Nat = new SurfaceConverter<BigInteger>()
	{
		public BigInteger convert(Value v)
			throws ProblemException
		{
			if (v instanceof VPrimitive.Int) {
				VPrimitive.Int i = (VPrimitive.Int) v;
				BigInteger bi = i.value;
				if (bi.compareTo(BigInteger.ZERO) < 0) {
					throw pex(v.loc, "expecting a natural number; negative integers are not allowed");
				}
				return i.value;
			}
			else {
				throw pex(v.loc, "expecting a natural number");
			}
		}
	};

	// ------------------------------------------------------------------------
	// Bound Int/Nat

	public static abstract class BoundNat<T> extends SurfaceConverter<T>
	{
		private final int numBits;
		private final BigInteger max;

		protected BoundNat(int numBits)
		{
			this.numBits = numBits;
			this.max = BigInteger.ONE.shiftLeft(numBits).subtract(BigInteger.ONE);
		}

		protected abstract T convert(BigInteger i);

		public T convert(Value v)
			throws ProblemException
		{
			if (v instanceof VPrimitive.Int) {
				BigInteger i = ((VPrimitive.Int)v).value;
				if (i.compareTo(BigInteger.ZERO) < 0) {
					throw pex(v.loc, "expecting a natural number; negative integers are not allowed");
				}
				if (i.compareTo(max) > 0) {
					throw pex(v.loc, "expecting a " + numBits + "-bit natural number; value cannot be greater than " + max);
				}
				return convert(i);
			}
			else {
				throw pex(v.loc, "expecting a natural number");
			}
		}
	}

	public static abstract class BoundInt<T> extends SurfaceConverter<T>
	{
		private final int numBits;
		private final BigInteger max;
		private final BigInteger min;

		protected BoundInt(int numBits)
		{
			this.numBits = numBits;
			BigInteger half = BigInteger.ONE.shiftLeft(numBits-1);
			this.max = half.subtract(BigInteger.ONE);
			this.min = half.negate();
		}

		protected abstract T convert(BigInteger i);

		public T convert(Value v)
			throws ProblemException
		{
			if (v instanceof VPrimitive.Int) {
				BigInteger i = ((VPrimitive.Int)v).value;
				if (i.compareTo(min) < 0) {
					throw pex(v.loc, "expecting an " + numBits + "-bit integer; value cannot be less than " + min);
				}
				if (i.compareTo(max) > 0) {
					throw pex(v.loc, "expecting an " + numBits + "-bit integer; value cannot be greater than " + max);
				}
				return convert(i);
			}
			else {
				throw pex(v.loc, "expecting an integer");
			}
		}
	}

	public static final BoundNat<BigInteger> Nat64 = new BoundNat<BigInteger>(64) {
		protected BigInteger convert(BigInteger i) { return i; }
	};
	public static final BoundNat<java.lang.Long> Nat32 = new BoundNat<java.lang.Long>(32) {
		protected java.lang.Long convert(BigInteger i) { return i.longValue(); }
	};
	public static final BoundNat<java.lang.Integer> Nat16 = new BoundNat<java.lang.Integer>(16) {
		protected java.lang.Integer convert(BigInteger i) { return i.intValue(); }
	};
	public static final BoundNat<java.lang.Short> Nat8 = new BoundNat<java.lang.Short>(8) {
		protected java.lang.Short convert(BigInteger i) { return i.shortValue(); }
	};

	public static final BoundInt<java.lang.Long> Int64 = new BoundInt<java.lang.Long>(64) {
		protected java.lang.Long convert(BigInteger i) { return i.longValue(); }
	};
	public static final BoundInt<java.lang.Integer> Int32 = new BoundInt<java.lang.Integer>(32) {
		protected java.lang.Integer convert(BigInteger i) { return i.intValue(); }
	};
	public static final BoundInt<java.lang.Short> Int16 = new BoundInt<java.lang.Short>(16) {
		protected java.lang.Short convert(BigInteger i) { return i.shortValue(); }
	};
	public static final BoundInt<java.lang.Byte> Int8 = new BoundInt<java.lang.Byte>(8) {
		protected java.lang.Byte convert(BigInteger i) { return i.byteValue(); }
	};
}
