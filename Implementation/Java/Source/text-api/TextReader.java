package cks.io;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public abstract class TextReader<T>
{
	public abstract T readImpl(Parser p) throws IOException, ProblemException;
	public cks.Maybe<T> getDefault() { return cks.Maybe.Nothing(); }

    public final T getOptionDefault(Parser p)
        throws ProblemException
    {
        cks.Maybe<T> m = getDefault();
        if (m.isNothing()) throw ProblemException.pex(p.getLastIdentSourcePos(), "option requires a value");
        return m.getJust();
    }

    public final T getFieldDefault(SourcePos recordStart, String fieldName)
        throws ProblemException
    {
        cks.Maybe<T> m = getDefault();
        if (m.isNothing()) throw ProblemException.pex(recordStart, "missing field \"" + fieldName + "\"'");
        return m.getJust();
    }

    // -------------------------------------------------
    // Convenience functions for clients.

    public T read(Parser p) throws IOException, ProblemException
    {
        T value = readImpl(p);
        p.endInput();
        return value;
    }

	// -------------------------------------------------
	// Implementations for the standard types.

	public static final class List<T> extends TextReader<java.util.List<T>>
	{
		private final TextReader<T> mT;

		public List(TextReader<T> mT)
		{
			this.mT = mT;
		}

		public static <T> List<T> mk(TextReader<T> mT)
		{
			return new List<T>(mT);
		}

		public java.util.List<T> readImpl(Parser in) throws IOException, ProblemException
		{
			ArrayList<T> l = new ArrayList<T>();
			in.beginList();
			while (in.hasListNext() != null) {
				T elem = mT.readImpl(in);
				l.add(elem);
			}
			return l;
		}
	}

	public static final class Set<T> extends TextReader<java.util.Set<T>>
	{
		private final TextReader<T> mT;

		public Set(TextReader<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Set<T> mk(TextReader<T> mT)
		{
			return new Set<T>(mT);
		}

		public java.util.Set<T> readImpl(Parser in) throws IOException, ProblemException
		{
			HashSet<T> set = new HashSet<T>();
			in.beginSet();
			SourcePos loc;
			while ((loc = in.hasSetNext()) != null) {
				T elem = mT.readImpl(in);
				boolean added = set.add(elem);
				if (!added) {
					throw ProblemException.pex(loc, "duplicate entry in set");
				}
			}
			return set;
		}
	}

	public static final class Map<K,V> extends TextReader<java.util.Map<K,V>>
	{
		private final TextReader<K> mK;
		private final TextReader<V> mV;

		public Map(TextReader<K> mK, TextReader<V> mV)
		{
			this.mK = mK;
			this.mV = mV;
		}

		public static <K,V> Map<K,V> mk(TextReader<K> mK, TextReader<V> mV)
		{
			return new Map<K,V>(mK, mV);
		}

		public java.util.Map<K,V> readImpl(Parser in) throws IOException, ProblemException
		{
            HashMap<K,V> r = new HashMap<K, V>();

            in.beginMap();
            while (true) {
                SourcePos keyPos = in.hasMapNext();
                if (keyPos == null) break;
                K key = mK.readImpl(in);

                V value;
                if (in.preMapValue() == null) {
                    // No value.  Use default.
                    cks.Maybe<V> def = mV.getDefault();
                    if (def.isNothing()) throw ProblemException.pex(keyPos, "map value required (there's no default)");
                    value = def.getJust();
                } else {
                    value = mV.readImpl(in);
                }

                Object displaced = r.put(key, value);
                if (displaced != null) throw ProblemException.pex(keyPos, "duplicate key");
            }

            return r;
		}
	}

	public static final TextReader<BigInteger> Nat = new TextReader<BigInteger>()
	{
		public BigInteger readImpl(Parser in) throws IOException, ProblemException
		{
			return in.parseNat();
		}
	};

	public static final TextReader<BigInteger> Int = new TextReader<BigInteger>()
	{
		public BigInteger readImpl(Parser in) throws IOException, ProblemException
		{
			return in.parseInt();
		}
	};

    public static final Nat8 Nat8 = new Nat8();
	public static final class Nat8 extends TextReader<Short>
	{
		public Short readImpl(Parser in) throws IOException, ProblemException
		{
			return in.parseNat8();
		}

        public static short readPrimitive(Parser in) throws IOException, ProblemException
        {
            return in.parseNat8();
        }
	}

    public static final Int8 Int8 = new Int8();
    public static final class Int8 extends TextReader<Byte>
	{
        public Byte readImpl(Parser in) throws IOException, ProblemException
        {
            return in.parseInt8();
        }

		public static byte readPrimitive(Parser in) throws IOException, ProblemException
		{
			return in.parseInt8();
		}
	}

	public static final Nat16 Nat16 = new Nat16();
    public static final class Nat16 extends TextReader<Integer>
	{
        public Integer readImpl(Parser in) throws IOException, ProblemException
        {
            return in.parseNat16();
        }

		public static int readPrimitive(Parser in) throws IOException, ProblemException
		{
			return in.parseNat16();
		}
	}

	public static final Int16 Int16 = new Int16();
    public static final class Int16 extends TextReader<Short>
	{
        public Short readImpl(Parser in) throws IOException, ProblemException
        {
            return in.parseInt16();
        }

		public short readPrimitive(Parser in) throws IOException, ProblemException
		{
			return in.parseInt16();
		}
    }

    public static final Nat32 Nat32 = new Nat32();
	public static final class Nat32 extends TextReader<Long>
	{
        public Long readImpl(Parser in) throws IOException, ProblemException
        {
            return in.parseNat32();
        }

		public long readPrimitive(Parser in) throws IOException, ProblemException
		{
			return in.parseNat32();
		}
	}

    public static final Int32 Int32 = new Int32();
	public static final class Int32 extends TextReader<Integer>
	{
        public Integer readImpl(Parser in) throws IOException, ProblemException
        {
            return in.parseInt32();
        }

		public int readPrimitive(Parser in) throws IOException, ProblemException
		{
			return in.parseInt32();
		}
	}

	public static final TextReader<BigInteger> Nat64 = new TextReader<BigInteger>()
	{
		public BigInteger readImpl(Parser in) throws IOException, ProblemException
		{
			return in.parseNat64();
		}
	};

    public static final Int64 Int64 = new Int64();
	public static final class Int64 extends TextReader<Long>
	{
        public Long readImpl(Parser in) throws IOException, ProblemException
        {
            return in.parseInt64();
        }

		public long readPrimitive(Parser in) throws IOException, ProblemException
		{
			return in.parseInt64();
		}
	}

	public static final TextReader<String> String = new TextReader<String>()
	{
		public String readImpl(Parser in) throws IOException, ProblemException
		{
			return in.parseString();
		}
	};

    public static final Bool Bool = new Bool();
	public static final class Bool extends TextReader<Boolean>
	{
        public Boolean readImpl(Parser in) throws IOException, ProblemException
        {
            return in.parseBool();
        }

		public boolean readPrimitive(Parser in) throws IOException, ProblemException
		{
			return in.parseBool();
		}
	}

	public static final TextReader<cks.Void> Void = new TextReader<cks.Void>()
	{
		public cks.Void readImpl(Parser in) throws IOException, ProblemException
		{
			in.parseVoid();
			return cks.Void.Instance;
		}

		public cks.Maybe<cks.Void> getDefault() { return VoidDefault; }
	};
	private static final cks.Maybe<cks.Void> VoidDefault = cks.Maybe.Just(cks.Void.Instance);

	public static final class Maybe<T> extends TextReader<cks.Maybe<T>>
	{
		private final TextReader<T> mT;

		public Maybe(TextReader<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Maybe<T> mk(TextReader<T> mT)
		{
			return new Maybe<T>(mT);
		}

		public cks.Maybe<T> readImpl(Parser in) throws IOException, ProblemException
		{
			Boolean state = in.beginMaybe();
			if (state == null) {
				return cks.Maybe.Nothing();
			}
			else if (state) {
				return cks.Maybe.Just(mT.readImpl(in));
			}
			else {
				cks.Maybe<T> v = mT.getDefault();
				if (v.isNothing()) {
					throw ProblemException.pex(in.getLastIdentSourcePos(), "missing value");
				}
				return cks.Maybe.Just(v.getJust());
			}
		}

        private static final cks.Maybe<cks.Maybe<?>> Default = cks.Maybe.<cks.Maybe<?>>Just(cks.Maybe.Nothing());

		public cks.Maybe<cks.Maybe<T>> getDefault()
		{
			@SuppressWarnings("unchecked")
			cks.Maybe<cks.Maybe<T>> d = (cks.Maybe<cks.Maybe<T>>) (Object) Default;
			return d;
		}
	}

	public static final class MaybeNull<T> extends TextReader<T>
	{
		private final TextReader<T> mT;

		public MaybeNull(TextReader<T> mT)
		{
			this.mT = mT;
		}

		public static <T> MaybeNull<T> mk(TextReader<T> mT)
		{
			return new MaybeNull<T>(mT);
		}

		public T readImpl(Parser in) throws IOException, ProblemException
		{
			Boolean state = in.beginMaybe();
			if (state == null) {
				return null;
			}
			else if (state) {
				return mT.readImpl(in);
			}
			else {
				cks.Maybe<T> v = mT.getDefault();
				if (v.isNothing()) {
					throw ProblemException.pex(in.getLastIdentSourcePos(), "missing value");
				}
				return v.getJust();
			}
		}

        private static final cks.Maybe<?> Default = cks.Maybe.Just(null);

        public cks.Maybe<T> getDefault()
        {
            @SuppressWarnings("unchecked")
            cks.Maybe<T> casted = (cks.Maybe<T>) Default;
            return casted;
        }
	}
}
