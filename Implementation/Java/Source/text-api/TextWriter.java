package cks.io;

import java.io.IOException;
import java.math.BigInteger;

public abstract class TextWriter<T>
{
	public abstract void write(Formatter out, T value) throws IOException;

	// -------------------------------------------------
	// Implementations for the standard types.

	public static final class List<T> extends TextWriter<java.util.List<T>>
	{
		private final TextWriter<T> mT;

		public List(TextWriter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> List<T> mk(TextWriter<T> mT)
		{
			return new List<T>(mT);
		}

		public void write(Formatter out, java.util.List<T> list)
			throws IOException
		{
			out.beginList();
			for (T elem : list) {
				out.preListElement();
				mT.write(out, elem);
			}
			out.endList();
		}
	}

	public static final class Set<T> extends TextWriter<java.util.Set<T>>
	{
		private final TextWriter<T> mT;

		public Set(TextWriter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Set<T> mk(TextWriter<T> mT)
		{
			return new Set<T>(mT);
		}

		public void write(Formatter out, java.util.Set<T> set)
			throws IOException
		{
			out.beginSet();
			for (T elem : set) {
				out.preSetElement();
				mT.write(out, elem);
			}
			out.endSet();
		}
	}

	public static final class Map<K,V> extends TextWriter<java.util.Map<K,V>>
	{
		private final TextWriter<K> mK;
		private final TextWriter<V> mV;

		public Map(TextWriter<K> mK, TextWriter<V> mV)
		{
			this.mK = mK;
			this.mV = mV;
		}

		public static <K,V> Map<K,V> mk(TextWriter<K> mK, TextWriter<V> mV)
		{
			return new Map<K,V>(mK, mV);
		}

		public void write(Formatter out, java.util.Map<K,V> map)
			throws IOException
		{
			out.beginMap();
			for (java.util.Map.Entry<K,V> entry : map.entrySet()) {
				out.preMapKey();
				mK.write(out, entry.getKey());
				out.preMapValue();
				mV.write(out, entry.getValue());
			}
			out.endMap();
		}
	}

	public static final TextWriter<BigInteger> Nat = new TextWriter<BigInteger>()
	{
		public void write(Formatter out, BigInteger value) throws IOException
		{
			out.writeInt(value);
		}
	};

	public static final TextWriter<BigInteger> Int = new TextWriter<BigInteger>()
	{
		public void write(Formatter out, BigInteger value) throws IOException
		{
			out.writeInt(value);
		}
	};

	public static final TextWriter<Short> Nat8 = new TextWriter<Short>()
	{
		public void write(Formatter out, Short value) throws IOException
		{
			out.writeInt32(value);
		}
	};

	public static final TextWriter<Byte> Int8 = new TextWriter<Byte>()
	{
		public void write(Formatter out, Byte value) throws IOException
		{
			out.writeInt32(value);
		}
	};

	public static final TextWriter<Integer> Nat16 = new TextWriter<Integer>()
	{
		public void write(Formatter out, Integer value) throws IOException
		{
			out.writeInt32(value);
		}
	};

	public static final TextWriter<Short> Int16 = new TextWriter<Short>()
	{
		public void write(Formatter out, Short value) throws IOException
		{
			out.writeInt32(value);
		}
	};

	public static final TextWriter<Long> Nat32 = new TextWriter<Long>()
	{
		public void write(Formatter out, Long value) throws IOException
		{
			out.writeInt64(value);
		}
	};

	public static final TextWriter<Integer> Int32 = new TextWriter<Integer>()
	{
		public void write(Formatter out, Integer value) throws IOException
		{
			out.writeInt32(value);
		}
	};

	public static final TextWriter<BigInteger> Nat64 = new TextWriter<BigInteger>()
	{
		public void write(Formatter out, BigInteger value) throws IOException
		{
			out.writeInt(value);
		}
	};

	public static final TextWriter<Long> Int64 = new TextWriter<Long>()
	{
		public void write(Formatter out, Long value) throws IOException
		{
			out.writeInt64(value);
		}
	};

	public static final TextWriter<String> String = new TextWriter<String>()
	{
		public void write(Formatter out, String value) throws IOException
		{
			out.writeString(value);
		}
	};

	public static final TextWriter<Boolean> Bool = new TextWriter<Boolean>()
	{
		public void write(Formatter out, Boolean value) throws IOException
		{
			out.writeBool(value);
		}
	};

	public static final TextWriter<cks.Void> Void = new TextWriter<cks.Void>()
	{
		public void write(Formatter out, cks.Void value) throws IOException
		{
			out.writeVoid();
		}
	};

	public static final class Maybe<T> extends TextWriter<cks.Maybe<T>>
	{
		private final TextWriter<T> mT;

		public Maybe(TextWriter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Maybe<T> mk(TextWriter<T> mT)
		{
			return new Maybe<T>(mT);
		}

		public void write(Formatter out, cks.Maybe<T> v)
			throws IOException
		{
			if (v.isNothing()) {
				out.writeMaybeNone();
			} else {
				out.beginMaybeSet();
				mT.write(out, v.getJust());
				out.endMaybeSet();
			}
		}
	}

	public static final class MaybeNull<T> extends TextWriter<T>
	{
		private final TextWriter<T> mT;

		public MaybeNull(TextWriter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> MaybeNull<T> mk(TextWriter<T> mT)
		{
			return new MaybeNull<T>(mT);
		}

		public void write(Formatter out, T v)
			throws IOException
		{
			if (v == null) {
				out.writeMaybeNone();
			} else {
				out.beginMaybeSet();
				mT.write(out, v);
				out.endMaybeSet();
			}
		}
	}
}
