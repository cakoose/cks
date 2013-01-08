package cks.io;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigInteger;

public abstract class BinaryWriter<T>
{
	// -------------------------------------------------
	// The only method in the interface.

	public abstract void write(OutputStream out, T value) throws IOException;

	// -------------------------------------------------
	// Convenience functions for clients.

	public final byte[] writeToByteArray(T value) throws IOException
	{
		// TODO: try optimizing for byte array output (compute size first, then write?)
		ByteArrayOutputStream bout = new ByteArrayOutputStream(256);
		write(bout, value);
		return bout.toByteArray();
	}

	// -------------------------------------------------
	// Helper functions for implementors.

	public static void writeByte(OutputStream out, int v) throws IOException
	{
		out.write(v);
	}

	public static void writeShort(OutputStream out, int v) throws IOException
	{
		out.write((v >>> 8) & 0xff);
		out.write((v >>> 0) & 0xff);
	}

	public static void writeInt(OutputStream out, int v) throws IOException
	{
		out.write((v >>> 24) & 0xff);
		out.write((v >>> 16) & 0xff);
		out.write((v >>>  8) & 0xff);
		out.write((v >>>  0) & 0xff);
	}

	public static void writeLong(OutputStream out, long v) throws IOException
	{
		writeInt(out, (int) (v >>> 32));
		writeInt(out, (int) (v >>> 0));
	}

	// -------------------------------------------------

	public static final BinaryWriter<BigInteger> Nat = new BinaryWriter<BigInteger>()
	{
		public void write(OutputStream out, BigInteger value) throws IOException
		{
			writeUnboundNat(out, value);
		}
	};

	public static final BinaryWriter<BigInteger> Int = new BinaryWriter<BigInteger>()
	{
		public void write(OutputStream out, BigInteger value) throws IOException
		{
			writeUnboundInt(out, value);
		}
	};

	public static final Nat8 Nat8 = new Nat8();
	public static final class Nat8 extends BinaryWriter<Short>
	{
		public void write(OutputStream out, Short value) throws IOException
		{
			writeByte(out, value.byteValue());
		}

		public static void writePrimitive(OutputStream out, short value) throws IOException
		{
			writeByte(out, (byte) value);
		}
	}

	public static final Int8 Int8 = new Int8();
	public static final class Int8 extends BinaryWriter<Byte>
	{
		public void write(OutputStream out, Byte value) throws IOException
		{
			writeByte(out, value);
		}

		public static void writePrimitive(OutputStream out, byte value) throws IOException
		{
			writeByte(out, value);
		}
	}

	public static final Nat16 Nat16 = new Nat16();
	public static final class Nat16 extends BinaryWriter<Integer>
	{
		public void write(OutputStream out, Integer value) throws IOException
		{
			writeShort(out, value.shortValue());
		}

		public static void writePrimitive(OutputStream out, int value) throws IOException
		{
			writeShort(out, (short) value);
		}
	}

	public static final Int16 Int16 = new Int16();
	public static final class Int16 extends BinaryWriter<Short>
	{
		public void write(OutputStream out, Short value) throws IOException
		{
			writeShort(out, value);
		}

		public static void writePrimitive(OutputStream out, short value) throws IOException
		{
			writeShort(out, value);
		}
	}

	public static final Nat32 Nat32 = new Nat32();
	public static final class Nat32 extends BinaryWriter<Long>
	{
		public void write(OutputStream out, Long value) throws IOException
		{
			writeInt(out, value.intValue());
		}

		public static void writePrimitive(OutputStream out, long value) throws IOException
		{
			writeInt(out, (int) value);
		}
	}

	public static final Int32 Int32 = new Int32();
	public static final class Int32 extends BinaryWriter<Integer>
	{
		public void write(OutputStream out, Integer value) throws IOException
		{
			writeInt(out, value);
		}

		public static void writePrimitive(OutputStream out, int value) throws IOException
		{
			writeInt(out, value);
		}
	}

	public static final BinaryWriter<BigInteger> Nat64 = new BinaryWriter<BigInteger>()
	{
		public void write(OutputStream out, BigInteger value) throws IOException
		{
			writeLong(out, value.longValue());
		}
	};

	public static final Int64 Int64 = new Int64();
	public static final class Int64 extends BinaryWriter<Long>
	{
		public void write(OutputStream out, Long value) throws IOException
		{
			writeLong(out, value);
		}

		public static void writePrimitive(OutputStream out, long value) throws IOException
		{
			writeLong(out, value);
		}
	}

	public static final BinaryWriter<java.lang.String> String = new BinaryWriter<java.lang.String>()
	{
		public void write(OutputStream out, java.lang.String value) throws IOException
		{
			int charLength = value.length();

			byte[] byteOut = new byte[charLength * 3]; // We'll need (at most) 3 bytes per char.
			int byteI = 0; // output index;

			int charPairs = 0; // Counts how many double-char codepoints we encounter.
			int charI = 0;

			copyLoop: {
				char c;

				// NOTE: This first loop makes the code run faster in the ASCII-only case,
				// but I don't know why (30% less execution time on Java6+amd64).
				while (true) {
					if (charI >= charLength) break copyLoop;
					c = value.charAt(charI++);
					if (c >= 128) break;
					byteOut[byteI++] = (byte) c;
				}

				while (true) {
					if (c < (1 << 7)) {
						byteOut[byteI++] = (byte) c;
					}
					else if (c <= (1 << 14)) {
						byteOut[byteI++] = (byte) ((c >>> 8) | 0x80);
						byteOut[byteI++] = (byte) c;
					}
					else if ((c >>> 10) == (0xD800 >>> 10)) {
						// High surrogate.  This means two characters in UTF-16.
						if (charI < charLength) {
							char c2 = value.charAt(charI++);
							if ((c2 >>> 10) == (0xDC00 >>> 10)) {
								// Valid low surrogate.  Compose them to get the full code point.
								int combo = ((c & 0x3ff) << 10) | (c2 & 0x3ff);
								int codePoint = Character.MIN_SUPPLEMENTARY_CODE_POINT + combo;
								byteOut[byteI++] = (byte) (0xc0 | (codePoint >> 16));
								byteOut[byteI++] = (byte) (codePoint >> 8);
								byteOut[byteI++] = (byte) codePoint;
								charPairs++;
							}
							else {
								// Low surrogate out of range.
								throw new IllegalArgumentException("Invalid UTF-16 low surrogate at index " + (charI-1) + ": low surrogate = 0x" + Integer.toString(c2, 16));
							}
						}
						else {
							// Missing low surrogate after high surrogate.
							throw new IllegalArgumentException("Found UTF-16 high surrogate as last 'char' in String: high surrogate = 0x" + Integer.toString(c, 16));
						}
					}
					else if (c > Character.MAX_LOW_SURROGATE) {
						int codePoint = c;
						byteOut[byteI++] = (byte) (0xc0 | (codePoint >> 16));
						byteOut[byteI++] = (byte) (codePoint >> 8);
						byteOut[byteI++] = (byte) codePoint;
					}
					else {
						// Stranded low surrogate.
						throw new IllegalArgumentException("Found UTF-16 low surrogate without preceding high surrogate at index " + (charI-1) + ": low surrogate = 0x" + Integer.toString(c, 16));
					}
					if (charI >= charLength) break copyLoop;
					c = value.charAt(charI++);
				}
			}

			int numCodePoints = (charI - charPairs);

			writeLength(out, numCodePoints); // length
			out.write(byteOut, 0, byteI);
		}
	};

	public static final BinaryWriter<java.lang.Boolean> Bool = new BinaryWriter<java.lang.Boolean>()
	{
		public void write(OutputStream out, java.lang.Boolean value) throws IOException
		{
			if (value) {
				writeByte(out, 1);
			} else {
				writeByte(out, 0);
			}
		}
	};

	public static final BinaryWriter<cks.Void> Void = new BinaryWriter<cks.Void>()
	{
		public void write(OutputStream out, cks.Void value)
		{
			// Don't write anything.
		}
	};

	public static final class List<T> extends BinaryWriter<java.util.List<T>>
	{
		private final BinaryWriter<T> mT;

		public List(BinaryWriter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> List<T> mk(BinaryWriter<T> mT)
		{
			return new List<T>(mT);
		}

		public void write(OutputStream out, java.util.List<T> list)
			throws IOException
		{
			int length = list.size();
			writeLength(out, length);
			for (T elem : list) {
				mT.write(out, elem);
			}
		}
	}

	public static final class Set<T> extends BinaryWriter<java.util.Set<T>>
	{
		private final BinaryWriter<T> mT;

		public Set(BinaryWriter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Set<T> mk(BinaryWriter<T> mT)
		{
			return new Set<T>(mT);
		}

		public void write(OutputStream out, java.util.Set<T> set)
			throws IOException
		{
			int length = set.size();
			writeLength(out, length);
			for (T elem : set) {
				mT.write(out, elem);
			}
		}
	}

	public static final class Map<K,V> extends BinaryWriter<java.util.Map<K,V>>
	{
		private final BinaryWriter<K> mK;
		private final BinaryWriter<V> mV;

		public Map(BinaryWriter<K> mK, BinaryWriter<V> mV)
		{
			this.mK = mK;
			this.mV = mV;
		}

		public static <K,V> Map<K,V> mk(BinaryWriter<K> mK, BinaryWriter<V> mV)
		{
			return new Map<K,V>(mK, mV);
		}

		public void write(OutputStream out, java.util.Map<K,V> map)
			throws IOException
		{
			int length = map.size();
			writeLength(out, length);
			for (java.util.Map.Entry<K,V> entry : map.entrySet()) {
				mK.write(out, entry.getKey());
				mV.write(out, entry.getValue());
			}
		}
	}

	public static final class Maybe<T> extends BinaryWriter<cks.Maybe<T>>
	{
		private final BinaryWriter<T> mT;

		public Maybe(BinaryWriter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Maybe<T> mk(BinaryWriter<T> mT)
		{
			return new Maybe<T>(mT);
		}

		public void write(OutputStream out, cks.Maybe<T> v)
			throws IOException
		{
			if (v.isNothing()) {
				writeByte(out, 0);
			} else {
				writeByte(out, 1);
				mT.write(out, v.getJust());
			}
		}
	}

	public static final class MaybeNull<T> extends BinaryWriter<T>
	{
		private final BinaryWriter<T> mT;

		public MaybeNull(BinaryWriter<T> mT)
		{
			this.mT = mT;
		}

		public static <T> MaybeNull<T> mk(BinaryWriter<T> mT)
		{
			return new MaybeNull<T>(mT);
		}

		public void write(OutputStream out, T v)
			throws IOException
		{
			if (v == null) {
				writeByte(out, 0);
			} else {
				writeByte(out, 1);
				mT.write(out, v);
			}
		}
	}

	public static void writeLength(OutputStream out, int value)
		throws IOException
	{
		assert (value >= 0);

		if ((value >>> 7) == 0) {
			// [0] + 7 bits
			writeByte(out, value);
		}
		else if ((value >>> 14) == 0) {
			// [10] + 6 bits + 1 byte
			writeShort(out, (0x80 << 8) | value);
		}
		else if ((value >>> 21) == 0) {
			// [110] + 5 bits + 2 bytes
			writeByte(out, 0xc0 | (value >> 16));
			writeShort(out, value & 0xffff);
		}
		else if ((value >>> 28) == 0) {
			// [1110] + 4 bits + 3 bytes
			writeInt(out, (0xe << 28) | value);
		}
		else {
			// [1111] [0000] + 4 bytes
			writeByte(out, 0xf0);
			writeInt(out, value);
		}
	}

	public static void writeUnboundInt(OutputStream out, int value)
		throws IOException
	{
		int bits; // The number of bits we need to write out.
		if (value >= 0) {
			bits = 32 - Integer.numberOfLeadingZeros(value) + 1;
		} else {
			bits = 32 - Integer.numberOfLeadingZeros(~value) + 1;
		}

		if (bits <= 7) {
			// [0] + 7 bits
			writeByte(out, 0x7f & value);
		}
		else if (bits <= 14) {
			// [10] + 14 bits
			value &= 0x3fff;
			value |= 0x8000;
			writeShort(out, value);
		}
		else if (bits <= 21) {
			// [110] + 21 bits
			int highByte = value >> 16;
			highByte &= 0xdf;
			highByte |= 0xc0;
			writeByte(out, highByte);
			writeShort(out, value & 0xffff);
		}
		else if ((value >>> 28) == 0) {
			// [1110] + 4 bits + 3 bytes
			value &= 0xefffffff;
			value |= 0xe0000000;
			writeInt(out, value);
		}
		else {
			// [1111] [0000] + 4 bytes
			writeByte(out, 0xf0);
			writeInt(out, value);
		}
	}

	public static void writeOptionIndex1(OutputStream out, int index)
		throws IOException
	{
		writeByte(out, index);
	}

	public static void writeOptionIndex2(OutputStream out, int index)
		throws IOException
	{
		writeShort(out, index);
	}

	public static void writeOptionIndex3(OutputStream out, int index)
		throws IOException
	{
		writeByte(out, index >> 16);
		writeShort(out, index);
	}

	public static void writeOptionIndex4(OutputStream out, int index)
		throws IOException
	{
		writeInt(out, index);
	}

	public static void writeUnboundNat(OutputStream out, BigInteger value)
		throws IOException
	{
		if (value.signum() == -1) throw new IllegalArgumentException("'value' cannot be negative");

		int bits = value.bitLength();

		if (bits <= 32) {
			writeLength(out, value.intValue());
		}
		else {
			// [1111] [nnnn] + (n+4) bytes
			int bytes = (bits+7) / 8;
			if (bytes <= 18) {
				// Short header: [1111] [(length - 4) in 4 bits]
				int header = 0xf0 | (bytes - 4);
				writeByte(out, header);
			}
			else {
				// Long header: [1111] [1111] [(length-19) as unbound nat]
				writeByte(out, 0xff);
				writeLength(out, bytes - 19);
			}
			byte[] data = value.toByteArray(); // Includes a leading zero bit (sign bit), which we don't need.
			int start = 0;
			assert data.length == bytes || data.length == (bytes+1) : "data.length = " + data.length + ", bytes = " + bytes;
			if (data.length > bytes) {
				start++; // Ignore the first data byte if it's just there to hold the sign bit.
			}
			out.write(data, start, bytes);
		}
	}

	public static void writeUnboundInt(OutputStream out, BigInteger value)
		throws IOException
	{
		int bits = value.bitLength();

		if (bits <= 31) {
			writeUnboundInt(out, value.intValue());
		}
		else {
			int bytes = (bits+7) / 8;
			if (bytes <= 18) {
				// Short header: [111] [(length - 4) in 4 bits]
				int header = 0xf0 | (bytes - 4);
				writeByte(out, header);
			}
			else {
				// Long header: [1111] [1111] [(length-19) as unbound nat]
				writeByte(out, 0xff);
				writeLength(out, bytes - 19);
			}
			byte[] data = value.toByteArray(); // Includes a leading zero bit (sign bit)
			assert data.length == bytes;
			out.write(data, 0, bytes);
		}
	}
}

