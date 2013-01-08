package cks.io;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.math.BigInteger;

public abstract class BinaryReader<T>
{
	// Thrown if the input is invalid or if this implementation can't handle
	// the input for whatever reason.
	public static final class FormatException extends Exception
	{
		public FormatException(java.lang.String message)
		{
			super(message);
		}
	}

	// -------------------------------------------------
	// The only method in the interface.

	/**
	 * Do not call this method yourself.  For internal use only.
	 */
	public abstract T readImpl(InputStream in) throws IOException, FormatException;

	// -------------------------------------------------
	// Convenience functions for clients.

	/**
	 * Reads a single CKS value from the stream.
	 *
	 * If there is more data in the stream after the value has been read, you'll
	 * get a FormatException.  If you do not want this, use readPartial(...).
	 *
	 * @param in
	 *    Recommended: use a buffered stream.
	 */
	public final T read(InputStream in) throws IOException, FormatException
	{
		T value = readImpl(in);
		if (in.read() != -1) {
			throw new FormatException("encountered extra data (after the value)");
		}
		return value;
	}

	/**
	 * Same as 'read' except it's OK for there to be left over data in the stream
	 * after the value is read.
	 *
	 * @param in
	 *    Recommended: use a buffered stream.
	 */
	public final T readPartial(InputStream in) throws IOException, FormatException
	{
		return readImpl(in);
	}

	public final T readFromByteArray(byte[] input) throws FormatException
	{
		// TODO: optimize for byte array (we know how long it is, so we can do fewer EOF checks)
		try {
			return read(new ByteArrayInputStream(input));
		}
		catch (IOException ex) {
			throw new RuntimeException("IOException from ByteArrayInputStream.  This should not happen.", ex);
		}
	}

	private static FormatException eof() {
		return new FormatException("unexpected end of input");
	}

	// -------------------------------------------------
	// Helper functions for implementors.

	public static int readUnsignedByte(InputStream in) throws IOException, FormatException
	{
		int c = in.read();
		if (c < 0) throw eof();
		return c;
	}

	public static byte readByte(InputStream in) throws IOException, FormatException
	{
		int c = in.read();
		if (c < 0) throw eof();
		return (byte) c;
	}

	public static short readShort(InputStream in) throws IOException, FormatException
	{
		int c1 = in.read();
		int c2 = in.read();
		if ((c1 | c2) < 0) throw eof();
		return (short) ((c1 << 8) + (c2 << 0));
	}

	public static int readUnsignedShort(InputStream in) throws IOException, FormatException
	{
		int c1 = in.read();
		int c2 = in.read();
		if ((c1 | c2) < 0) throw eof();
		return (c1 << 8) + (c2 << 0);
	}

	public static int readInt(InputStream in) throws IOException, FormatException
	{
		int c1 = in.read();
		int c2 = in.read();
		int c3 = in.read();
		int c4 = in.read();
		if ((c1 | c2 | c3 | c4) < 0) throw eof();
		return ((c1 << 24) + (c2 << 16) + (c3 << 8) + (c4 << 0));
	}

	public static long readLong(InputStream in) throws IOException, FormatException
	{
		int i1 = readInt(in);
		int i2 = readInt(in);
		return (((long) i1) << 32) | ((long) i2 & 0xffffffffL);
	}

	public static void readFully(InputStream in, byte buf[], int offset, int len) throws IOException, FormatException
	{
		if (len < 0) throw new IndexOutOfBoundsException();

		int end = offset + len;
		int pos = offset;
		while (pos < end) {
			int numRead = in.read(buf, pos, (end - pos));
			if (numRead < 0) throw eof();
			pos += numRead;
		}
	}

	public static void readFully(InputStream in, byte buf[]) throws IOException, FormatException
	{
		readFully(in, buf, 0, buf.length);
	}

	// -------------------------------------------------
	// Implementations for the standard types.

	public static final BinaryReader<BigInteger> Nat = new BinaryReader<BigInteger>()
	{
		public BigInteger readImpl(InputStream in) throws IOException, FormatException
		{
			return readUnboundNat(in);
		}
	};

	public static final BinaryReader<BigInteger> Int = new BinaryReader<BigInteger>()
	{
		public BigInteger readImpl(InputStream in) throws IOException, FormatException
		{
			return readUnboundInt(in);
		}
	};

	public static final Nat8 Nat8 = new Nat8();
	public static final class Nat8 extends BinaryReader<Short>
	{
		public Short readImpl(InputStream in) throws IOException, FormatException
		{
			return readPrimitive(in);
		}
		public static short readPrimitive(InputStream in) throws IOException, FormatException
		{
			return (short) readUnsignedByte(in);
		}
	}

	public static final Int8 Int8 = new Int8();
	public static final class Int8 extends BinaryReader<Byte>
	{
		public Byte readImpl(InputStream in) throws IOException, FormatException
		{
			return readPrimitive(in);
		}
		public static byte readPrimitive(InputStream in) throws IOException, FormatException
		{
			return readByte(in);
		}
	}

	public static final Nat16 Nat16 = new Nat16();
	public static final class Nat16 extends BinaryReader<Integer>
	{
		public Integer readImpl(InputStream in) throws IOException, FormatException
		{
			return readPrimitive(in);
		}
		public static int readPrimitive(InputStream in) throws IOException, FormatException
		{
			return readUnsignedShort(in);
		}
	}

	public static final Int16 Int16 = new Int16();
	public static final class Int16 extends BinaryReader<Short>
	{
		public Short readImpl(InputStream in) throws IOException, FormatException
		{
			return readPrimitive(in);
		}
		public static short readPrimitive(InputStream in) throws IOException, FormatException
		{
			return readShort(in);
		}
	}

	public static final Nat32 Nat32 = new Nat32();
	public static final class Nat32 extends BinaryReader<Long>
	{
		public Long readImpl(InputStream in) throws IOException, FormatException
		{
			return readPrimitive(in);
		}
		public static long readPrimitive(InputStream in) throws IOException, FormatException
		{
			int i = readInt(in);
			return ((long)i & 0xffffffffL);
		}
	}

	public static final Int32 Int32 = new Int32();
	public static final class Int32 extends BinaryReader<Integer>
	{
		public Integer readImpl(InputStream in) throws IOException, FormatException
		{
			return readPrimitive(in);
		}
		public static int readPrimitive(InputStream in) throws IOException, FormatException
		{
			return readInt(in);
		}
	}

	public static final BinaryReader<BigInteger> Nat64 = new BinaryReader<BigInteger>()
	{
		public BigInteger readImpl(InputStream in) throws IOException, FormatException
		{
			byte[] data = new byte[8];
			readFully(in, data);
			return new BigInteger(1, data);
			// Passing '1' as the signum seems to work even if 'data' is all zeros.
		}
	};

	public static final Int64 Int64 = new Int64();
	public static final class Int64 extends BinaryReader<Long>
	{
		public Long readImpl(InputStream in) throws IOException, FormatException
		{
			return readPrimitive(in);
		}
		public static long readPrimitive(InputStream in) throws IOException, FormatException
		{
			return readLong(in);
		}
	}

	public static final BinaryReader<java.lang.String> String = new BinaryReader<java.lang.String>()
	{
		public java.lang.String readImpl(InputStream in) throws IOException, FormatException
		{
			int codePointLength = readLength(in);
			// TODO: Don't initially allocate all the required memory (in case we just read some
			// random bits.  Allocate up to a certain amount, then allocate more as we go along.

			int charI = 0;
			char[] charBuf = new char[codePointLength * 2];

			int byteI = 0;
			byte[] byteBuf = new byte[codePointLength * 2];
				// We start out only using 'codePointLength', but if everything in the
				// body is 3-byte code points, we might refill it to 'codePointLength * 2'.

			readFully(in, byteBuf, 0, codePointLength);
			int byteEnd = codePointLength;
			int byteMore = 0;

			while (byteI < byteEnd) {
				int b = byteBuf[byteI] & 0xff;
				if (b >= 128) break;
				byteI++;
				charBuf[charI++] = (char) b;
			}

			while (true) {
				if (byteI >= byteEnd) {
					if (byteMore == 0) break;
					// refill
					readFully(in, byteBuf, 0, byteMore);
					byteI = 0;
					byteEnd = byteMore;
					byteMore = 0;
				}

				int b = byteBuf[byteI++] & 0xff;

				if (b < 128) {
					charBuf[charI++] = (char) b;
				}
				else if ((b & 0x40) == 0) {
					byteMore++;
					if (byteI >= byteEnd) {
						assert byteMore >= 0;
						// refill
						readFully(in, byteBuf, 0, byteMore);
						byteI = 0;
						byteEnd = byteMore;
						byteMore = 0;
					}
					int next = byteBuf[byteI++] & 0xff;
					int value = ((b & 0x3f) << 8) | next;
					charBuf[charI++] = (char) value;
					// We can't reach the surrogate range with 14 bits, so no need to check.
				}
				else {
					byteMore += 2;
					int next1;

					if (byteI < byteEnd) {
						next1 = byteBuf[byteI++] & 0xff;
						if (byteI >= byteEnd) {
							assert byteMore > 0;
							// refill
							readFully(in, byteBuf, 0, byteMore);
							byteI = 0;
							byteEnd = byteMore;
							byteMore = 0;
						}
					}
					else {
						assert byteMore > 0;
						// refill
						readFully(in, byteBuf, 0, byteMore);
						byteI = 0;
						byteEnd = byteMore;
						byteMore = 0;
						next1 = byteBuf[byteI++] & 0xff;
					}

					int next2 = byteBuf[byteI++] & 0xff;
					int value = ((b & 0x3f) << 16) | ((next1 & 0xff) << 8) | (next2 & 0xff);

					if (value > 0x10ffff) throw new FormatException("character value out of range: " + value);

					if (value >= 0x010000) {
						charBuf[charI++] = (char) (0xD8 | (value >>> 10));  // high 10 bits
						charBuf[charI++] = (char) (0xDC | (value & 0x3fff)); // low 10 bits
					}
					else if (value > Character.MAX_SURROGATE || value < Character.MIN_SURROGATE) {
						charBuf[charI++] = (char) value;
					}
					else {
						throw new FormatException("character value out of range: " + value + " (characters in the UTF-16 surrogate range are not allowed.");
					}
				}
			}

			return new String(charBuf, 0, charI);
		}
	};

	public static int readOptionIndex1(InputStream in)
		throws IOException, FormatException
	{
		return readUnsignedByte(in);
	}

	public static int readOptionIndex2(InputStream in)
		throws IOException, FormatException
	{
		return readUnsignedShort(in);
	}

	public static int readOptionIndex3(InputStream in)
		throws IOException, FormatException
	{
		return readUnsignedByte(in);
	}

	public static int readOptionIndex4(InputStream in)
		throws IOException, FormatException
	{
		int i = readInt(in);
		if (i < 0) throw new FormatException("Option index out of range: " + ((long)i & 0xffffffffL));
		return i;
	}

	public static int readLength(InputStream in)
		throws IOException, FormatException
	{
		int b = readUnsignedByte(in);
		if ((b & 0x80) == 0) {
			// "0" [7 bits]
			return b;
		}
		else if ((b & 0x40) == 0) {
			// "10" [14 bits]
			int next = readUnsignedByte(in);
			int value = ((b & 0x3f) << 8) | next;
			return value;
		}
		else if ((b & 0x20) == 0) {
			// "110" [5 bits] + 2 bytes
			int next = readUnsignedShort(in);
			int value = ((b & 0x1f) << 16) | next;
			return value;
		}
		else if ((b & 0x10) == 0) {
			// "1110" [4 bits] + 3 bytes
			int next1 = readUnsignedByte(in);
			int next2 = readUnsignedShort(in);
			int value = ((b & 0x0f) << 24) | (next1 << 16) | next2;
			return value;
		}
		else {
			b &= 0x0f;
			int value = readInt(in);
			if (b == 0 && value >= 0) { // 4 bytes
				return value;
			} else {
				throw new FormatException("data length value too large");
			}
		}
	}

	public static BigInteger readUnboundNat(InputStream in)
		throws IOException, FormatException
	{
		int b = readUnsignedByte(in);
		if ((b & 0x80) == 0) {
			// "0" [7 bits]
			return BigInteger.valueOf(b);
		}
		else if ((b & 0x40) == 0) {
			// "10" [14 bits]
			int next = readUnsignedByte(in);
			int value = ((b & 0x3f) << 8) | next;
			return BigInteger.valueOf(value);
		}
		else if ((b & 0x20) == 0) {
			// "110" [21 bits]
			int next = readUnsignedShort(in);
			int value = ((b & 0x1f) << 16) | next;
			return BigInteger.valueOf(value);
		}
		else if ((b & 0x10) == 0) {
			// "1110" [4 bits] + 3 bytes
			int next1 = readUnsignedByte(in);
			int next2 = readUnsignedShort(in);
			int value = ((b & 0x0f) << 24) | (next1 << 16) | next2;
			return BigInteger.valueOf(value);
		}
		else {
			int head = b & 0x0f;
			int length;
			if (head != 0x0f) {
				length = head + 4; // Short header
			} else {
				// Length is actually encoded as another unbound nat.
				length = readLength(in);
				length += 19;
				if (length < 0) {
					throw new FormatException("reading unbounded natural number: data length value too large: " + length);
				}
			}

			try {
				byte[] data = new byte[length];
				readFully(in, data);
				return new BigInteger(1, data);
			} catch (OutOfMemoryError ex) {
				// Not sure if it's a good idea to be catching these exceptions...
				throw new FormatException("reading a " + length + "-byte unbounded natural number: ran out of memory");
			}
		}
	}

	public static BigInteger readUnboundInt(InputStream in)
		throws IOException, FormatException
	{
		int b = readUnsignedByte(in);
		if ((b & 0x80) == 0) {
			// [0] [7 bits]

			// fix high bits (sign extend)
			if ((b & 0x40) != 0) {
				b |= 0x80;
			}

			return new BigInteger(new byte[]{(byte) b});
		}
		else if ((b & 0x40) == 0) {
			// [10] [14 bits]

			// fix high bits (sign extend)
			if ((b & 0x20) != 0) {
				b |= 0x40;
			} else {
				b &= 0x3f;
			}

			int next = readUnsignedByte(in);
			return new BigInteger(new byte[]{(byte) b, (byte) next});
		}
		else if ((b & 0x20) == 0) {
			// [110] [21 bits]

			// fix high bits (sign extend)
			if ((b & 0x10) != 0) {
				b |= 0x20;
			} else {
				b &= 0x1f;
			}

			int n1 = readUnsignedByte(in);
			int n2 = readUnsignedByte(in);
			return new BigInteger(new byte[]{(byte) b, (byte) n1, (byte) n2});
		}
		else if ((b & 0x10) == 0) {
			// [1110] [4 bits] + 3 bytes

			// fix high bits (sign extend)
			if ((b & 0x08) != 0) {
				b |= 0x10;
			} else {
				b &= 0x0f;
			}

			int n1 = readUnsignedByte(in);
			int n2 = readUnsignedByte(in);
			int n3 = readUnsignedByte(in);
			return new BigInteger(new byte[]{(byte) b, (byte) n1, (byte) n2, (byte) n3});
		}
		else {
			int head = b & 0x0f;
			int length;
			if (head != 0x0f) {
				length = head + 4; // Short header
			} else {
				// Length is actually encoded as another unbound nat.
				length = readLength(in);
				if (length < 0) {
					throw new FormatException("reading unbounded natural number: data length value too large: " + length);
				}
			}

			try {
				byte[] data = new byte[length];
				readFully(in, data);
				return new BigInteger(data);
			} catch (OutOfMemoryError ex) {
				throw new FormatException("reading a " + length + "-byte unbounded natural number: ran out of memory");
			}
		}
	}

	public static final BinaryReader<java.lang.Boolean> Bool = new BinaryReader<java.lang.Boolean>()
	{
		public java.lang.Boolean readImpl(InputStream in) throws IOException, FormatException
		{
			int i = readUnsignedByte(in);
			if (i == 1) {
				return java.lang.Boolean.TRUE;
			} else if (i == 0) {
				return java.lang.Boolean.FALSE;
			} else {
				throw new FormatException("reading boolean: expecting 0 or 1, found " + i);
			}
		}
	};

	public static final BinaryReader<cks.Void> Void = new BinaryReader<cks.Void>()
	{
		public cks.Void readImpl(InputStream in)
		{
			return cks.Void.Instance;
		}
	};

	public static final class List<T> extends BinaryReader<java.util.List<T>>
	{
		private final BinaryReader<T> mT;

		public List(BinaryReader<T> mT)
		{
			this.mT = mT;
		}

		public static <T> List<T> mk(BinaryReader<T> mT)
		{
			return new List<T>(mT);
		}

		public java.util.List<T> readImpl(InputStream in) throws IOException, FormatException
		{
			int length = readLength(in);

			java.util.ArrayList<T> list = new java.util.ArrayList<T>(Math.min(length, 1000));

			for (int i = 0; i < length; i++) {
				list.add(mT.readImpl(in));
			}

			return list;
		}
	}

	public static final class Set<T> extends BinaryReader<java.util.Set<T>>
	{
		private final BinaryReader<T> mT;

		public Set(BinaryReader<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Set<T> mk(BinaryReader<T> mT)
		{
			return new Set<T>(mT);
		}

		public java.util.Set<T> readImpl(InputStream in) throws IOException, FormatException
		{
			int length = readLength(in);

			java.util.LinkedHashSet<T> set = new java.util.LinkedHashSet<T>();

			for (int i = 0; i < length; i++) {
				boolean added = set.add(mT.readImpl(in));
				if (!added) {
					throw new FormatException("reading set: duplicate entry");
				}
			}

			return set;
		}
	}

	public static final class Map<K,V> extends BinaryReader<java.util.Map<K,V>>
	{
		private final BinaryReader<K> mK;
		private final BinaryReader<V> mV;

		public Map(BinaryReader<K> mK, BinaryReader<V> mV)
		{
			this.mK = mK;
			this.mV = mV;
		}

		public static <K,V> Map<K,V> mk(BinaryReader<K> mK, BinaryReader<V> mV)
		{
			return new Map<K,V>(mK, mV);
		}

		public java.util.Map<K,V> readImpl(InputStream in) throws IOException, FormatException
		{
			BigInteger l = readUnboundNat(in);
			if (l.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0) {
				throw new FormatException("reading map: length value too large: " + l);
			}

			int length = l.intValue();

			java.util.LinkedHashMap<K,V> map = new java.util.LinkedHashMap<K,V>();

			for (int i = 0; i < length; i++) {
				Object displaced = map.put(mK.readImpl(in), mV.readImpl(in));
				if (displaced != null) {
					throw new FormatException("reading map: duplicate key");
				}
			}

			return map;
		}
	}

	public static final class Maybe<T> extends BinaryReader<cks.Maybe<T>>
	{
		private final BinaryReader<T> mT;

		public Maybe(BinaryReader<T> mT)
		{
			this.mT = mT;
		}

		public static <T> Maybe<T> mk(BinaryReader<T> mT)
		{
			return new Maybe<T>(mT);
		}

		public cks.Maybe<T> readImpl(InputStream in) throws IOException, FormatException
		{
			int i = readUnsignedByte(in);
			if (i == 0) {
				return cks.Maybe.Nothing();
			} else if (i == 1) {
				return cks.Maybe.Just(mT.readImpl(in));
			} else {
				throw new FormatException("reading optional: expecting 0 or 1, found " + i);
			}
		}
	}

	public static final class MaybeNull<T> extends BinaryReader<T>
	{
		private final BinaryReader<T> mT;

		public MaybeNull(BinaryReader<T> mT)
		{
			this.mT = mT;
		}

		public static <T> MaybeNull<T> mk(BinaryReader<T> mT)
		{
			return new MaybeNull<T>(mT);
		}

		public T readImpl(InputStream in) throws IOException, FormatException
		{
			int i = readUnsignedByte(in);
			if (i == 0) {
				return null;
			} else if (i == 1) {
				return mT.readImpl(in);
			} else {
				throw new FormatException("reading optional: expecting 0 or 1, found " + i);
			}
		}

	}
}
