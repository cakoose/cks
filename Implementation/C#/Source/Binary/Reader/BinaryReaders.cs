using C = Cks.Data;
using IO = System.IO;
using StringBuilder = System.Text.StringBuilder;
using BigInteger = Cks.Data.BigInteger;
using BigNatural = Mono.Math.BigInteger;
using Debug = System.Diagnostics.Debug;
using ApplicationException = System.ApplicationException;

namespace Cks.Binary.Reader
{

public static class BinaryReaders
{
	public static readonly BinaryReader<bool> Bool = new _Bool();
	private sealed class _Bool : BinaryReader<bool>
	{
		public override bool ReadImpl(IO.BinaryReader In)
		{
			byte b = In.ReadByte();
			if (b == 1) {
				return true;
			} else if (b == 0) {
				return false;
			} else {
				throw new BinaryFormatException("reading boolean: expecting 0 or 1, found " + b);
			}
		}
	}

	public sealed class Maybe<T> : BinaryReader<C.Maybe<T>>
	{
		public readonly BinaryReader<T> mT;

		public Maybe(BinaryReader<T> mT)
		{
			this.mT = mT;
		}

		public override C.Maybe<T> ReadImpl(IO.BinaryReader In)
		{
			byte i = In.ReadByte();
			if (i == 0) {
				return new C.Maybe<T>.Nothing();
			} else if (i == 1) {
				return new C.Maybe<T>.Just(mT.ReadImpl(In));
			} else {
				throw new BinaryFormatException("reading optional: expecting 0 or 1, found " + i);
			}
		}
	}

	public static readonly BinaryReader<string> String = new _String();
	private sealed class _String : BinaryReader<string>
	{
		public override string ReadImpl(IO.BinaryReader In)
		{
			int CodePointLength = ReadLength(In);

			int CharI = 0;
			char[] CharBuf = new char[CodePointLength * 2];

			int ByteI = 0;
			byte[] ByteBuf = new byte[CodePointLength * 2];
				// We start out only using 'CodePointLength' bytes of this array.
				// But if everything we read is 3-byte code points, we might refill
				// it to 'CodePointLength * 2'.

			ReadFully(In, ByteBuf, 0, CodePointLength);
			int ByteEnd = CodePointLength;
			int ByteMore = 0;

			while (ByteI < ByteEnd) {
				int b = ByteBuf[ByteI] & 0xff;
				if (b >= 128) break;
				ByteI++;
				CharBuf[CharI++] = (char) b;
			}

			while (true) {
				if (ByteI >= ByteEnd) {
					if (ByteMore == 0) break;
					// refill
					ReadFully(In, ByteBuf, 0, ByteMore);
					ByteI = 0;
					ByteEnd = ByteMore;
					ByteMore = 0;
				}

				int b = ByteBuf[ByteI++] & 0xff;
				
				if (b < 128) {
					CharBuf[CharI++] = (char) b;
				}
				else if ((b & 0x40) == 0) {
					ByteMore++;
					if (ByteI >= ByteEnd) {
						Debug.Assert(ByteMore >= 0);
						// refill
						ReadFully(In, ByteBuf, 0, ByteMore);
						ByteI = 0;
						ByteEnd = ByteMore;
						ByteMore = 0;
					}
					int Next = ByteBuf[ByteI++] & 0xff;
					int Value = ((b & 0x3f) << 8) | Next;
					CharBuf[CharI++] = (char) Value;
					// We can't reach the surrogate range with 14 bits, so no need to check.
				}
				else {
					ByteMore += 2;
					int Next1;

					if (ByteI < ByteEnd) {
						Next1 = ByteBuf[ByteI++] & 0xff;
						if (ByteI >= ByteEnd) {
							Debug.Assert(ByteMore > 0);
							// refill
							ReadFully(In, ByteBuf, 0, ByteMore);
							ByteI = 0;
							ByteEnd = ByteMore;
							ByteMore = 0;
						}
					}
					else {
						Debug.Assert(ByteMore > 0);
						// refill
						ReadFully(In, ByteBuf, 0, ByteMore);
						ByteI = 0;
						ByteEnd = ByteMore;
						ByteMore = 0;
						Next1 = ByteBuf[ByteI++] & 0xff;
					}

					int Next2 = ByteBuf[ByteI++] & 0xff;
					int Value = ((b & 0x3f) << 16) | ((Next1 & 0xff) << 8) | (Next2 & 0xff);

					if (Value > 0x10ffff) throw new BinaryFormatException("character value out of range: " + Value);

					if (Value >= 0x10000) {
						CharBuf[CharI++] = (char) (0xD8 | (Value >> 10)); // high 10 bits
						CharBuf[CharI++] = (char) (0xDC | (Value & 0x3fff)); // low 10 bits
					}
					else if (Value > 0xDFFF || Value < 0xD800) {
						CharBuf[CharI++] = (char) Value;
					}
					else {
						throw new BinaryFormatException("character value out of range: " + Value + " (characters in the UTF-16 surrogate range are not allowed.");
					}
				}
			}

			return new string(CharBuf, 0, CharI);
		}
	}

	private static void ReadFully(IO.BinaryReader In, byte[] Data, int Offset, int Length)
	{
		int BytesReadImpl = In.Read(Data, Offset, Length);
		if (BytesReadImpl < Length) {
			throw new BinaryFormatException("unexpected end of file");
		}
	}

	public sealed class List<T> : BinaryReader<C.List<T>>
	{
		public readonly BinaryReader<T> mT;

		public List(BinaryReader<T> mT)
		{
			this.mT = mT;
		}

		public override C.List<T> ReadImpl(IO.BinaryReader In)
		{
			int Length = ReadLength(In);

			C.List<T> List = new C.List<T>(); // TODO: preallocate list capacity if 'length' is less than, say, 1000
			for (int i = 0; i < Length; i++) {
				List.Add(mT.ReadImpl(In));
			}

			return List;
		}
	}

	public sealed class Set<T> : BinaryReader<C.HashSet<T>>
	{
		public readonly BinaryReader<T> mT;

		public Set(BinaryReader<T> mT)
		{
			this.mT = mT;
		}

		public override C.HashSet<T> ReadImpl(IO.BinaryReader In)
		{
			int Length = ReadLength(In);

			C.HashSet<T> s = new C.HashSet<T>();
			for (int i = 0; i < Length; i++) {
				T e = mT.ReadImpl(In);
				if (s.Contains(e)) {
					throw new BinaryFormatException("reading set: duplicate entry");
				}
				s.Add(e);
			}

			return s;
		}
	}

	public sealed class Map<K,V> : BinaryReader<C.Dictionary<K,V>>
	{
		public readonly BinaryReader<K> mK;
		public readonly BinaryReader<V> mV;

		public Map(BinaryReader<K> mK, BinaryReader<V> mV)
		{
			this.mK = mK;
			this.mV = mV;
		}

		public override C.Dictionary<K,V> ReadImpl(IO.BinaryReader In)
		{
			int Length = ReadLength(In);

			C.Dictionary<K,V> m = new C.Dictionary<K,V>();
			for (int i = 0; i < Length; i++) {
				K k = mK.ReadImpl(In);
				if (m.ContainsKey(k)) {
					throw new BinaryFormatException("reading set: duplicate entry");
				}
				V v = mV.ReadImpl(In);
				m.Add(k, v);
			}

			return m;
		}
	}


	public static readonly BinaryReader<BigInteger> Int = new _Int();
	public sealed class _Int : BinaryReader<BigInteger>
	{
		public override BigInteger ReadImpl(IO.BinaryReader In)
		{
			return ReadUnboundInt(In);
		}
	}

	public static readonly BinaryReader<BigInteger> Nat = new _Nat();
	public sealed class _Nat : BinaryReader<BigInteger>
	{
		public override BigInteger ReadImpl(IO.BinaryReader In)
		{
			BigNatural n = ReadUnboundNat(In);
			return new BigInteger(true, n);
		}
	}

	// ------------------------------------------------------

	public static readonly BinaryReader<ulong> Nat64 = new _Nat64();
	private class _Nat64 : BinaryReader<ulong>
	{
		public override ulong ReadImpl(IO.BinaryReader In)
		{
			ulong v = In.ReadByte();
			for (int j = 1; j < 8; j++) {
				v <<= 8;
				v |= In.ReadByte();
			}
			return v;
		}
	}

	public static readonly BinaryReader<uint> Nat32 = new _Nat32();
	private class _Nat32 : BinaryReader<uint>
	{
		public override uint ReadImpl(IO.BinaryReader In)
		{
			uint v = In.ReadByte();
			for (int j = 1; j < 4; j++) {
				v <<= 8;
				v |= In.ReadByte();
			}
			return v;
		}
	}

	public static readonly BinaryReader<ushort> Nat16 = new _Nat16();
	private class _Nat16 : BinaryReader<ushort>
	{
		public override ushort ReadImpl(IO.BinaryReader In)
		{
			ushort v = In.ReadByte();
			v <<= 8;
			v |= In.ReadByte();
			return v;
		}
	}

	public static readonly BinaryReader<byte> Nat8 = new _Nat8();
	private class _Nat8 : BinaryReader<byte>
	{
		public override byte ReadImpl(IO.BinaryReader In)
		{
			return In.ReadByte();
		}
	}

	// ------------------------------------------------------

	public static readonly BinaryReader<long> Int64 = new _Int64();
	private class _Int64 : BinaryReader<long>
	{
		public override long ReadImpl(IO.BinaryReader In)
		{
			long v = In.ReadByte();
			for (int j = 1; j < 8; j++) {
				v <<= 8;
				v |= In.ReadByte();
			}
			return v;
		}
	}

	public static readonly BinaryReader<int> Int32 = new _Int32();
	private class _Int32 : BinaryReader<int>
	{
		public override int ReadImpl(IO.BinaryReader In)
		{
			int v = In.ReadByte();
			for (int j = 1; j < 4; j++) {
				v <<= 8;
				v |= In.ReadByte();
			}
			return v;
		}
	}

	public static readonly BinaryReader<short> Int16 = new _Int16();
	private class _Int16 : BinaryReader<short>
	{
		public override short ReadImpl(IO.BinaryReader In)
		{
			short v = In.ReadByte();
			v <<= 8;
			v |= In.ReadByte();
			return v;
		}
	}

	public static readonly BinaryReader<sbyte> Int8 = new _Int8();
	private class _Int8 : BinaryReader<sbyte>
	{
		public override sbyte ReadImpl(IO.BinaryReader In)
		{
			sbyte v = (sbyte) In.ReadByte();
			return v;
		}
	}

	// ------------------------------------------------------

	public static int ReadLength(IO.BinaryReader In)
	{
		byte b = In.ReadByte();
		if ((b & 0x80) == 0) {
			// "0" [7 bits]
			return b;
		}
		else if ((b & 0x40) == 0) {
			// "10" [6 bits] + 1 byte
			byte Next = In.ReadByte();
			int Value = ((b & 0x3f) << 8 | Next);
			return Value;
		}
		else if ((b & 0x20) == 0) {
			// "110" [5 bits] + 2 bytes
			byte Next1 = In.ReadByte();
			byte Next2 = In.ReadByte();
			int Value = (b & 0x1f) << 16 | Next1 << 8 | Next2;
			return Value;
		}
		else if ((b & 0x10) == 0) {
			// "1110" [4 bits] + 3 bytes
			byte Next1 = In.ReadByte();
			byte Next2 = In.ReadByte();
			byte Next3 = In.ReadByte();
			int Value = (b & 0x1f) << 24 | Next1 << 16 | Next2 << 8 | Next3;
			return Value;
		}
		else {
			// 4 bytes
			b &= 0x0f;
			int Value = Int32.ReadImpl(In);
			if (b == 0 && Value >= 0) {
				return Value;
			} else {
				throw new BinaryFormatException("data length value too large");
			}
		}
	}
	
	public static BigNatural ReadUnboundNat(IO.BinaryReader In)
	{
		byte b = In.ReadByte();
		if ((b & 0x80) == 0) {
			// "0" [7 bits]
			return new BigNatural(b);
		}
		else if ((b & 0x40) == 0) {
			// "10" [6 bits] + 1 byte
			byte Next = In.ReadByte();
			ushort Value = (ushort) ((b & 0x3f) << 8 | Next);
			return new BigNatural(Value);
		}
		else if ((b & 0x20) == 0) {
			// "110" [5 bits] + 2 bytes
			byte Next1 = In.ReadByte();
			byte Next2 = In.ReadByte();
			uint Value = (uint) (b & 0x1f) << 16 | (uint) Next1 << 8 | (uint) Next2;
			return new BigNatural(Value);
		}
		else if ((b & 0x10) == 0) {
			// "1110" [4 bits] + 3 bytes
			byte Next1 = In.ReadByte();
			byte Next2 = In.ReadByte();
			byte Next3 = In.ReadByte();
			uint Value = (uint) (b & 0x1f) << 24 | (uint) Next1 << 16 | (uint) Next2 << 8 | (uint) Next3;
			return new BigNatural(Value);
		}
		else {
			byte Head = (byte) (b & 0x0f);
			int Length;
			if (Head != 0x0f) {
				Length = Head + 4; // Short header
			}
			else {
				// Length is actually encoded as another unbound nat.
				Length = ReadLength(In) + 19; // Add in bias.

				if (Length < 0) {
					// We wrapped.
					throw new BinaryFormatException("reading unbound natural number: data length value too large: " + (uint) Length);
				}
			}

			byte[] Data = In.ReadBytes(Length);
			if (Data.Length < Length) {
				throw new BinaryFormatException("reading string: trying to read " + Length + " bytes; encountered EOF after " + Data.Length + " bytes");
			}
			Debug.Assert(Data.Length == Length);

			return new BigNatural(Data);
		}
	}

	public static BigInteger ReadUnboundInt(IO.BinaryReader In)
	{
		byte b = In.ReadByte();
		if ((b & 0x80) == 0) {
			// "0" [7 bits]
			bool IsPositive = true;
			if ((b & (1 << 6)) != 0) {
				// Negative (2s complement)
				IsPositive = false;
				b = (byte) (~b + 1); // Get positive equivalent.
				b &= (1 << 7) - 1; // Mask out overflow bit.
			}
			return new BigInteger(IsPositive, new BigNatural(b));
		}
		else if ((b & 0x40) == 0) {
			// "10" [14 bits]
			byte Next = In.ReadByte();
			ushort Value = (ushort) ((b & 0x3f) << 8 | Next);
			bool IsPositive = true;
			if ((Value & (1 << 13)) != 0) {
				// Negative (2s complement)
				IsPositive = false;
				Value = (ushort) (~Value + 1); // Get positive equivalent.
				Value &= (1 << 14) - 1; // Mask out overflow bit.
			}
			return new BigInteger(IsPositive, new BigNatural(Value));
		}
		else if ((b & 0x20) == 0) {
			// "110" [21 bits]
			byte Next1 = In.ReadByte();
			byte Next2 = In.ReadByte();
			uint Value = (uint) ((b & 0x1f) << 16 | Next1 << 8 | Next2);
			bool IsPositive = true;
			if ((Value & (1 << 20)) != 0) {
				// Negative (2s complement)
				IsPositive = false;
				Value = ~Value + 1; // Get positive equivalent.
				Value &= (1 << 21) - 1; // Mask out overflow bit.
			}
			return new BigInteger(IsPositive, new BigNatural(Value));
		}
		else if ((b & 0x10) == 0) {
			// "1110" [4 bits] + 3 bytes
			byte Next1 = In.ReadByte();
			byte Next2 = In.ReadByte();
			byte Next3 = In.ReadByte();
			uint Value = (uint) ((b & 0x1f) << 24 | Next1 << 16 | Next2 << 8 | Next3);
			bool IsPositive = true;
			if ((Value & (1 << 27)) != 0) {
				// Negative (2s complement)
				IsPositive = false;
				Value = ~Value + 1; // Get positive equivalent.
				Value &= (1 << 28) - 1; // Mask out overflow bit.
			}
			return new BigInteger(IsPositive, new BigNatural(Value));
		}
		else {
			byte Head = (byte) (b & 0x0f);
			int Length;
			if (Head != 0x0f) {
				Length = Head + 4; // Short header.
			}
			else {
				// Length is encoded as an unbound nat.
				Length = ReadLength(In);
				Length += 19; // Add in the bias.

				if (Length < 0) {
					// We wrapped.
					throw new BinaryFormatException("reading unbound integer: data length value too large: " + (uint) Length);
				}
			}

			byte[] Data = In.ReadBytes(Length);
			if (Data.Length < Length) {
				throw new BinaryFormatException("reading unbound integer: trying to read " + Length + " bytes; encountered EOF after " + Data.Length + " bytes");
			}

			bool IsPositive = true;
			if ((Data[0] & 0x80) != 0) {
				IsPositive = false;
				// 2s complement negation: find the least significant "1"
				// and flip all bits more significant than it.
				int i = Length - 1;
				do {
					if (Data[i] != 0) {
						// First byte with a '1' bit.
						Data[i] = (byte) (~Data[i] + 1);
						i--;
						while (i >= 0) {
							// Flip all the bits more significant than it.
							Data[i] = (byte) ~Data[i];
							i--;
						}
						break;
					}
					i--;
				} while (i >= 0);
			}
			return new BigInteger(IsPositive, new BigNatural(Data));
		}
	}

	public static byte ReadOptionIndex1(IO.BinaryReader In)
	{
		return In.ReadByte();
	}

	public static ushort ReadOptionIndex2(IO.BinaryReader In)
	{
		ushort v = In.ReadByte();
		v <<= 8; v |= In.ReadByte();
		return v;
	}

	public static uint ReadOptionIndex3(IO.BinaryReader In)
	{
		uint v = In.ReadByte();
		v <<= 8; v |= In.ReadByte();
		v <<= 8; v |= In.ReadByte();
		return v;
	}

	public static uint ReadOptionIndex4(IO.BinaryReader In)
	{
		uint v = In.ReadByte();
		v <<= 8; v |= In.ReadByte();
		v <<= 8; v |= In.ReadByte();
		v <<= 8; v |= In.ReadByte();
		return v;
	}
}

}
