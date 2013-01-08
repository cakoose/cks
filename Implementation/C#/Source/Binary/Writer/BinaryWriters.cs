using C = Cks.Data;
using IO = System.IO;
using StringBuilder = System.Text.StringBuilder;
using BigInteger = Cks.Data.BigInteger;
using BigNatural = Mono.Math.BigInteger;
using Debug = System.Diagnostics.Debug;
using ApplicationException = System.ApplicationException;

namespace Cks.Binary.Writer
{

public static class BinaryWriters
{
	public static readonly BinaryWriter<BigInteger> Nat = new _Nat();
	public class _Nat : BinaryWriter<BigInteger>
	{
		public override void Write(IO.BinaryWriter Out, BigInteger v)
		{
			WriteUnboundNat(Out, v.Magnitude);
		}
	}

	public static readonly BinaryWriter<BigInteger> Int = new _Int();
	public class _Int : BinaryWriter<BigInteger>
	{
		public override void Write(IO.BinaryWriter Out, BigInteger v)
		{
			WriteUnboundInt(Out, v);
		}
	}

	public static readonly BinaryWriter<ulong> Nat64 = new _Nat64();
	private class _Nat64 : BinaryWriter<ulong>
	{
		public override void Write(IO.BinaryWriter Out, ulong v)
		{
			Out.Write((byte) (v >> 56));
			Out.Write((byte) (v >> 48));
			Out.Write((byte) (v >> 40));
			Out.Write((byte) (v >> 32));
			Out.Write((byte) (v >> 24));
			Out.Write((byte) (v >> 16));
			Out.Write((byte) (v >> 8));
			Out.Write((byte) v);
		}
	}

	public static readonly BinaryWriter<long> Int64 = new _Int64();
	private class _Int64 : BinaryWriter<long>
	{
		public override void Write(IO.BinaryWriter Out, long v)
		{
			Out.Write((byte) (v >> 56));
			Out.Write((byte) (v >> 48));
			Out.Write((byte) (v >> 40));
			Out.Write((byte) (v >> 32));
			Out.Write((byte) (v >> 24));
			Out.Write((byte) (v >> 16));
			Out.Write((byte) (v >> 8));
			Out.Write((byte) v);
		}
	}

	public static readonly BinaryWriter<uint> Nat32 = new _Nat32();
	private class _Nat32 : BinaryWriter<uint>
	{
		public override void Write(IO.BinaryWriter Out, uint v)
		{
			Out.Write((byte) (v >> 24));
			Out.Write((byte) (v >> 16));
			Out.Write((byte) (v >> 8));
			Out.Write((byte) v);
		}
	}

	public static readonly BinaryWriter<int> Int32 = new _Int32();
	private class _Int32 : BinaryWriter<int>
	{
		public override void Write(IO.BinaryWriter Out, int v)
		{
			Out.Write((byte) (v >> 24));
			Out.Write((byte) (v >> 16));
			Out.Write((byte) (v >> 8));
			Out.Write((byte) v);
		}
	}

	public static readonly BinaryWriter<ushort> Nat16 = new _Nat16();
	private class _Nat16 : BinaryWriter<ushort>
	{
		public override void Write(IO.BinaryWriter Out, ushort v)
		{
			Out.Write((byte) (v >> 8));
			Out.Write((byte) v);
		}
	}

	public static readonly BinaryWriter<short> Int16 = new _Int16();
	private class _Int16 : BinaryWriter<short>
	{
		public override void Write(IO.BinaryWriter Out, short v)
		{
			Out.Write((byte) (v >> 8));
			Out.Write((byte) v);
		}
	}

	public static readonly BinaryWriter<byte> Nat8 = new _Nat8();
	private class _Nat8 : BinaryWriter<byte>
	{
		public override void Write(IO.BinaryWriter Out, byte v)
		{
			Out.Write((byte) v);
		}
	}

	public static readonly BinaryWriter<sbyte> Int8 = new _Int8();
	private class _Int8 : BinaryWriter<sbyte>
	{
		public override void Write(IO.BinaryWriter Out, sbyte v)
		{
			Out.Write((byte) v);
		}
	}

	// --------------------------------------------------------

	public static readonly BinaryWriter<bool> Bool = new _Bool();
	private class _Bool : BinaryWriter<bool>
	{
		public override void Write(IO.BinaryWriter Out, bool v)
		{
			Out.Write((byte) (v ? 1 : 0));
		}
	}

	public static readonly BinaryWriter<string> String = new _String();
	private class _String : BinaryWriter<string>
	{
		public override void Write(IO.BinaryWriter Out, string v)
		{
			int CharLength = v.Length;

			byte[] ByteOut = new byte[CharLength*3]; // We'll need (at most) 3 byte per char
			int ByteI = 0; // Output array index.

			int CharPairs = 0; // Counts how many UTF-16 surrogate pairs we encounter.
			int CharI = 0;

			char c;

			// NOTE: This first loop makes the code run faster in the ASCII-only case,
			// but I don't know why (30% less execution time on Java6+amd64).
			while (true) {
				if (CharI >= CharLength) goto EndCopyLoop;
				c = v[CharI++];
				if (c >= 128) break;
				ByteOut[ByteI++] = (byte) c;
			}

			while (true) {
				if ((c >> 7) == 0) {
					ByteOut[ByteI++] = (byte) c;
				}
				else if ((c >> 14) == 0) {
					ByteOut[ByteI++] = (byte) ((c >> 8) | 0x80);
					ByteOut[ByteI++] = (byte) c;
				}
				else if ((c >> 10) == (0xD800 >> 10)) {
					// High surrogate.  This means two characters in UTF-16.
					if (CharI < CharLength) {
						char c2 = v[CharI++];
						if ((c2 >> 10) == (0xDC00 >> 10)) {
							// Valid low surrogate.  Compose them to get the full code point.
							int Combo = ((c & 0x3ff) << 10) | (c2 & 0x3ff);
							int CodePoint = 0x10000 + Combo;
							ByteOut[ByteI++] = (byte) (0xc0 | (CodePoint >> 16));
							ByteOut[ByteI++] = (byte) (CodePoint >> 8);
							ByteOut[ByteI++] = (byte) CodePoint;
							CharPairs++;
						}
					}
				}
				else if (c > 0xDFFF) {
					int CodePoint = c;
					ByteOut[ByteI++] = (byte) (0xc0 | (CodePoint >> 16));
					ByteOut[ByteI++] = (byte) (CodePoint >> 8);
					ByteOut[ByteI++] = (byte) CodePoint;
				}
				else {
					// Stranded low surrogate.
					throw new System.ArgumentException("Found UTF-16 low surrogate without preceding high surrogate at index " + (CharI-1) + ": low surrogate = 0x" + ((uint) c).ToString("x"));
				}
				if (CharI >= CharLength) goto EndCopyLoop;
				c = v[CharI++];
			}

			EndCopyLoop:

			int NumCodePoints = CharI - CharPairs;

			WriteLength(Out, NumCodePoints);
			Out.Write(ByteOut, 0, ByteI);
		}
	}

	public static readonly BinaryWriter<C.Void> Void = new _Void();
	private class _Void : BinaryWriter<C.Void>
	{
		public override void Write(IO.BinaryWriter Out, C.Void v)
		{
			// Don't write anything.
		}
	}

	// --------------------------------------------------------
	
	public sealed class List<T> : BinaryWriter<C.List<T>>
	{
		private readonly BinaryWriter<T> mT;

		public List(BinaryWriter<T> mT)
		{
			this.mT = mT;
		}

		public override void Write(IO.BinaryWriter Out, C.List<T> l)
		{
			WriteLength(Out, l.Count);
			foreach (T e in l) {
				mT.Write(Out, e);
			}
		}
	}
	
	public sealed class Set<T> : BinaryWriter<C.HashSet<T>>
	{
		private readonly BinaryWriter<T> mT;

		public Set(BinaryWriter<T> mT)
		{
			this.mT = mT;
		}

		public override void Write(IO.BinaryWriter Out, C.HashSet<T> l)
		{
			WriteLength(Out, l.Count);
			foreach (T e in l) {
				mT.Write(Out, e);
			}
		}
	}
	
	public sealed class Map<K,V> : BinaryWriter<C.Dictionary<K,V>>
	{
		private readonly BinaryWriter<K> mK;
		private readonly BinaryWriter<V> mV;

		public Map(BinaryWriter<K> mK, BinaryWriter<V> mV)
		{
			this.mK = mK;
			this.mV = mV;
		}

		public override void Write(IO.BinaryWriter Out, C.Dictionary<K,V> l)
		{
			WriteLength(Out, l.Count);
			// TODO: More efficient iteration (i.e. avoid lookup operation "l[Key]")
			foreach (K Key in l.Keys) {
				mK.Write(Out, Key);
				mV.Write(Out, l[Key]);
			}
		}
	}
	
	public sealed class Maybe<T> : BinaryWriter<C.Maybe<T>>
	{
		private readonly BinaryWriter<T> mT;

		public Maybe(BinaryWriter<T> mT)
		{
			this.mT = mT;
		}

		public override void Write(IO.BinaryWriter Out, C.Maybe<T> v)
		{
			if (v is C.Maybe<T>.Nothing) {
				Out.Write((byte) 0);
			} else {
				Out.Write((byte) 1);
				mT.Write(Out, ((C.Maybe<T>.Just) v).Value);
			}
		}
	}

	public static void WriteLength(IO.BinaryWriter Out, int v)
	{
		WriteLength(Out, (uint) v);
	}

	public static void WriteLength(IO.BinaryWriter Out, uint v)
	{
		if ((v >> 7) == 0) {
			// "0" [7 bits]
			Out.Write((byte) v);
		}
		else if ((v >> 14) == 0) {
			// "10" [14 bits]
			Out.Write((byte) (0x80 | (v >> 8)));
			Out.Write((byte) v);
		}
		else if ((v >> 21) == 0) {
			// "110" [21 bits]
			Out.Write((byte) (0xc0 | (v >> 16)));
			Out.Write((byte) (v >> 8));
			Out.Write((byte) v);
		}
		else if ((v >> 28) == 0) {
			// "1110" [4 bits] + 3 bytes
			Out.Write((byte) (0xe0 | (v >> 24)));
			Out.Write((byte) (v >> 16));
			Out.Write((byte) (v >> 8));
			Out.Write((byte) v);
		}
		else {
			// "1111" "0000" [4 bytes]
			Out.Write((byte) 0xf0);
			Out.Write((byte) (v >> 24));
			Out.Write((byte) (v >> 16));
			Out.Write((byte) (v >> 8));
			Out.Write(v);
		}
	}

	public static void WriteUnboundNat(IO.BinaryWriter Out, BigNatural v)
	{
		int Bits = v.BitCount();

		if (Bits <= 32) {
			byte[] Data = v.GetBytes();
			Debug.Assert(Data.Length <= 4);
			uint ui = Data[0];
			if (Data.Length >= 2) {
				ui <<= 8; ui |= Data[1];
				if (Data.Length >= 3) {
					ui <<= 8; ui |= Data[2];
					if (Data.Length >= 4) {
						ui <<= 8; ui |= Data[3];
					}
				}
			}
			WriteLength(Out, ui);
		}
		else {
			// "1111" [nnnn] + (n+4) bytes
			int Bytes = (Bits + 7) / 8;
			if (Bytes <= 18) {
				// Short header: "1111" [length-4 in 4 bits]
				byte Header = (byte) (0xf0 | (Bytes - 4));
				Out.Write((byte) Header);
			}
			else {
				// Long header: "1111" "1111" [length-19 as unbound nat]
				Out.Write((byte) 0xff);
				WriteLength(Out, Bytes-19);
			}
			byte[] Data = v.GetBytes();
			Debug.Assert(Data.Length == Bytes);
			Out.Write(Data);
		}
	}

	private static void WriteIntBits(IO.BinaryWriter Out, int Bits, byte[] Data, byte Filler)
	{
		int AvailableBytes = Data.Length;
		int Bytes = (Bits + 7) / 8;

		if (Bits <= 7) {
			if (AvailableBytes < 1) {
				Out.Write((byte) (Filler & 0x7f));
			} else {
				Out.Write((byte) (Data[0] & 0x7f));
			}
		}
		else if (Bits <= 14) {
			if (AvailableBytes < 2) {
				Out.Write((byte) ((1 << 7) | (Filler & 0x3f)));
				Out.Write((byte) Data[0]);
			} else {
				Out.Write((byte) ((1 << 7) | (Data[0] & 0x3f)));
				Out.Write((byte) Data[1]);
			}
		}
		else if (Bits <= 21) {
			if (AvailableBytes < 3) {
				Out.Write((byte) ((3 << 6) | (Filler & 0x1f)));
				Out.Write((byte) Data[0]);
				Out.Write((byte) Data[1]);
			} else {
				Out.Write((byte) ((3 << 6) | (Data[0] & 0x1f)));
				Out.Write((byte) Data[1]);
				Out.Write((byte) Data[2]);
			}
		}
		else if (Bits <= 28) {
			if (AvailableBytes < 4) {
				Out.Write((byte) ((7 << 5) | (Filler & 0x0f)));
				Out.Write((byte) Data[0]);
				Out.Write((byte) Data[1]);
				Out.Write((byte) Data[2]);
			} else {
				Out.Write((byte) ((7 << 5) | (Data[0] & 0x0f)));
				Out.Write((byte) Data[1]);
				Out.Write((byte) Data[2]);
				Out.Write((byte) Data[3]);
			}
		}
		else {
			// "1111" [nnnn] + (n+4) bytes
			if (Bytes <= 18) {
				// Short header: "1111" [length-4 in 4 bits]
				byte Header = (byte) (0xf0 | (Bytes - 4));
				Out.Write((byte) Header);
			}
			else {
				// Long header: "1111" "1111" [length-19 as unbound nat]
				Out.Write((byte) 0xff);
				WriteLength(Out, Bytes-19);
			}

			if (AvailableBytes < Bytes) Out.Write((byte) Filler);
			Out.Write(Data);
		}
	}

	// Similar to 'Nat', except write it out in 2s complement.
	public static void WriteUnboundInt(IO.BinaryWriter Out, BigInteger v)
	{
		if (v == BigInteger.Zero) {
			Out.Write((byte) 0);
		}
		else if (v.IsPositive) {
			// Positive integer.
			BigNatural m = v.Magnitude;
			Debug.Assert(m != 0);
			int Bits = m.BitCount() + 1; // Need a leading zero in addition to the regular bits.

			WriteIntBits(Out, Bits, m.GetBytes(), 0);
		}
		else {
			// Negative integer.
			BigNatural m = v.Magnitude;
			Debug.Assert(m != 0);
			byte[] Data = m.GetBytes();

			bool SingleOneBit = false; // Whether exactly one 1-bit is set

			// Find the least significant 1 bit, then flip all the bits
			// in more significant positions.
			int i = Data.Length - 1;
			do {
				if (Data[i] != 0) {
					byte b = Data[i];
					if ((b & (b-1)) == 0) SingleOneBit = true;
					Data[i] = (byte) (~b + 1);
					i--;
					if (i >= 0) {
						SingleOneBit = false;
						do {
							Data[i] = (byte) ~Data[i];
							i--;
						} while (i >= 0);
					}
					break;
				}
				i--;
			} while (i >= 0);

			int Bits;
			if (SingleOneBit) {
				Bits = m.BitCount();
			} else {
				Bits = m.BitCount() + 1;
			}

			WriteIntBits(Out, Bits, Data, 0xff);
		}
	}

	public static void WriteOptionIndex1(IO.BinaryWriter Out, uint Index)
	{
		Debug.Assert(Index < ((uint)1 << 8));
		Out.Write((byte) Index);
	}

	public static void WriteOptionIndex2(IO.BinaryWriter Out, uint Index)
	{
		Debug.Assert(Index < ((uint)1 << 16));
		Out.Write((byte) (Index >> 8));
		Out.Write((byte) Index);
	}

	public static void WriteOptionIndex3(IO.BinaryWriter Out, uint Index)
	{
		Debug.Assert(Index < ((uint)1 << 24));
		Out.Write((byte) (Index >> 16));
		Out.Write((byte) (Index >> 8));
		Out.Write((byte) Index);
	}

	public static void WriteOptionIndex4(IO.BinaryWriter Out, uint Index)
	{
		Out.Write((byte) (Index >> 24));
		Out.Write((byte) (Index >> 16));
		Out.Write((byte) (Index >> 8));
		Out.Write((byte) Index);
	}
}

} // namespace

