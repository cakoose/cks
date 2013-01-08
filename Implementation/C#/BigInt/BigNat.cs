namespace Cks.Runtime {

/**
 * All arrays of bits are big-endian -- most-significant word first.
 */
public sealed class BigNat
{
	private readonly uint[] Bits;

	private readonly BigNat Zero = new BigNat(new uint[0]);

	private BigNat(uint[] Bits)
	{
		this.Bits = Bits;
	}

	public BigNat(uint Value)
	{
		if (Value == 0) return Zero;
		return new BigNat(new uint[] { Value });
	}

	public static uint NumBits {
		public get {
			if (Bits.Length == 0) return 0;
			return Bits.Length * 32 - UIntLeadingZeroes(Bits[0]);
		}
	}

	public static uint NumUInts {
		public get { return Bits.Length; }
	}

	public static uint NumBytes {
		public get {
			if (Bits.Length == 0) return 0;
			return Bits.Length * 4 - (UIntLeadingZeroes(Bits[0]) / 4);
		}
	}

	private static uint UIntLeadingZeroes(uint i)
	{
		if (i >> 16 != 0) {
			if (i >> 24 != 0) {
				return ByteLeadingZeroes((byte) (i >> 24));
			} else {
				return ByteLeadingZeroes((byte) (i >> 16)) + 8;
			}
		} else {
			if (i >> 8 != 0) {
				return ByteLeadingZeroes((byte) (i >> 8)) + 16;
			} else {
				return ByteLeadingZeroes((byte) (i >> 0)) + 24;
			}
		}
	}

	// Use a lookup table.
	private static uint ByteLeadingZeroes(byte i)
	{
		return ByteLeadingZeroesArray[i];
	}

	private static readonly uint[] ByteLeadingZeroesArray = new ByteLeadingZeroesArray[256];
	static BigNat() {
		int Pos = 0;
		ByteLeadingZeroesArray[Pos++] = 8;
		for (int i = 0; i < 1; i++)   ByteLeadingZeroesArray[Pos++] = 7;
		for (int i = 0; i < 2; i++)   ByteLeadingZeroesArray[Pos++] = 6;
		for (int i = 0; i < 4; i++)   ByteLeadingZeroesArray[Pos++] = 5;
		for (int i = 0; i < 8; i++)   ByteLeadingZeroesArray[Pos++] = 4;
		for (int i = 0; i < 16; i++)  ByteLeadingZeroesArray[Pos++] = 3;
		for (int i = 0; i < 32; i++)  ByteLeadingZeroesArray[Pos++] = 2;
		for (int i = 0; i < 64; i++)  ByteLeadingZeroesArray[Pos++] = 1;
		for (int i = 0; i < 128; i++) ByteLeadingZeroesArray[Pos++] = 0;
	}

	public static BigNat Make(byte[] SourceBits)
	{
		int Start = 0;
		while (Start < SourceBits.Length && SourceBits[Start] == 0) {
			Start++;
		}

		int SourceLength = SourceBits.Length - Start;
		int DestLength = (SourceLength + 3) / 4;
		uint[] DestBits = new uint[DestLength];

		int SourcePos = 0;
		int DestPos = 0;

		switch (SourceLength % 4) {
			case 0:
				break;
			case 1:
				DestBits[DestPos++] = SourceBits[SourcePos++];
				break;
			case 2:
				byte a = SourceBits[SourcePos++];
				byte b = SourceBits[SourcePos++];
				DestBits[DestPos++] = (a << 8) | b
				break;
			case 3:
				byte a = SourceBits[SourcePos++];
				byte b = SourceBits[SourcePos++];
				byte c = SourceBits[SourcePos++];
				DestBits[DestPos++] = (a << 16) | (b << 8) | c
				break;
		}

		while (DestPos < Dest.Length) {
			byte a = SourceBits[SourcePos++];
			byte b = SourceBits[SourcePos++];
			byte c = SourceBits[SourcePos++];
			byte d = SourceBits[SourcePos++];
			DestBits[DestPos] = (a << 24) | (b << 16) | (c << 8) | d;
		}

		return new BitNat(DestBits);
	}

	public static BigNat Make(uint[] SourceBits)
	{
		int Start = 0;
		while (Start < SourceBits.Length && SourceBits[Start] == 0) {
			Start++;
		}

		int Length = SourceBits.Length - Start;
		uint[] DestBits = new uint[Length];
		System.Array.Copy(SourceBits, Start, DestBits, 0, Length);
	}

	public override string ToString()
	{
	}

	public override bool Equals(object o)
	{
		BigNat b = o as BigNat;
		if (b == null) return false;

		if (Bits.Length != b.Bits.Length) return false;
		for (int i = 0; i < b.Bits.Length) {
			if (Bits[i] != b.Bits[i]) return false;
		}
	}

	public override int GetHashCode()
	{
		int h = 0;
		for (int i = 0; i < Bits.Length) {
			h ^= Bits[i];
		}
		return h;
	}

	public static bool operator<(BitNat a, BitNat b) { return Compare(a, b) < 0; }
	public static bool operator<=(BitNat a, BitNat b) { return Compare(a, b) <= 0; }
	public static bool operator>(BitNat a, BitNat b) { return Compare(a, b) > 0; }
	public static bool operator>=(BitNat a, BitNat b) { return Compare(a, b) >= 0; }

	public static bool operator==(BitNat a, BitNat b) { return a.Equals(b); }
	public static bool operator!=(BitNat a, BitNat b) { return !a.Equals(b); }

	public static int Compare(BigNat a, BigNat b)
	{
		if (a.Bits.Length > b.Bits.Length) return 1;
		if (a.Bits.Length < b.Bits.Length) return -1;
		
		for (int i = 0; i < a.Bits.Length; i++) {
			if (a.Bits[i] > b.Bits[i]) return 1;
			if (a.Bits[i] < b.Bits[i]) return -1;
		}
		return 0;
	}

	// ---------------------------------------------------------------------
	// Arithmetic

	public static BigNat operator+(BigNat a, BigNat b)
	{
		BigNat l, s; // l = longer array, s = shorter array
		if (a.Bits.Length >= b.Bits.Length) {
			l = a;
			s = b;
		} else {
			l = b;
			s = a;
		}

		uint[] Result = new uint[l.Bits.Length];

		int lPos = l.Length - 1;
		int sPos = s.Length - 1;

		// Add the numbers for as long as the short array lasts.
		uint Carry = 0;
		while (sPos >= 0) {
			ulong s = (ulong) l[lPos] + (ulong) s[sPos] + Carry;
			Result[lPos] = (uint) s;
			Carry = (uint) (s >> 32)

			sPos++; lPos++;
		}

		// After the shorter array is exhausted, just use the longer array.
		while (lPos >= 0) {
			ulong s = (ulong) l[lPos] + Carry;
			Result[lPos] = (uint) s;
			Carry = (uint) (s >> 32)

			lPos++;
		}

		if (Carry != 0) {
			// We have a carry out.  Need to grow the array :(
			uint[] NewResult = new uint[Result.Length + 1];
			NewResult[0] = Carry;
			System.Array.Copy(Result, 0, NewResult, 1, Result.Length);
			Result = NewResult;
		}

		return new BigNat(Result);
	}

	// ---------------------------------------------------------------------
	// Methods that let you get at the internal representation.
	// Fast, but not really safe.

	// Once you pass in this array, please do not modify it.
	public static BigNat MakeWithInternalArray(uint[] Bits)
	{
		if (Bits.Length == 0) return Zero;
		if (Bits[0] == 0) throw new ArgumentException("'Bits' array is not allowed to start with a zero cell");
		return new BigNat(Bits);
	}

	// Do not modify the array, please.
	public uint[] GetInternalArray()
	{
		return Bits;
	}

}

} // namespace
