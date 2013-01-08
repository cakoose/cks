using C = Cks.Data;
using IO = System.IO;
using StringBuilder = System.Text.StringBuilder;
using BigInteger = Cks.Data.BigInteger;
using Debug = System.Diagnostics.Debug;
using ApplicationException = System.ApplicationException;

namespace Cks.Text.Writer
{

public static class TextWriters
{
	public class BigIntegerWriter : TextWriter<BigInteger>
	{
		public override void Write(IO.TextWriter Out, BigInteger v)
		{
			Out.Write(v.ToString());
		}
	}

	private static readonly BigIntegerWriter BigIntegerWriterInstance = new BigIntegerWriter();

	public static readonly TextWriter<BigInteger> Nat = BigIntegerWriterInstance;
	public static readonly TextWriter<BigInteger> Int = BigIntegerWriterInstance;

	public static readonly TextWriter<ulong> Nat64 = new _Nat64();
	private class _Nat64 : TextWriter<ulong>
	{
		public override void Write(IO.TextWriter Out, ulong v) { Out.Write(v); }
	}

	public static readonly TextWriter<long> Int64 = new _Int64();
	private class _Int64 : TextWriter<long>
	{
		public override void Write(IO.TextWriter Out, long v) { Out.Write(v); }
	}

	public static readonly TextWriter<uint> Nat32 = new _Nat32();
	private class _Nat32 : TextWriter<uint>
	{
		public override void Write(IO.TextWriter Out, uint v) { Out.Write(v); }
	}

	public static readonly TextWriter<int> Int32 = new _Int32();
	private class _Int32 : TextWriter<int>
	{
		public override void Write(IO.TextWriter Out, int v) { Out.Write(v); }
	}

	public static readonly TextWriter<ushort> Nat16 = new _Nat16();
	private class _Nat16 : TextWriter<ushort>
	{
		public override void Write(IO.TextWriter Out, ushort v) { Out.Write(v); }
	}

	public static readonly TextWriter<short> Int16 = new _Int16();
	private class _Int16 : TextWriter<short>
	{
		public override void Write(IO.TextWriter Out, short v) { Out.Write(v); }
	}

	public static readonly TextWriter<byte> Nat8 = new _Nat8();
	private class _Nat8 : TextWriter<byte>
	{
		public override void Write(IO.TextWriter Out, byte v) { Out.Write(v); }
	}

	public static readonly TextWriter<sbyte> Int8 = new _Int8();
	private class _Int8 : TextWriter<sbyte>
	{
		public override void Write(IO.TextWriter Out, sbyte v) { Out.Write(v); }
	}

	// --------------------------------------------------------

	public static readonly TextWriter<bool> Bool = new _Bool();
	private class _Bool : TextWriter<bool>
	{
		public override void Write(IO.TextWriter Out, bool v)
		{
			Out.Write(v ? "True" : "False");
		}
	}

	public static readonly TextWriter<C.Void> Void = new _Void();
	private class _Void : TextWriter<C.Void>
	{
		public override void Write(IO.TextWriter Out, C.Void v)
		{
			Out.Write("()");
		}
	}

	private static readonly string Hex = "0123456789abcdef";

	public static readonly TextWriter<string> String = new _String();
	private class _String : TextWriter<string>
	{
		public override void Write(IO.TextWriter Out, string v)
		{
			Out.Write('"');
			char[] HexEscape = null;
			for (int i = 0; i < v.Length; i++) {
				char c = v[i];
				switch (c) {
					case '"': Out.Write("\\\""); break;
					case '\\': Out.Write("\\\\"); break;
					case '\n': Out.Write("\\n"); break;
					case '\t': Out.Write("\\t"); break;
					case '\0': Out.Write("\\0"); break;

					default:
						if (IsControl(c)) {
							// Encode as: "x" + two hex digits.
							if (HexEscape == null) {
								HexEscape = new char[4];
								HexEscape[0] = '\\';
								HexEscape[1] = 'x';
							}
							HexEscape[3] = Hex[c & 0xf];
							c >>= 4;
							HexEscape[2] = Hex[c & 0xf];
							Out.Write(HexEscape, 0, 4);
						}
						else if (IsHighSurrogate(c)) {
							// Surrogate pair
							i++;
							if (i >= v.Length) {
								throw new System.ArgumentException("high surrogate not followed by anything");
							}
							char c2 = v[i];
							if (!IsLowSurrogate(c2)) {
								throw new System.ArgumentException("high surrogate not followed by low surrogate");
							}
							Out.Write(v, i-1, 2);
						}
						else if (IsLowSurrogate(c)) {
							throw new System.ArgumentException("low surrogate without preceding high surrogate");
						}
						else {
							// Basic Multilingual Plane (16 bits)
							Out.Write(c);
						}
						break;
				}
			}
			Out.Write('"');
		}

		public static int ToCodePoint(char High, char Low) {
			return ((High - MIN_HIGH_SURROGATE) << 10)
				+ (Low - MIN_LOW_SURROGATE) + 0x10000;
		}

		public static bool IsControl(char c)
		{
			return (c >= 0 && c <= 0x1f) || (c >= 0x7f && c <= 0x9f);
		}

		public static bool IsHighSurrogate(char c)
		{
			return c >= MIN_HIGH_SURROGATE && c <= MAX_HIGH_SURROGATE;
		}

		public static bool IsLowSurrogate(char c)
		{
			return c >= MIN_LOW_SURROGATE && c <= MAX_LOW_SURROGATE;
		}

		public static readonly char MIN_HIGH_SURROGATE = (char) 0xD800;
		public static readonly char MAX_HIGH_SURROGATE = (char) 0xDBFF;
		public static readonly char MIN_LOW_SURROGATE  = (char) 0xDC00;
		public static readonly char MAX_LOW_SURROGATE  = (char) 0xDFFF;
	}

	// --------------------------------------------------------
	
	public sealed class List<T> : TextWriter<C.List<T>>
	{
		private readonly TextWriter<T> mT;

		public List(TextWriter<T> mT)
		{
			this.mT = mT;
		}

		public override void Write(IO.TextWriter Out, C.List<T> l)
		{
			Out.Write('[');
			string Sep = "";
			foreach (T e in l) {
				Out.Write(Sep); Sep = ", ";
				mT.Write(Out, e);
			}
			Out.Write(']');
		}

		public override void WriteIndented(IndentPrinter Out, C.List<T> l)
		{
			Out.WriteLine('[');
			Out.Indent();
			foreach (T e in l) {
				mT.WriteIndented(Out, e);
			}
			Out.Dedent();
			Out.WriteLine(']');
		}
	}
	
	public sealed class Set<T> : TextWriter<C.HashSet<T>>
	{
		private readonly TextWriter<T> mT;

		public Set(TextWriter<T> mT)
		{
			this.mT = mT;
		}

		public override void Write(IO.TextWriter Out, C.HashSet<T> l)
		{
			Out.Write('[');
			string Sep = "";
			foreach (T e in l) {
				Out.Write(Sep); Sep = ", ";
				mT.Write(Out, e);
			}
			Out.Write(']');
		}

		public override void WriteIndented(IndentPrinter Out, C.HashSet<T> l)
		{
			Out.WriteLine('[');
			Out.Indent();
			foreach (T e in l) {
				mT.WriteIndented(Out, e);
			}
			Out.Dedent();
			Out.WriteLine(']');
		}
	}
	
	public sealed class Map<K,V> : TextWriter<C.Dictionary<K,V>>
	{
		private readonly TextWriter<K> mK;
		private readonly TextWriter<V> mV;

		public Map(TextWriter<K> mK, TextWriter<V> mV)
		{
			this.mK = mK;
			this.mV = mV;
		}

		public override void Write(IO.TextWriter Out, C.Dictionary<K,V> l)
		{
			Out.Write('[');
			string Sep = "";
			// TODO: More efficient iteration (i.e. avoid lookup operation "l[Key]")
			foreach (K Key in l.Keys) {
				Out.Write(Sep); Sep = ", ";
				mK.Write(Out, Key);
				Out.Write(" -> ");
				mV.Write(Out, l[Key]);
			}
			Out.Write(']');
		}

		public override void WriteIndented(IndentPrinter Out, C.Dictionary<K,V> l)
		{
			Out.WriteLine('[');
			Out.Indent();
			// TODO: More efficient iteration (i.e. avoid lookup operation "l[Key]")
			foreach (K Key in l.Keys) {
				mK.WriteIndented(Out, Key);
				Out.Indent();
				Out.Write(" -> ");
				mV.WriteIndented(Out, l[Key]);
				Out.Dedent();
			}
			Out.Dedent();
			Out.WriteLine(']');
		}
	}
	
	public sealed class Maybe<T> : TextWriter<C.Maybe<T>>
	{
		private readonly TextWriter<T> mT;

		public Maybe(TextWriter<T> mT)
		{
			this.mT = mT;
		}

		public override void Write(IO.TextWriter Out, C.Maybe<T> v)
		{
			if (v is C.Maybe<T>.Nothing) {
				Out.Write("None");
			} else {
				Out.Write("Set: ");
				mT.Write(Out, ((C.Maybe<T>.Just) v).Value);
			}
		}

		public override void WriteIndented(IndentPrinter Out, C.Maybe<T> v)
		{
			if (v is C.Maybe<T>.Nothing) {
				Out.WriteLine("None");
			} else {
				Out.Write("Set: ");
				mT.WriteIndented(Out, ((C.Maybe<T>.Just) v).Value);
			}
		}
	}
}

} // namespace
