using C = Cks.Data;
using SC = System.Collections.Generic;
using StringBuilder = System.Text.StringBuilder;
using BigInteger = Cks.Data.BigInteger;
using BI = Mono.Math.BigInteger;
using Math = System.Math;
using Cks.Text.Reader.Model;
using Debug = System.Diagnostics.Debug;
using ApplicationException = System.ApplicationException;

namespace Cks.Text.Reader
{

public static class TextReaders
{
	public static SC.Dictionary<string,VEntry> GetFields(Value v)
	{
		VRecord r = v as VRecord;
		if (r == null) throw pex(v, "expecting a record value");
		return r.Fields;
	}

	public static T GetField<T>(Value v, ref uint NumFieldsUsed, SC.Dictionary<string,VEntry> Fields, string Name, TextReader<T> m)
	{
		if (Fields.ContainsKey(Name)) {
			// Field is present.  Handle it with the given TextReader.
			VEntry e = Fields[Name];
			NumFieldsUsed++;
			return m.Marshal(e.Value);
		}
		else {
			// Field not present.  See if there's a default value.
			C.Maybe<T>.Just Default = m.Default as C.Maybe<T>.Just;
			if (Default == null) throw pex(v, "missing entry for field \"" + Name + "\"");
			return Default.Value;
		}
	}

	public static void ReportExtraFields(SC.Dictionary<string,VEntry> GivenFields, string[] ValidFields)
	{
		// Figure out which fields are "extra" (i.e. provided in the value but not part of the type).
		SC.HashSet<string> Extras = new SC.HashSet<string>(GivenFields.Keys);
		foreach (string f in ValidFields) {
			Extras.Remove(f);
		}
		Debug.Assert(Extras.Count > 0); // The caller should have checked this already.

		// Get one of the extra fields.
		SC.IEnumerator<string> Enum = Extras.GetEnumerator();
		bool Moved = Enum.MoveNext();
		Debug.Assert(Moved);
		string ExtraName = Enum.Current;

		// Report it.
		VEntry ExtraEntry = GivenFields[ExtraName];
		throw pex(ExtraEntry, "record type does not have a field named \"" + ExtraName + "\"");
	}

	public static VEntry GetVariant(Value v)
	{
		VEntry e = v as VEntry;
		if (e == null) throw pex(v, "expecting a variant value");
		return e;
	}

	public static readonly TextReader<bool> Bool = new _Bool();
	private sealed class _Bool : TextReader<bool>
	{
		public override bool Marshal(Value v)
		{
			if (v is VEntry) {
				VEntry e = (VEntry) v;
				bool r;
				if (e.Name == "True") {
					r = true;
				} else if (e.Name == "False") {
					r = false;
				} else {
					throw pex(v, "expecting a boolean value (either \"True\" or \"False\")");
				}
				if (!(e.Value is VPrimitive.Void)) {
					throw pex(v, "option \"" + e.Name + "\" requires a void value");
				}
				return r;
			}
			else {
				throw pex(v, "expecting a boolean value (either \"True\" or \"False\")");
			}
		}
	}

	public sealed class Maybe<T> : TextReader<C.Maybe<T>>
	{
		public readonly TextReader<T> mT;

		public Maybe(TextReader<T> mT)
		{
			this.mT = mT;
		}

		public override C.Maybe<T> Marshal(Value v)
		{
			if (v is VEntry) {
				VEntry e = (VEntry) v;
				if (e.Name == "Set") {
					T t = mT.Marshal(e.Value);
					return new C.Maybe<T>.Just(t);
				}
				else if (e.Name == "None") {
					if (!(e.Value is VPrimitive.Void)) {
						throw pex(v, "option \"None\" requires a void value");
					}
					return new C.Maybe<T>.Nothing();
				}
				else {
					throw pex(v, "expecting an optional value (either \"Set\" with a value or \"None\"");
				}
			}
			else {
				throw pex(v, "expecting an optional value (either \"Set\" with a value or \"None\"");
			}
		}

		public override C.Maybe<C.Maybe<T>> Default
		{
			get { return new C.Maybe<C.Maybe<T>>.Just(new C.Maybe<T>.Nothing()); }
		}
	}

	public static readonly TextReader<string> String = new _String();
	private sealed class _String : TextReader<string>
	{
		public override string Marshal(Value v)
		{
			VPrimitive.String s = v as VPrimitive.String;
			if (s == null) throw pex(v, "expecting a string");
			return s.Value;
		}
	}

	public sealed class List<T> : TextReader<C.List<T>>
	{
		public readonly TextReader<T> mT;

		public List(TextReader<T> mT)
		{
			this.mT = mT;
		}

		public override C.List<T> Marshal(Value v)
		{
			VCollection l = v as VCollection;
			if (l == null) throw pex(v, "expecting a list");
			C.List<T> r = new C.List<T>(l.Elements.Count);
			foreach (VCollection.Element e in l.Elements) {
				if (e.ArrowPos != null) {
					throw pex(e.ArrowPos, "expecting a list element, found a map entry");
				}
				r.Add(mT.Marshal(e.First));
			}
			return r;
		}
	}

	public sealed class Set<T> : TextReader<C.HashSet<T>>
	{
		public readonly TextReader<T> mT;

		public Set(TextReader<T> mT)
		{
			this.mT = mT;
		}

		public override C.HashSet<T> Marshal(Value v)
		{
			VCollection l = v as VCollection;
			if (l == null) throw pex(v, "expecting a set");
			C.HashSet<T> r = new C.HashSet<T>();
			foreach (VCollection.Element e in l.Elements) {
				if (e.ArrowPos != null) {
					throw pex(e.ArrowPos, "expecting a set element, found a map entry");
				}
				T ee = mT.Marshal(e.First);
				if (r.Contains(ee)) {
					throw pex(e.First, "duplicate entry in set");
				}
				r.Add(ee);
			}
			return r;
		}
	}

	public sealed class Map<K,V> : TextReader<C.Dictionary<K,V>>
	{
		public readonly TextReader<K> mK;
		public readonly TextReader<V> mV;

		public Map(TextReader<K> mK, TextReader<V> mV)
		{
			this.mK = mK;
			this.mV = mV;
		}

		public override C.Dictionary<K,V> Marshal(Value v)
		{
			VCollection l = v as VCollection;
			if (l == null) throw pex(v, "expecting a map");
			C.Dictionary<K,V> r = new C.Dictionary<K,V>();
			foreach (VCollection.Element e in l.Elements) {
				K Key = mK.Marshal(e.First);
				V Value;
				if (e.ArrowPos == null) {
					C.Maybe<V>.Just Default = mV.Default as C.Maybe<V>.Just;
					if (Default == null) throw pex(e.First, "missing value for map entry");
					Value = Default.Value;
				} else {
					Value = mV.Marshal(e.Second);
				}
				if (r.ContainsKey(Key)) {
					throw pex(e.First, "duplicate key in map");
				}
				r.Add(Key, Value);
			}
			return r;
		}
	}

	public static readonly TextReader<BigInteger> Int = new _Int();
	public sealed class _Int : TextReader<BigInteger>
	{
		public override BigInteger Marshal(Value v)
		{
			VPrimitive.Int i = v as VPrimitive.Int;
			if (i == null) throw pex(v, "expecting an integer");
			return i.Value;
		}
	}

	public static readonly TextReader<BigInteger> Nat = new _Nat();
	public sealed class _Nat : TextReader<BigInteger>
	{
		public override BigInteger Marshal(Value v)
		{
			VPrimitive.Int i = v as VPrimitive.Int;
			if (i == null) throw pex(v, "expecting a natural number");
			if (i.Value < BigInteger.Zero) {
				throw pex(v, "expecting a natural number; negative integers are not allowed");
			}
			return i.Value;
		}
	}

	public abstract class BoundNat<T> : TextReader<T>
	{
		private readonly uint NumBits;
		private readonly BigInteger Max;

		protected BoundNat(uint NumBits)
		{
			this.NumBits = NumBits;

			BI Limit = new BI(1);
			Limit <<= (int) NumBits;

			this.Max = new BigInteger(true, Limit - 1);
		}

		protected abstract T Convert(BigInteger i);

		public override T Marshal(Value v)
		{
			VPrimitive.Int vi = v as VPrimitive.Int;
			if (vi == null) throw pex(v, "expecting a natural number");
			BigInteger i = vi.Value;
			if (i < BigInteger.Zero) throw pex(v, "expecting a natural number; negative integers are not allowed");
			if (i > Max) throw pex(v, "expecting a " + NumBits + "-bit natural number; value cannot be greater than " + Max);
			return Convert(i);
		}
	}

	public abstract class BoundInt<T> : TextReader<T>
	{
		private readonly uint NumBits;
		private readonly BigInteger Max;
		private readonly BigInteger Min;

		protected BoundInt(uint NumBits)
		{
			this.NumBits = NumBits;
			
			BI Half = new BI(1);
			Half <<= (int) (NumBits-1);

			this.Max = new BigInteger(true, Half - 1);
			this.Min = new BigInteger(false, Half);
		}

		protected abstract T Convert(BigInteger i);

		public override T Marshal(Value v)
		{
			VPrimitive.Int vi = v as VPrimitive.Int;
			if (vi == null) throw pex(v, "expecting a natural number");
			BigInteger i = vi.Value;
			if (i < Min) throw pex(v, "expecting a " + NumBits + "-bit integer; value cannot be less than " + Min);
			if (i > Max) throw pex(v, "expecting a " + NumBits + "-bit integer; value cannot be greater than " + Max);
			return Convert(i);
		}
	}

	// ------------------------------------------------------

	public static readonly BoundNat<ulong> Nat64 = new _Nat64();
	private class _Nat64 : BoundNat<ulong>
	{
		public _Nat64() : base(64) {}

		protected override ulong Convert(BigInteger i)
		{
			byte[] data = i.Magnitude.GetBytes();
			ulong v = data[0];
			int End = Math.Min(8, data.Length);
			for (int j = 1; j < End; j++) {
				v <<= 8;
				v |= data[j];
			}
			return v;
		}
	}

	public static readonly BoundNat<uint> Nat32 = new _Nat32();
	private class _Nat32 : BoundNat<uint>
	{
		public _Nat32() : base(32) {}

		protected override uint Convert(BigInteger i)
		{
			byte[] data = i.Magnitude.GetBytes();
			uint v = data[0];
			int End = Math.Min(4, data.Length);
			for (int j = 1; j < End; j++) {
				v <<= 8;
				v |= data[j];
			}
			return v;
		}
	}

	public static readonly BoundNat<ushort> Nat16 = new _Nat16();
	private class _Nat16 : BoundNat<ushort>
	{
		public _Nat16() : base(16) {}

		protected override ushort Convert(BigInteger i)
		{
			byte[] data = i.Magnitude.GetBytes();
			ushort v = data[0];
			if (data.Length > 1) {
				v <<= 8;
				v |= data[1];
			}
			return v;
		}
	}

	public static readonly BoundNat<byte> Nat8 = new _Nat8();
	private class _Nat8 : BoundNat<byte>
	{
		public _Nat8() : base(8) {}

		protected override byte Convert(BigInteger i)
		{
			byte[] data = i.Magnitude.GetBytes();
			return data[0];
		}
	}

	// ------------------------------------------------------

	public static readonly BoundInt<long> Int64 = new _Int64();
	private class _Int64 : BoundInt<long>
	{
		public _Int64() : base(64) {}

		protected override long Convert(BigInteger i)
		{
			byte[] data = i.Magnitude.GetBytes();
			long v = data[0];
			int End = Math.Min(8, data.Length);
			for (int j = 1; j < End; j++) {
				v <<= 8;
				v |= data[j];
			}
			if (!i.IsPositive) v = -v;
			return v;
		}
	}

	public static readonly BoundInt<int> Int32 = new _Int32();
	private class _Int32 : BoundInt<int>
	{
		public _Int32() : base(32) {}

		protected override int Convert(BigInteger i)
		{
			byte[] data = i.Magnitude.GetBytes();
			int v = data[0];
			int End = Math.Min(4, data.Length);
			for (int j = 1; j < End; j++) {
				v <<= 8;
				v |= data[j];
			}
			if (!i.IsPositive) v = -v;
			return v;
		}
	}

	public static readonly BoundInt<short> Int16 = new _Int16();
	private class _Int16 : BoundInt<short>
	{
		public _Int16() : base(16) {}

		protected override short Convert(BigInteger i)
		{
			byte[] data = i.Magnitude.GetBytes();
			short v = data[0];
			if (data.Length > 1) {
				v <<= 8;
				v |= data[1];
			}
			if (!i.IsPositive) v = (short) -v;
			return v;
		}
	}

	public static readonly BoundInt<sbyte> Int8 = new _Int8();
	private class _Int8 : BoundInt<sbyte>
	{
		public _Int8() : base(8) {}

		protected override sbyte Convert(BigInteger i)
		{
			byte[] data = i.Magnitude.GetBytes();
			sbyte v = (sbyte) data[0];
			if (!i.IsPositive) v = (sbyte) -v;
			return v;
		}
	}

	// ------------------------------------------------------
	// pex

	private static ProblemException pex(Node n, string Message)
	{
		return pex(n.SourcePos, Message);
	}

	private static ProblemException pex(SourcePos p, string Message)
	{
		return new ProblemException(new Problem(p, Message));
	}
}

}
