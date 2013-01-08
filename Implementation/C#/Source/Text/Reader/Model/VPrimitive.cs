using StringBuilder = System.Text.StringBuilder;
using BigInteger = Cks.Data.BigInteger;
using TextWriter = System.IO.TextWriter;
using Void = Cks.Data.Void;

namespace Cks.Text.Reader.Model
{

public abstract class VPrimitive : Value
{
	public VPrimitive(SourcePos SourcePos) : base(SourcePos) {}

	public sealed class Void : VPrimitive
	{
		public Void(SourcePos SourcePos) : base(SourcePos) {}

		public override void ToString(StringBuilder buf) { buf.Append("Void"); }
	}

	public sealed class String : VPrimitive
	{
		public readonly string Value;

		public String(SourcePos SourcePos, string Value) : base(SourcePos)
		{
			this.Value = Value;
		}

		public static void QuotePart(StringBuilder buf, string s)
		{
			buf.Append(s);
		}

		public override void ToString(StringBuilder buf)
		{
			buf.Append('"');
			QuotePart(buf, Value);
			buf.Append('"');
		}
	}

	public sealed class Int : VPrimitive
	{
		public readonly BigInteger Value;

		public Int(SourcePos SourcePos, BigInteger Value) : base(SourcePos)
		{
			this.Value = Value;
		}

		public override void ToString(StringBuilder buf)
		{
			buf.Append(Value);
		}
	}

	public sealed class Bool : VPrimitive
	{
		public readonly bool Value;

		public Bool(SourcePos SourcePos, bool Value) : base(SourcePos)
		{
			this.Value = Value;
		}

		public override void ToString(StringBuilder buf)
		{
			buf.Append(Value ? "True" : "False");
		}
	}
}

}

