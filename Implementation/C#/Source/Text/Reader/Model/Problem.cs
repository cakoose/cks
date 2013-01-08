using System.Collections.Generic;
using StringBuilder = System.Text.StringBuilder;

namespace Cks.Text.Reader.Model
{

public sealed class Problem
{
	public readonly Ref Primary;
	public readonly List<Ref> Additional;

	public Problem(Ref Primary, List<Ref> Additional)
	{
		this.Primary = Primary;
		this.Additional = Additional;
	}

	public Problem(Ref Primary) : this(Primary, new List<Ref>(0)) {}
	public Problem(SourcePos SourcePos, string Message)
		: this(new Ref(SourcePos, Message)) {}
	public Problem(SourcePos SourcePos, string Message, SourcePos SourcePos2, string Message2)
		: this(new Ref(SourcePos, Message), SingletonList(new Ref(SourcePos2, Message2))) {}

	private static List<T> SingletonList<T>(T Element)
	{
		List<T> List = new List<T>(1);
		List.Add(Element);
		return List;
	}

	public override string ToString()
	{
		StringBuilder buf = new StringBuilder();
		ToString(buf);
		return buf.ToString();
	}

	public sealed class Ref
	{
		public readonly SourcePos SourcePos;
		public readonly string Message;

		public Ref(SourcePos SourcePos, string Message)
		{
			this.SourcePos = SourcePos;
			this.Message = Message;
		}
	}

	public void ToString(StringBuilder buf)
	{
		Primary.SourcePos.ToString(buf);
		buf.Append(": ");
		buf.Append(Primary.Message);

		foreach (Ref r in Additional) {
			buf.Append("; ");
			buf.Append(r.Message);
			buf.Append(" at [");
			r.SourcePos.ToStringRelative(buf, Primary.SourcePos);
			buf.Append("]");
		}
	}
}

}
