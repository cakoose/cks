using System.Collections.Generic;
using StringBuilder = System.Text.StringBuilder;
using TextWriter = System.IO.TextWriter;

namespace Cks.Text.Reader.Model
{

public sealed class VCollection : Value
{
	public readonly List<Element> Elements;

	public VCollection(SourcePos SourcePos, List<Element> Elements) : base(SourcePos)
	{
		this.Elements = Elements;
	}

	public override void ToString(StringBuilder buf)
	{
		buf.Append("[");
		string sep = "";
		foreach (Element e in Elements) {
			buf.Append(sep); sep = ", ";
			e.First.ToString(buf);
			if (e.ArrowPos != null) {
				buf.Append(" -> ");
				e.Second.ToString(buf);
			}
		}
		buf.Append("]");
	}

	protected override string DumpHeader()
	{
		return "[...]";
	}

	protected override void DumpChildren(TextWriter Out, uint Depth)
	{
		foreach (Element e in Elements) {
			e.First.Dump(Out, Depth);
			if (e.ArrowPos != null) {
				Indent(Out, Depth+1);
				Out.WriteLine("->");
				e.Second.Dump(Out, Depth+1);
			}
		}
	}

	public sealed class Element
	{
		public readonly Value First;
		public readonly SourcePos ArrowPos;
		public readonly Value Second;

		public Element(Value First)
		{
			this.First = First;
			this.ArrowPos = null;
			this.Second = null;
		}

		public Element(Value First, SourcePos ArrowPos, Value Second)
		{
			this.First = First;
			this.ArrowPos = ArrowPos;
			this.Second = Second;
		}
	}
}

}
