using StringBuilder = System.Text.StringBuilder;
using TextWriter = System.IO.TextWriter;

namespace Cks.Text.Reader.Model
{

public abstract class Node
{
	public readonly SourcePos SourcePos;

	public Node(SourcePos SourcePos)
	{
		this.SourcePos = SourcePos;
	}

	public sealed override string ToString()
	{
		StringBuilder buf = new StringBuilder();
		ToString(buf);
		return buf.ToString();
	}

	public abstract void ToString(StringBuilder buf);

	public void Dump(TextWriter Out)
	{
		Dump(Out, 0);
	}

	public void Dump(TextWriter Out, uint Depth)
	{
		Indent(Out, Depth);
		Out.WriteLine(DumpHeader());
		DumpChildren(Out, Depth+1);
	}

	protected static void Indent(TextWriter Out, uint Depth)
	{
		for (uint i = 0; i < Depth; i++) {
			Out.Write("  ");
		}
	}

	protected virtual string DumpHeader() { return ToString(); }
	protected virtual void DumpChildren(TextWriter Out, uint Depth) { }
}

}
