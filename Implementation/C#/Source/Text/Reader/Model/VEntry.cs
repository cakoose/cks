using StringBuilder = System.Text.StringBuilder;
using TextWriter = System.IO.TextWriter;

namespace Cks.Text.Reader.Model
{

public sealed class VEntry : Value
{
	public readonly string Name;
	public readonly Value Value;

	public VEntry(SourcePos SourcePos, string Name, Value Value) : base(SourcePos)
	{
		this.Name = Name;
		this.Value = Value;
	}

	public override void ToString(StringBuilder buf)
	{
		buf.Append(Name);
		buf.Append(": ");
		Value.ToString(buf);
	}

	protected override string DumpHeader()
	{
		return Name + ":";
	}

	protected override void DumpChildren(TextWriter Out, uint Depth)
	{
		Value.Dump(Out, Depth);
	}
}

}

