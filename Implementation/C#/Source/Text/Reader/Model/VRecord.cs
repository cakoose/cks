using System.Collections.Generic;
using StringBuilder = System.Text.StringBuilder;
using TextWriter = System.IO.TextWriter;

namespace Cks.Text.Reader.Model
{

public sealed class VRecord : Value
{
	public readonly Dictionary<string,VEntry> Fields;

	public VRecord(SourcePos SourcePos, Dictionary<string,VEntry> Fields) : base(SourcePos)
	{
		this.Fields = Fields;
	}

	public override void ToString(StringBuilder buf)
	{
		buf.Append("{");
		string sep = "";
		foreach (VEntry e in Fields.Values) {
			buf.Append(sep); sep = ", ";
			e.ToString(buf);
		}
		buf.Append("}");
	}

	protected override string DumpHeader()
	{
		return "{...}";
	}

	protected override void DumpChildren(TextWriter Out, uint Depth)
	{
		foreach (VEntry e in Fields.Values) {
			Indent(Out, Depth);
			Out.Write(e.Name);
			Out.WriteLine(" =");
			e.Value.Dump(Out, Depth+1);
		}
	}
}

}


