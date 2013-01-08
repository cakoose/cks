using StringBuilder = System.Text.StringBuilder;

namespace Cks.Text.Reader.Model
{

public sealed class SourcePos
{
	public readonly uint Line;
	public readonly uint Column;

	private static readonly char LineColSep = '.';

	public SourcePos(uint Line, uint Column)
	{
		this.Line = Line;
		this.Column = Column;
	}

	public void ToString(StringBuilder buf)
	{
		buf.Append(Line);
		buf.Append(LineColSep);
		buf.Append(Column);
	}

	public void ToStringRelative(StringBuilder buf, SourcePos other)
	{
		if (this.Line == other.Line) {
			// "col <column>"
			buf.Append("col ");
			buf.Append(this.Column);
		} else {
			ToString(buf);
		}
	}

	public override string ToString()
	{
		StringBuilder buf = new StringBuilder();
		ToString(buf);
		return buf.ToString();
	}

	public string ToStringRelative(SourcePos other)
	{
		StringBuilder buf = new StringBuilder();
		ToStringRelative(buf, other);
		return buf.ToString();
	}
}

}
