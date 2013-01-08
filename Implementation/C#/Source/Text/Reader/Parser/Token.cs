using System.Collections.Generic;
using BigInteger = Cks.Data.BigInteger;
using StringBuilder = System.Text.StringBuilder;
using SourcePos = Cks.Text.Reader.Model.SourcePos;

namespace Cks.Text.Reader.Parser {

public class Token
{
	public readonly SourcePos SourcePos;
	public readonly uint Type;

	public Token(SourcePos SourcePos, uint Type)
	{
		this.SourcePos = SourcePos;
		this.Type = Type;
	}

	public class LitInt : Token
	{
		public readonly BigInteger Value;
		public LitInt(SourcePos SourcePos, BigInteger Value) : base(SourcePos, Codes.LitInt)
		{
			this.Value = Value;
		}
		public override void ToStringContent(StringBuilder buf)
		{
			base.ToStringContent(buf);
			buf.Append(' ');
			buf.Append(Value);
		}
	}

	public class LitString : Token
	{
		public readonly string Value;
		public LitString(SourcePos SourcePos, string Value) : base(SourcePos, Codes.LitString)
		{
			this.Value = Value;
		}
		public override void ToStringContent(StringBuilder buf)
		{
			base.ToStringContent(buf);
			buf.Append(" \"");
			foreach (char c in Value) {
				switch (c) {
					case '"':
					case '\\':
						buf.Append('\\');
						buf.Append(c);
						break;
					case '\t': buf.Append("\\t"); break;
					case '\n': buf.Append("\\n"); break;
					case '\r': buf.Append("\\r"); break;
					case '\0': buf.Append("\\0"); break;
					default:
						buf.Append(c);
						break;
				}
			}
			buf.Append("\"");
		}
	}

	public class Ident : Token
	{
		public readonly string Text;
		public Ident(SourcePos SourcePos, string Text) : base(SourcePos, Codes.Ident)
		{
			this.Text = Text;
		}
		public override void ToStringContent(StringBuilder buf)
		{
			base.ToStringContent(buf);
			buf.Append(' ');
			buf.Append(Text);
		}
	}

	// ToString
	
	public override string ToString()
	{
		StringBuilder buf = new StringBuilder();
		ToString(buf);
		return buf.ToString();
	}

	public void ToString(StringBuilder buf)
	{
		buf.Append("[");
		SourcePos.ToString(buf);
		buf.Append("] ");
		ToStringContent(buf);
	}

	public string ToStringContent()
	{
		StringBuilder buf = new StringBuilder();
		ToStringContent(buf);
		return buf.ToString();
	}

	public virtual void ToStringContent(StringBuilder buf)
	{
		ToStringContent(buf, this.Type);
	}

	public static string ToStringContent(uint Type)
	{
		StringBuilder buf = new StringBuilder();
		ToStringContent(buf, Type);
		return buf.ToString();
	}

	public static void ToStringContent(StringBuilder buf, uint Type)
	{
		if (Type < Codes.FirstCode) {
			if (Type == '"' || Type == '\'') {
				buf.Append('(');
				buf.Append((char) Type);
				buf.Append(')');
			} else {
				buf.Append('"');
				buf.Append((char) Type);
				buf.Append('"');
			}
		} else {
			uint Index = Type - Codes.FirstCode;
			Codes.Entry e = Codes.Entries[Index];
			buf.Append(e.Name);
		}
	}

}
	
// Token Codes
class Codes
{
	public sealed class Entry
	{
		public readonly uint Code;
		public readonly string Name;

		public Entry(uint Code, string Name)
		{
			this.Code = Code;
			this.Name = Name;
		}
	}

	public static readonly uint FirstCode = 128;
	public static readonly uint LastCode;
	public static readonly Entry[] Entries;

	private static List<Entry> EntryList = new List<Entry>();
	private static uint mk(string Name)
	{
		uint Code = FirstCode + (uint) EntryList.Count;
		EntryList.Add(new Entry(Code, Name));
		return Code;
	}

	public static readonly uint LitInt = mk("integer literal");
	public static readonly uint LitString = mk("string literal");
	public static readonly uint Ident = mk("identifier");
	public static readonly uint RightArrow = mk("\"->\"");

	public static readonly uint Eof = mk("end of file");

	static Codes() {
		LastCode = FirstCode + (uint) EntryList.Count;
		Entries = EntryList.ToArray();
		EntryList = null;
	}
}

} // namespace
