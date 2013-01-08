using System.Collections.Generic;
using StringBuilder = System.Text.StringBuilder;
using BigInteger = Cks.Data.BigInteger;
using TR = System.IO.TextReader;
using Cks.Text.Reader.Model;
using Debug = System.Diagnostics.Debug;
using ApplicationException = System.ApplicationException;

namespace Cks.Text.Reader.Parser {

public sealed class TextParser
{
	public static Value Parse(TR In)
	{
		return Parse(In, 1, 1);
	}

	public static Value Parse(TR In, uint StartLine, uint StartColumn)
	{
		TextParser p = new TextParser(new Tokenizer(In, StartLine, StartColumn));
		return p.Parse();
	}

	private readonly Tokenizer Tokenizer;

	public TextParser(Tokenizer Tokenizer)
	{
		this.Tokenizer = Tokenizer;
		this.Peek = Tokenizer.Lex();
	}
	
	// ------------------------------------------------
	// Tokenizer
	
	private Token Peek;

	private Token Take()
	{
		Token t = Peek;
		Peek = Tokenizer.Lex();
		return t;
	}

	private void Next()
	{
		Peek = Tokenizer.Lex();
	}

	private Token Expect(uint Type)
	{
		Token t = Take();
		if (t.Type != Type) {
			StringBuilder Message = new StringBuilder();
			Message.Append("expecting ");
			Token.ToStringContent(Message, Type);
			Message.Append(", found ");
			t.ToStringContent(Message);
			throw pex(t, Message.ToString());
		}
		return t;
	}

	private static ProblemException pex(Token t, string Message)
	{
		return new ProblemException(new Problem(t.SourcePos, Message));
	}

	private static ProblemException pex(SourcePos t1, string m1, SourcePos t2, string m2)
	{
		return new ProblemException(new Problem(t1, m1, t2, m2));
	}

	private static ProblemException pex(Token t, string Message, SourcePos t2, string m2)
	{
		return new ProblemException(new Problem(t.SourcePos, Message, t2, m2));
	}

	// ------------------------------------------------
	// Parser
	
	private Value Parse()
	{
		Value v = pValue();
		Expect(Codes.Eof);
		return v;
	}

	private Value pValue()
	{
		Token t = Peek;
		if (t.Type == Codes.Ident) {
			return pVariant();
		}
		else if (t.Type == '{') {
			return pRecord();
		}
		else if (t.Type == '[') {
			return pCollection();
		}
		else if (t.Type == '(') {
			Next();
			Expect(')');
			return new VPrimitive.Void(t.SourcePos);
		}
		else if (t.Type == Codes.LitString) {
			Next();
			return new VPrimitive.String(t.SourcePos, ((Token.LitString)t).Value);
		}
		else if (t.Type == Codes.LitInt) {
			Next();
			return new VPrimitive.Int(t.SourcePos, ((Token.LitInt)t).Value);
		}
		else {
			throw pex(t, "expecting start of a new value");
		}
	}

	private VEntry pVariant()
	{
		Token Label = Take();
		Debug.Assert(Label.Type == Codes.Ident);

		string Name = ((Token.Ident)Label).Text;

		Value Value;
		if (Peek.Type == ':') {
			// Value follows.  Parse it.
			Next();
			Value = pValue();
		}
		else {
			// No value specified.
			Value = new VPrimitive.Void(Label.SourcePos);
		}

		return new VEntry(Label.SourcePos, Name, Value);
	}

	private VRecord pRecord()
	{
		Token Open = Take();
		Debug.Assert(Open.Type == '{');

		Dictionary<string,VEntry> Fields = new Dictionary<string,VEntry>();
		pEntries(Fields);

		Token Close = Take();
		if (Close.Type != '}') {
			throw pex(Close, "expecting either more fields or a closing brace for a record",
				Open.SourcePos, "opened");
		}

		return new VRecord(Open.SourcePos, Fields);
	}

	private void pEntries(Dictionary<string,VEntry> Entries)
	{
		bool NeedDivider = false;

		while (true) {
			Token t = Peek;
			if (t.Type != Codes.Ident) break;

			if (NeedDivider && !Tokenizer.LastWhitespaceHadEol) {
				throw pex(t, "multiple entries on the same line need to be separated by commas");
			}

			Next();
			Token.Ident NameToken = (Token.Ident) t;
			Expect('=');
			Value Value = pValue();

			AddEntry(Entries, NameToken.SourcePos, ((Token.Ident)NameToken).Text, Value);

			if (Peek.Type == ',') {
				Next();
				NeedDivider = false;
			} else {
				NeedDivider = true;
			}
		}
	}

	private void AddEntry(Dictionary<string,VEntry> Entries, SourcePos Loc, string Name, Value Value)
	{
		VEntry Displaced;
		Entries.TryGetValue(Name, out Displaced);
		if (Displaced != null) {
			throw pex(Loc, "duplicate definition of \"" + Name + "\"",
				Displaced.SourcePos, "original definition");
		}

		Entries.Add(Name, new VEntry(Loc, Name, Value));
	}

	private VCollection pCollection()
	{
		Token Open = Take();
		Debug.Assert(Open.Type == '[');

		List<VCollection.Element> Elements = new List<VCollection.Element>();
		pCollectionElements(Elements);

		Token Close = Take();
		Debug.Assert(Close.Type == ']');

		return new VCollection(Open.SourcePos, Elements);
	}

	private void pCollectionElements(List<VCollection.Element> Elements)
	{
		bool NeedDivider = false;

		while (true) {
			if (Peek.Type == ']') break;

			if (NeedDivider && !Tokenizer.LastWhitespaceHadEol) {
				throw pex(Peek, "multiple entries on the same line need to be separated by commas");
			}

			// Collection element
			Value First = pValue();
			VCollection.Element e;

			if (Peek.Type == Codes.RightArrow) {
				// There's more: "->" Value
				Token Arrow = Take();
				Value Second = pValue();
				e = new VCollection.Element(First, Arrow.SourcePos, Second);
			} else {
				e = new VCollection.Element(First);
			}
			Elements.Add(e);

			if (Peek.Type == ',') {
				Next();
				NeedDivider = false;
			} else {
				NeedDivider = true;
			}
		}
	}
}

}
