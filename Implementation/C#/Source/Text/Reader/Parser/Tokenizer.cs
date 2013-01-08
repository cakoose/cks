using System.Collections.Generic;
using BigInteger = Cks.Data.BigInteger;
using StringBuilder = System.Text.StringBuilder;
using Cks.Text.Reader.Model;
using TR = System.IO.TextReader;

namespace Cks.Text.Reader.Parser {

public class Tokenizer
{
	private readonly TR In;
	private uint Line, Column;

	public Tokenizer(TR In, uint StartLine, uint StartColumn)
	{
		this.In = In;
		this.Line = StartLine;
		this.Column = StartColumn;
	}

	public bool LastWhitespaceHadEol = true;
	private StringBuilder Builder = new StringBuilder(); // reused in Lex()

	private static ProblemException pex(uint Line, uint Column, string Message)
	{
		return new ProblemException(new Problem(new SourcePos(Line, Column), Message));
	}
	
	private bool EatNewline(char Next)
	{
		if (Next == '\n') {
		}
      else if (Next == '\r') {
			do {
				Next = ReadChar("end-of-line");
			} while (Next == '\r');
			if (Next != '\n') {
				throw pex(Line, Column, "a sequence of carriage return characters cannot be followed by " + StringUtil.ReadableChar(Next));
			}
		}
		else {
			return false;
		}

		Line++;
		Column = 1;
		LastWhitespaceHadEol = true;
		return true;
	}

	public Token Lex()
	{
		LastWhitespaceHadEol = false;

		char Next;

		// Skip over whitespace.
		while (true) {
			int NextI = In.Read();
			if (NextI < 0) return new Token(new SourcePos(Line, Column), Codes.Eof);
			Next = (char) NextI;

			// Regular whitespace
			if (Next == ' ' || Next == '\t') {
				Column++;
			}
			else if (EatNewline(Next)) {
			}
			// Comments.
			else if (Next == '/') {
				Column++;
				Next = ReadChar("comment");
				if (Next == '*') {
					Column++;
					// Block comment.
					while (true) {
						Next = ReadChar("comment");
						if (Next == '*' && In.Peek() == '/') {
							In.Read();
							Column += 2;
							break;
						}
						else if (EatNewline(Next)) {
						}
						else if (Next >= 32 && Next <= 126 || Next == '\t') {
							Column++;
						}
						else {
							throw pex(Line, Column, "unexpected character: " + StringUtil.ReadableChar(Next));
						}
					}
				}
				else if (Next == '/') {
					Column++;
					// Line comment.
					while (true) {
						NextI = In.Peek();
						if (NextI >= 32 && NextI <= 126 || NextI == '\t') {
							In.Read();
							Column++;
						} else {
							break;
						}
					}
				}
				else {
					throw pex(Line, Column-1, "unexpected \"/\"");
				}
			}
			else {
				// Not a whitespace char.  Break out of whitespace-eating loop.
				break;
			}
		}

		if (IsIdentStart(Next)) {
			uint StartColumn = Column;
			Builder.Length = 0;
			while (true) {
				Column++;
				Builder.Append(Next);
				int NextI = In.Peek();
				if (NextI < 0) break;
				Next = (char) NextI;
				if (!IsIdentPart(Next)) break;
				In.Read();
			}
			return new Token.Ident(new SourcePos(Line, StartColumn), Builder.ToString());
		}
		else if (IsDigit(Next)) {
			uint StartColumn = Column;
			BigInteger Value = BigInteger.Zero;
			while (true) {
				Column++;

				Value *= 10;
				Value += (Next - '0');

				int NextI = In.Peek();
				if (NextI < 0) break;
				Next = (char) NextI;
				if (!IsDigit(Next)) break;
				In.Read();
			}
			return new Token.LitInt(new SourcePos(Line, StartColumn), Value);
		}
		else if (Next == '-') {
			uint StartColumn = Column;
			Column++;

			char After = ReadChar("token");

			if (IsDigit(After)) {
				BigInteger Value = BigInteger.Zero;

				Next = After;

				while (true) {
					Value *= 10;
					Value += (Next - '0');
					Column++;

					int NextI = In.Peek();
					if (NextI < 0) break;
					Next = (char) NextI;
					if (!IsDigit(Next)) break;
					In.Read();
				}

				Value = Value * -1;
				return new Token.LitInt(new SourcePos(Line, StartColumn), Value);
			}
			else if (After == '>') {
				Column++;
				return new Token(new SourcePos(Line, StartColumn), Codes.RightArrow);
			}
			else {
				throw pex(Line, StartColumn, "expecting either a negative integer or \"->\"");
			}
		}
		else {
			switch (Next) {
				case '+': case '{': case '}': case ',':
				case '<': case '>': case '[': case ']': case ':': case '=': {
					Token t = new Token(new SourcePos(Line, Column), Next);
					Column++;
					return t;
				}

				case '"': {
					uint StartColumn = Column;
					Builder.Length = 0;
					Column++;
					while (true) {
						Next = ReadChar("a string literal");
						if (Next == '"') {
							Column++;
							break;
						}
						else if (Next == '\\') {
							uint EscapeStart = Column;
							Column++;
							Next = ReadChar("a string literal");
							switch (Next) {
								case '"': case '\\': case '\'': Builder.Append(Next); break;
								case 'n': Builder.Append('\n'); break;
								case 't': Builder.Append('\t'); break;
								case 'r': Builder.Append('\r'); break;
								case '0': Builder.Append('\0'); break;
								case 'x': {
									int Value = 0;
									for (uint j = 0; j < 2; j++) {
										Column++;
										Next = ReadChar("a string literal");
										int d = HexValue(Next);
										if (d < 0) throw pex(Line, EscapeStart+1+j, "in \"\\xHH\" escape sequence: expecting hex digit, found " + StringUtil.ReadableChar(Next));
										Value <<= 4;
										Value |= d;
									}
									Builder.Append((char) Value);
									break;
								}
								case '+': {
									// "u+"
									int Value = 0;
									for (uint j = 0; j < 6; j++) {
										Column++;
										Next = ReadChar("a string literal");
										int d = HexValue(Next);
										if (d < 0) throw pex(Line, EscapeStart+1+j, "in \"\\u+HHHHHH\" escape sequence: expecting hex digit, found " + StringUtil.ReadableChar(Next));
										Value <<= 4;
										Value |= d;
									}
									if (Value <= 0xFFFF) {
										if (Value >= 0xD800 && Value <= 0xDFFF) {
											throw pex(Line, EscapeStart, "escape sequence refers to a UTF-16 surrogate, which isn't a valid code point");
										}
										Builder.Append((char) Value);
									}
									else if (Value <= 0x10FFFF) {
										// Encode as surrogate pair.
										Value -= 0x10000;
										char HighSurrogate = (char) (0xD800 + (Value >> 10));
										char LowSurrogate = (char) (0xDC00 + (Value & 0x3FF));
										Builder.Append(HighSurrogate);
										Builder.Append(LowSurrogate);
									}
									else {
										throw pex(Line, EscapeStart, "escape sequence refers to a code point that is past the Unicode range");
									}
									break;
								}
								case 'u': {
									// "u"
									int Value = 0;
									for (uint j = 0; j < 4; j++) {
										Column++;
										Next = ReadChar("a string literal");
										int d = HexValue(Next);
										if (d < 0) throw pex(Line, EscapeStart+1+j, "in \"uHHHH\" escape sequence: expecting hex digit, found " + StringUtil.ReadableChar(Next));
										Value <<= 4;
										Value |= d;
									}
									if (Value >= 0xD800 && Value <= 0xDFFF) {
										throw pex(Line, EscapeStart, "escape sequence refers to a UTF-16 surrogate, which isn't a valid code point");
									}
									Builder.Append((char) Value);
									break;
								}

								default:
									throw pex(Line, EscapeStart, "invalid escape sequence character: " + StringUtil.ReadableChar(Next));
							}
							Column++;
						}
						else if (Next >= 32 && Next <= 126) {
							Column++;
							Builder.Append(Next);
						}
						else {
							throw pex(Line, Column, "invalid character in string literal: " + StringUtil.ReadableChar(Next));
						}
					}
					return new Token.LitString(new SourcePos(Line, StartColumn), Builder.ToString());
				}

				default:
					throw pex(Line, Column, "unexpected character: " + StringUtil.ReadableChar(Next));
			}
		}
	}

	private char ReadChar(string place)
	{
		int i = In.Read();
		if (i < 0) {
			throw pex(Line, Column, "encountered end-of-file in " + place);
		}
		return (char) i;
	}

	private static bool IsIdentStart(char c)
	{
		if (c >= 'A' && c <= 'Z') return true;
		if (c >= 'a' && c <= 'z') return true;
		return false;
	}

	private static bool IsIdentPart(char c)
	{
		return IsIdentStart(c) || (c >= '0' && c <= '9') || c == '_';
	}

	private static bool IsDigit(char c)
	{
		return (c >= '0' && c <= '9');
	}
	
	private static int HexValue(char c)
	{
		if (c >= '0' && c <= '9') {
			return c - '0';
		}
		else if (c >= 'a' && c <= 'f') {
			return c - 'a' + 10;
		}
		else if (c >= 'A' && c <= 'F') {
			return c - 'A' + 10;
		}
		else {
			return -1;
		}
	}

}

} // namespace
