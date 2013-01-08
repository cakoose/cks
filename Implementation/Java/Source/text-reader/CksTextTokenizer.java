package cks.io;

import static cks.io.ProblemException.pex;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigInteger;
import java.nio.charset.Charset;

public final class CksTextTokenizer
{
	private final Reader in;

	private static final Charset UTF8 = Charset.forName("UTF-8");

	public CksTextTokenizer(InputStream in) throws IOException
	{
		this(StringUtil.utf8Reader(in));
	}

	public CksTextTokenizer(Reader in) throws IOException
	{
		this(in, 1, 1);
	}

	public CksTextTokenizer(Reader in, int line, int column) throws IOException
	{
		if (line < 1) throw new IllegalArgumentException("'line' cannot be less than 1 (line = " + line + ")");
		if (column < 1) throw new IllegalArgumentException("'column' cannot be less than 1 (column = " + column + ")");

		this.in = in;
		this.peekedChar = in.read();

		this.line = line;
		this.column = column;
	}

	public boolean lastWhitespaceHadEol()
	{
		return lastWhitespaceHadEol;
	}

	private int line, column;
	private boolean lastWhitespaceHadEol;
	private Token peek;

	public Token take() throws ProblemException, IOException
	{
		Token t;
		if (peek == null) {
			t = lex();
		} else {
			t = peek;
			peek = null;
		}
		return t;
	}

	public void next() throws ProblemException, IOException
	{
		assert peek != null : "called next() without looking at token: " + peek;
		peek = lex();
	}

	public Token peek() throws ProblemException, IOException
	{
		if (peek == null) peek = lex();
		return peek;
	}

	private StringBuilder builder = new StringBuilder();

	public Token.Text takeTagContent()
		throws ProblemException, IOException
	{
		assert peek == null : peek;
		if (peekedChar == '<') return null;

		int startColumn = column;
		builder.setLength(0);
		column++;
		while (true) {
			char next = readChar("tag content");
			if (next == '\\') {
				parseEscape();
			}
			else if (next >= 32 && next <= 126 || next == '\t') {
				column++;
				builder.append(next);
			}
			else if (next == '\n') {
				line++;
				column = 1;
				builder.append('\n');
			}
			else if (next == '\r') {
				do {
					next = readChar("end-of-line");
				} while (next == '\r');
				if (next != '\n') {
					throw ex(line, column, "a sequence of carriage return characters cannot be followed by " + StringUtil.readableChar(next));
				}
				line++;
				column = 1;
				builder.append('\n');
			}
			else {
				throw ex(line, column, "invalid character in tag content: " + StringUtil.readableChar(next));
			}
			if (peekedChar == '<') {
				break;
			}
			else if (peekedChar == '>') {
				throw ex(line, column, "invalid character in tag content: " + StringUtil.readableChar('>'));
			}
		}
		return new Token.Text(new SourcePos(line, startColumn), builder.toString());
	}

	private Token lex()
		throws ProblemException, IOException
	{
		lastWhitespaceHadEol = false;

		char next;

		// Skip over whitespace.
		while (true) {
			int nextI = readStream();
			if (nextI < 0) return new Token(new SourcePos(line, column), Codes.Eof);
			next = (char) nextI;

			// Regular whitespace
			if (next == ' ' || next == '\t') {
				column++;
			}
			else if (eatNewline(next)) {
			}
			// Comments.
			else if (next == '/') {
				column++;
				if (peekStream() == '*') {
                    readStream();
					column++;
					// Block comment.
					while (true) {
						next = readChar("block comment");
						if (next == '*' && peekStream() == '/') {
							readStream();
							column += 2;
							break;
						}
						else if (eatNewline(next)) {
						}
						else if (next >= 32 && next <= 126 || next == '\t') {
							column++;
						}
						else {
							throw ex(line, column, "unexpected character: " + StringUtil.readableChar(next));
						}
					}
				}
				else if (peekStream() == '/') {
                    readStream();
					column++;
					// line comment.
					while (true) {
						nextI = peekStream();
						if (nextI >= 32 && nextI <= 126 || nextI == '\t') {
							readStream();
							column++;
						} else {
							break;
						}
					}
				}
				else {
					return new Token(new SourcePos(line, column-1), '/');
				}
			}
			else {
				// Not a whitespace char.  Break out of whitespace-eating loop.
				break;
			}
		}

		if (isIdentStart(next)) {
			int startColumn = column;
			builder.setLength(0);
			while (true) {
				column++;
				builder.append(next);
				int nextI = peekStream();
				if (nextI < 0) break;
				next = (char) nextI;
				if (!isIdentPart(next)) break;
				readStream();
			}
			return new Token.Ident(new SourcePos(line, startColumn), builder.toString());
		}
		else if (isDigit(next)) {
			int startColumn = column;
			builder.setLength(0);

			while (true) {
				column++;

				builder.append(next);

				int nextI = peekStream();
				if (nextI < 0) break;
				next = (char) nextI;
				if (!isDigit(next)) break;
				readStream();
			}
			return new Token.LitInt(new SourcePos(line, startColumn), new BigInteger(builder.toString(), 10));
		}
		else if (next == '-') {
			int startColumn = column;
			column++;

			char after = readChar("token");

			if (isDigit(after)) {
				builder.setLength(0);
				builder.append(next);

				next = after;

				while (true) {
					builder.append(next);
					column++;

					int nextI = peekStream();
					if (nextI < 0) break;
					next = (char) nextI;
					if (!isDigit(next)) break;
					readStream();
				}

				return new Token.LitInt(new SourcePos(line, startColumn), new BigInteger(builder.toString(), 10));
			}
			else if (after == '>') {
				column++;
				return new Token(new SourcePos(line, startColumn), Codes.RightArrow);
			}
			else {
				throw ex(line, column, "expecting either a negative integer or \"->\"");
			}
		}
		else {
			switch (next) {
				case '+': case '{': case '}': case ',': case '(': case ')': case '*':
				case '<': case '>': case '[': case ']': case ':': case '=': case '?': {
					Token t = new Token(new SourcePos(line, column), next);
					column++;
					return t;
				}
				case '"': {
					int startColumn = column;
					builder.setLength(0);
					column++;
					while (true) {
						next = readChar("a string literal");
						if (next == '"') {
							column++;
							break;
						}
						else if (next == '\\') {
							parseEscape();
						}
						else if (next >= 32 && next <= 126) {
							column++;
							builder.append(next);
						}
						else {
							throw ex(line, column, "invalid character in string literal: " + StringUtil.readableChar(next));
						}
					}
					return new Token.LitString(new SourcePos(line, startColumn), builder.toString());
				}

				default:
					throw ex(line, column, "unexpected character: " + StringUtil.readableChar(next));
			}
		}
	}

	private void parseEscape()
		throws ProblemException, IOException
	{
		char next;
		int EscapeStart = column;
		column++;
		next = readChar("a string literal");
		switch (next) {
			case '"': case '\\': case '\'': builder.append(next); break;
			case 'n': builder.append('\n'); break;
			case 't': builder.append('\t'); break;
			case 'r': builder.append('\r'); break;
			case '0': builder.append('\0'); break;
			case 'x': {
				int value = 0;
				for (int j = 0; j < 2; j++) {
					column++;
					next = readChar("a string literal");
					int d = hexValue(next);
					if (d < 0) throw ex(line, EscapeStart+1+j, "in \"\\xHH\" escape sequence: expecting hex digit, found " + StringUtil.readableChar(next));
					value <<= 4;
					value |= d;
				}
				builder.append((char) value);
				break;
			}
			case '+': {
				int value = 0;
				for (int j = 0; j < 6; j++) {
					column++;
					next = readChar("a string literal");
					int d = hexValue(next);
					if (d < 0) throw ex(line, EscapeStart+1+j, "in \"\\+HHHHHH\" escape sequence: expecting hex digit, found " + StringUtil.readableChar(next));
					value <<= 4;
					value |= d;
				}
				if (value <= 0xFFFF) {
					if (value >= Character.MIN_SURROGATE && value <= Character.MAX_SURROGATE) {
						throw ex(line, EscapeStart, "escape sequence refers to a UTF-16 surrogate, which isn't a valid code point");
					}
					builder.append((char) value);
				}
				else if (value <= 0x10FFFF) {
					// Encode as surrogate pair.
					value -= 0x10000;
					char highSurrogate = (char) (Character.MIN_HIGH_SURROGATE + (value >>> 10));
					char lowSurrogate = (char) (Character.MIN_LOW_SURROGATE + (value & 0x3FF));
					builder.append(highSurrogate);
					builder.append(lowSurrogate);
				}
				else {
					throw ex(line, EscapeStart, "escape sequence refers to a code point that is past the Unicode range");
				}
				break;
			}
			case 'u': {
				// "u"
				int value = 0;
				for (int j = 0; j < 4; j++) {
					column++;
					next = readChar("a string literal");
					int d = hexValue(next);
					if (d < 0) throw ex(line, EscapeStart+1+j, "in \"uHHHH\" escape sequence: expecting hex digit, found " + StringUtil.readableChar(next));
					value <<= 4;
					value |= d;
				}
				if (value >= Character.MIN_SURROGATE && value <= Character.MAX_SURROGATE) {
					throw ex(line, EscapeStart, "escape sequence refers to a UTF-16 surrogate, which isn't a valid code point");
				}
				builder.append((char) value);
				break;
			}
			default:
				throw ex(line, EscapeStart, "invalid escape sequence character: " + StringUtil.readableChar(next));
		}
		column++;
	}

	private static int hexValue(char c)
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

	private int peekedChar;

	private int peekStream()
	{
		return peekedChar;
	}

	private int readStream() throws IOException
	{
		int c = peekedChar;
		peekedChar = in.read();
		return c;
	}

	private char readChar(String place)
		throws ProblemException, IOException
	{
		int i = readStream();
		if (i < 0) {
			throw ex(line, column, "encountered end-of-file in " + place);
		}
		return (char) i;
	}

	private boolean eatNewline(char next)
		throws ProblemException, IOException
	{
		if (next == '\n') {
		}
		else if (next == '\r') {
			do {
				next = readChar("end-of-line");
			} while (next == '\r');
			if (next != '\n') {
				throw ex(line, column, "a sequence of carriage return characters cannot be followed by " + StringUtil.readableChar(next));
			}
		}
		else {
			return false;
		}

		line++;
		column = 1;
		lastWhitespaceHadEol = true;
		return true;
	}

	private ProblemException ex(int line, int column, String message)
	{
		return ProblemException.pex(new SourcePos(line, column), message);
	}

	public Token.Ident expectIdent()
		throws ProblemException, IOException
	{
		Token t = take();
		if (t.type != Codes.Ident) {
			StringBuilder message = new StringBuilder();
			message.append("expecting ");
			Token.toStringContent(message, Codes.Ident);
			message.append(", found ");
			t.toStringContent(message);
			throw pex(t.loc, message.toString());
		}
		return (Token.Ident) t;
	}

	public Token expect(int type)
		throws ProblemException, IOException
	{
		Token t = take();
		expect(type, t);
		return t;
	}

	public static void expect(int type, Token t)
		throws ProblemException
	{
		if (t.type != type) {
			StringBuilder message = new StringBuilder();
			message.append("expecting ");
			Token.toStringContent(message, type);
			message.append(", found ");
			t.toStringContent(message);
			throw pex(t.loc, message.toString());
		}
	}

	private static boolean isIdentStart(char c)
	{
		if (c >= 'A' && c <= 'Z') return true;
		if (c >= 'a' && c <= 'z') return true;
		return false;
	}

	private static boolean isIdentPart(char c)
	{
		return isIdentStart(c) || (c >= '0' && c <= '9') || c == '_';
	}

	private static boolean isDigit(char c)
	{
		return (c >= '0' && c <= '9');
	}
}
