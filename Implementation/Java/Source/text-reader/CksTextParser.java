package cks.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigInteger;

public final class CksTextParser extends Parser
{
	protected final CksTextTokenizer in;
	protected final Token next;

	public CksTextParser(InputStream in)
		throws IOException, ProblemException
	{
		// Don't use a buffered reader because we don't necessarily own the whole stream.
		this(StringUtil.utf8Reader(in));
	}

	public CksTextParser(Reader reader)
		throws IOException, ProblemException
	{
		this.in = new CksTextTokenizer(reader);
		this.next = in.peek();
	}

	private boolean firstEntry = false;
	private Token lastIdent;

    @Override
	public SourcePos getLastIdentSourcePos() { return lastIdent.loc; }

	@Override
	public SourcePos beginRecord() throws ProblemException, IOException
	{
		Token t = in.expect('{');
		firstEntry = true;
        return t.loc;
	}

	@Override
	public String getField() throws ProblemException, IOException
	{
		Token t = in.take();
		this.lastIdent = t;

		if (!firstEntry) {
			if (t.type == ',') {
				t = in.take();
			}
			else if (t.type == '}') {
				return null;
			}
			else if (in.lastWhitespaceHadEol()) {
				// EOL is a good enough separator
			}
			else {
				throw ProblemException.pex(t.loc, "expecting '}' or ',' or end-of-line before next field");
			}
		}
		firstEntry = false;

		if (t.type == '}') return null;

		CksTextTokenizer.expect(Codes.Ident, t);
		Token.Ident id = (Token.Ident) t;
		lastIdent = id;
		String fieldName = id.text;

		in.expect('=');

		return fieldName;
	}

	private boolean hasVariantValue = false;

	@Override
	public String beginVariant() throws ProblemException, IOException
	{
		Token.Ident id = in.expectIdent();
		lastIdent = id;

		if (in.peek().type == ':') {
			in.next();
			hasVariantValue = true;
		} else {
			hasVariantValue = false;
		}
		return id.text;
	}

	@Override
	public boolean hasVariantValue() throws ProblemException, IOException
	{
		return hasVariantValue;
	}

	@Override
	public void endVariant() throws ProblemException, IOException
	{
	}

	@Override
	public void beginList() throws ProblemException, IOException
	{
		in.expect('[');
		firstEntry = true;
	}

	@Override
	public SourcePos hasListNext() throws ProblemException, IOException
	{
		Token t = in.peek();

		if (!firstEntry) {
			if (t.type == ',') {
				in.next();
				t = in.peek();
			}
			else if (t.type == ']') {
				in.next();
				return null;
			}
			else if (in.lastWhitespaceHadEol()) {
				// EOL is a good enough separator
				return t.loc;
			}
			else {
				throw ProblemException.pex(t.loc, "expecting ']' or ',' or end-of-line before next list element");
			}
		}
		firstEntry = false;

		if (t.type == ']') {
			in.next();
			return null;
		}

		return t.loc;
	}

	@Override
	public void beginSet() throws ProblemException, IOException
	{
		beginList();
	}

	@Override
	public SourcePos hasSetNext() throws ProblemException, IOException
	{
		return hasListNext();
	}

	@Override
	public void beginMap() throws ProblemException, IOException
	{
        in.expect('[');
        firstEntry = true;
	}

	@Override
	public SourcePos hasMapNext() throws ProblemException, IOException
	{
        Token t = in.peek();

        if (!firstEntry) {
            if (t.type == ',') {
                in.next();
                t = in.peek();
            }
            else if (t.type == ']') {
                in.next();
                return null;
            }
            else if (in.lastWhitespaceHadEol()) {
                // EOL is a good enough separator
                return t.loc;
            }
            else {
                throw ProblemException.pex(t.loc, "expecting ']' or ',' or end-of-line before next map entry");
            }
        }
        firstEntry = false;

        if (t.type == '}') {
            in.next();
            return null;
        }

        return t.loc;
	}

	@Override
	public SourcePos preMapValue() throws ProblemException, IOException
	{
        Token t = in.peek();
        if (t.type == Codes.RightArrow) {
            in.next();
            t = in.peek();
            return t.loc;
        } else {
            return null;
        }
	}

	public BigInteger parseInt() throws ProblemException, IOException
	{
		Token.LitInt t = (Token.LitInt) in.expect(Codes.LitInt);
		return t.value;
	}

	public BigInteger parseNat() throws ProblemException, IOException
	{
		Token.LitInt t = (Token.LitInt) in.expect(Codes.LitInt);
		BigInteger v = t.value;
		if (v.signum() == -1) {
			throw ProblemException.pex(t.loc, "expecting natural number, found negative integer");
		}
		return v;
	}

	private BigInteger parseIntBits(int numBits) throws ProblemException, IOException
	{
		Token.LitInt t = (Token.LitInt) in.expect(Codes.LitInt);
		BigInteger v = t.value;
		if (v.bitLength() >= numBits) {
			throw ProblemException.pex(t.loc, "value out of range; expecting " + numBits + "-bit 2's complement integer");
		}
		return v;
	}

	@Override
	public byte parseInt8() throws ProblemException, IOException
	{
		return parseIntBits(8).byteValue();
	}

	@Override
	public short parseInt16() throws ProblemException, IOException
	{
		return parseIntBits(16).shortValue();
	}

	@Override
	public int parseInt32() throws ProblemException, IOException
	{
		return parseIntBits(32).intValue();
	}

	@Override
	public long parseInt64() throws ProblemException, IOException
	{
		return parseIntBits(64).longValue();
	}

	private BigInteger parseNatBits(int numBits) throws ProblemException, IOException
	{
		Token.LitInt t = (Token.LitInt) in.expect(Codes.LitInt);
		BigInteger v = t.value;
		if (v.signum() == -1) {
			throw ProblemException.pex(t.loc, "expecting " + numBits + "-bit natural number, found negative integer");
		}
		if (v.bitLength() > numBits) {
			throw ProblemException.pex(t.loc, "value too large; expecting " + numBits + "-bit natural number");
		}
		return v;
	}

	@Override
	public short parseNat8() throws ProblemException, IOException
	{
		return parseNatBits(8).shortValue();
	}

	@Override
	public int parseNat16() throws ProblemException, IOException
	{
		return parseNatBits(16).intValue();
	}

	@Override
	public long parseNat32() throws ProblemException, IOException
	{
		return parseNatBits(32).longValue();
	}

	@Override
	public BigInteger parseNat64() throws ProblemException, IOException
	{
		return parseNatBits(64);
	}

	@Override
	public String parseString() throws ProblemException, IOException
	{
		Token.LitString t = (Token.LitString) in.expect(Codes.LitString);
		return t.value;
	}

	@Override
	public boolean parseBool() throws ProblemException, IOException
	{
		String option = beginVariant();

		boolean val;
		if (option.equals("True")) {
			val = true;
		}
		else if (option.equals("False")) {
			val = false;
		}
		else {
			throw ProblemException.pex(getLastIdentSourcePos(), "expecting a Bool value; either \"True\" or \"False\"");
		}

		if (hasVariantValue()) {
			parseVoid();
		}

		return val;
	}

	@Override
	public void parseVoid() throws ProblemException, IOException
	{
		in.expect('(');
		in.expect(')');
	}

	@Override
	public Boolean beginMaybe() throws ProblemException, IOException
	{
		String option = beginVariant();

		if (option.equals("Set")) {
			return hasVariantValue();
		}
		else if (option.equals("None")) {
			if (hasVariantValue()) {
				parseVoid();
			}
			return null;
		}
		else {
			throw ProblemException.pex(getLastIdentSourcePos(), "expecting a Maybe value; either \"Set\" or \"None\"");
		}
	}

	@Override
	public void endMaybe() throws ProblemException, IOException
	{
	}

    @Override
    public void endInput() throws ProblemException, IOException
    {
        in.expect(Codes.Eof);
    }
}
