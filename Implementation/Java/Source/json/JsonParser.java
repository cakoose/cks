package cks.io;

import com.fasterxml.jackson.core.JsonFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigInteger;

public class JsonParser extends Parser
{
    private static final JsonFactory DefaultFactory = new JsonFactory();

    private final com.fasterxml.jackson.core.JsonParser parser;

    public JsonParser(com.fasterxml.jackson.core.JsonParser parser)
        throws IOException, ProblemException
    {
        this.parser = parser;
    }

    public JsonParser(InputStream in)
        throws IOException, ProblemException
    {
        this(DefaultFactory, in);
    }

    public JsonParser(Reader in)
        throws IOException, ProblemException
    {
        this(DefaultFactory, in);
    }

    public JsonParser(byte[] data)
        throws IOException, ProblemException
    {
        this(DefaultFactory, data);
    }

    public JsonParser(JsonFactory factory, InputStream in)
        throws IOException, ProblemException
    {
        this(factory.createParser(in));
    }

    public JsonParser(JsonFactory factory, Reader in)
        throws IOException, ProblemException
    {
        this(factory.createParser(in));
    }

    public JsonParser(JsonFactory factory, byte[] data)
        throws IOException, ProblemException
    {
        this(factory.createParser(data));
    }

    @Override
    public SourcePos getLastIdentSourcePos()
    {
        throw new AssertionError("todo");
    }

    @Override
    public void endInput() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public SourcePos beginRecord() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public String getField() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public String beginVariant() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public boolean hasVariantValue() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public void endVariant() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public void beginList() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public SourcePos hasListNext() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public void beginSet() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public SourcePos hasSetNext() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public void beginMap() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public SourcePos hasMapNext() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public SourcePos preMapValue() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public BigInteger parseInt() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public BigInteger parseNat() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public byte parseInt8() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public short parseInt16() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public int parseInt32() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public long parseInt64() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public short parseNat8() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public int parseNat16() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public long parseNat32() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public BigInteger parseNat64() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public String parseString() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public boolean parseBool() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public void parseVoid() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public Boolean beginMaybe() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }

    @Override
    public void endMaybe() throws ProblemException, IOException
    {
        throw new AssertionError("todo");
    }
}
