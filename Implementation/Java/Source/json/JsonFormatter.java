package cks.io;

import com.fasterxml.jackson.core.JsonGenerator;

import java.io.IOException;
import java.math.BigInteger;

public class JsonFormatter extends Formatter
{
	private final JsonGenerator gen;

	public JsonFormatter(JsonGenerator gen)
	{
		this.gen = gen;
	}

	@Override
	public void beginRecord() throws IOException
	{
		gen.writeStartObject();
	}

	@Override
	public void preField(String name) throws IOException
	{
		gen.writeFieldName(name);
	}

	@Override
	public void endRecord() throws IOException
	{
		gen.writeEndObject();
	}

	@Override
	public void voidVariant(String tag) throws IOException
	{
		gen.writeString(tag);
	}

	@Override
	public void beginVariant(String tag) throws IOException
	{
		gen.writeStartArray();
		gen.writeString(tag);
	}

	@Override
	public void endVariant() throws IOException
	{
		gen.writeEndArray();
	}

	@Override
	public void beginList() throws IOException
	{
		gen.writeStartArray();
	}

	@Override
	public void preListElement() throws IOException
	{
	}

	@Override
	public void endList() throws IOException
	{
		gen.writeEndArray();
	}

	@Override
	public void beginSet() throws IOException
	{
		gen.writeStartArray();
	}

	@Override
	public void preSetElement() throws IOException
	{
	}

	@Override
	public void endSet() throws IOException
	{
		gen.writeEndArray();
	}

	@Override
	public void beginMap() throws IOException
	{
		gen.writeStartArray();
	}

	@Override
	public void preMapKey() throws IOException
	{
		gen.writeStartArray();
	}

	@Override
	public void preMapValue() throws IOException
	{
	}

	@Override
	public void endMap() throws IOException
	{
		gen.writeEndArray();
	}

	@Override
	public void writeMaybeNone() throws IOException
	{
		gen.writeNull();
	}

	@Override
	public void beginMaybeSet() throws IOException
	{
		gen.writeStartArray();
	}

	@Override
	public void endMaybeSet() throws IOException
	{
		gen.writeEndArray();
	}

	@Override
	public void writeInt32(int value) throws IOException
	{
		gen.writeNumber(value);
	}

	@Override
	public void writeInt64(long value) throws IOException
	{
		gen.writeNumber(value);
	}

	@Override
	public void writeInt(BigInteger value) throws IOException
	{
		gen.writeNumber(value);
	}

	@Override
	public void writeString(String value) throws IOException
	{
		gen.writeString(value);
	}

	@Override
	public void writeBool(boolean value) throws IOException
	{
		gen.writeBoolean(value);
	}

	@Override
	public void writeVoid() throws IOException
	{
		gen.writeNull();
	}
}
