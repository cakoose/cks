package cks.io;

import java.io.IOException;
import java.math.BigInteger;

public abstract class Formatter
{
	public abstract void beginRecord() throws IOException;
	public abstract void preField(String name) throws IOException;
	public abstract void endRecord() throws IOException;

	public abstract void voidVariant(String tag) throws IOException;
	public abstract void beginVariant(String tag) throws IOException;
	public abstract void endVariant() throws IOException;

	public abstract void beginList() throws IOException;
	public abstract void preListElement() throws IOException;
	public abstract void endList() throws IOException;

	public abstract void beginSet() throws IOException;
	public abstract void preSetElement() throws IOException;
	public abstract void endSet() throws IOException;

	public abstract void beginMap() throws IOException;
	public abstract void preMapKey() throws IOException;
	public abstract void preMapValue() throws IOException;
	public abstract void endMap() throws IOException;

	public abstract void writeMaybeNone() throws IOException;
	public abstract void beginMaybeSet() throws IOException;
	public abstract void endMaybeSet() throws IOException;

	public abstract void writeInt32(int value) throws IOException;
	public abstract void writeInt64(long value) throws IOException;
	public abstract void writeInt(BigInteger value) throws IOException;

	public abstract void writeString(String value) throws IOException;
	public abstract void writeBool(boolean value) throws IOException;
	public abstract void writeVoid() throws IOException;
}
