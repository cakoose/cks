package cks.dynamic;

import cks.io.Formatter;

import java.io.OutputStream;
import java.io.IOException;

public abstract class GValue
{
	public abstract void writeBinary(OutputStream out) throws java.io.IOException;
	public abstract void write(Formatter out) throws IOException;
}
