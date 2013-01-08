package cks.surface.model;

import cks.io.SourcePos;

import java.io.PrintWriter;

public class VEntry extends Value
{
	public final String name;
	public final Value value;

	public VEntry(SourcePos loc, String name, Value value)
	{
		super(loc);
		this.name = name;
		this.value = value;
	}

	public void toString(StringBuilder b)
	{
		super.toString(b);
		b.append(": ");
		b.append(this.name);
	}

	protected void dumpChildren(PrintWriter out, int depth)
	{
		this.value.dump(out, depth);
		super.dumpChildren(out, depth);
	}
}
