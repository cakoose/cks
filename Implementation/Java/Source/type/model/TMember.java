package cks.type.model;

import cks.io.SourcePos;

public class TMember
{
	public final SourcePos loc;
	public final String name;
	public final TExpr type;

	public TMember(SourcePos loc, String name, TExpr type)
	{
		assert loc != null;
		assert name != null;
		assert type != null;
		this.loc = loc;
		this.name = name;
		this.type = type;
	}

	private int index = -1;
	public int getIndex() { return index; }
	public void setIndex(int index) { this.index = index; }
}
