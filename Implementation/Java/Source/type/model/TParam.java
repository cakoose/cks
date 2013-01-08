package cks.type.model;

import cks.io.SourcePos;

public class TParam extends TBinding
{
	public final SourcePos loc;
	public final TKind kind;

	public TParam(SourcePos loc, String name, TKind kind)
	{
		super(name);
		this.loc = loc;
		this.kind = kind;
	}

	public TKind getKind() { return kind; }
}
