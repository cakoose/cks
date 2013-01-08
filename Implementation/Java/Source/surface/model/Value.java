package cks.surface.model;

import cks.io.SourcePos;

public abstract class Value extends Node
{
	public Value(SourcePos loc)
	{
		super(loc);
	}
}
