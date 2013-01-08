package cks.type.model;

import java.util.Map;

public class TModule
{
	public final Map<String,TDef> defs;  // Top-level defs.
	public final int totalNumDefs; // Total number of defs (including nested ones) in the entire module.

	public Boolean resolveResult = null;

	public TModule(Map<String, TDef> defs, int totalNumDefs)
	{
		this.defs = defs;
		this.totalNumDefs = totalNumDefs;
	}
}
