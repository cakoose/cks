package cks.surface.model;

import cks.io.SourcePos;

import java.io.PrintWriter;
import java.util.Map;

public class VRecord extends Value
{
	public final Map<String,VEntry> fields;

	public VRecord(SourcePos loc, Map<String,VEntry> fields)
	{
		super(loc);
		this.fields = fields;
	}

	protected void dumpChildren(PrintWriter out, int depth)
	{
		for (VEntry field : fields.values()) {
			field.dump(out, depth);
		}

		super.dumpChildren(out, depth);
	}
}
