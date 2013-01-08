package cks.surface.model;

import cks.io.SourcePos;

import java.io.PrintWriter;
import java.util.List;

public class VCollection extends Value
{
	public final List<Element> elements;

	public VCollection(SourcePos loc, List<Element> elements)
	{
		super(loc);
		this.elements = elements;
	}

	protected void dumpChildren(PrintWriter out, int depth)
	{
		for (Element e : elements) {
			e.first.dump(out, depth);
			if (e.arrow != null) {
				indent(out, depth+1);
				out.println("->");
				e.second.dump(out, depth+1);
			}
		}
		super.dumpChildren(out, depth);
	}

	public static final class Element
	{
		public final Value first;
		public final SourcePos arrow; // Might be null.
		public final Value second; // Might be null.

		public Element(Value first, SourcePos arrow, Value second)
		{
			assert first != null;
			assert (arrow == null) == (second == null); // Both must either be null or non-null.
			this.first = first;
			this.arrow = arrow;
			this.second = second;
		}
	}
}
