package cks.surface.model;

import cks.io.SourcePos;
import java.io.PrintWriter;

public abstract class Node
{
	public SourcePos loc;

	public Node(SourcePos loc)
	{
		assert(loc != null);
		this.loc = loc;
	}

	/**
	 * Debug dump of the node tree.
	 */
	public void dump(PrintWriter out)
	{
		dump(out, 0);
	}

	/**
	 * Debug dump of the node tree at a given depth.
	 */
	public void dump(PrintWriter out, int depth)
	{
		indent(out, depth);
		out.println(this.toString());
		dumpChildren(out, depth+1);
	}

	protected final void indent(PrintWriter out, int depth)
	{
		while (depth > 0) {
			out.print("	");
			depth--;
		}
	}

	public String toString()
	{
		StringBuilder b = new StringBuilder();
		toString(b);
		return b.toString();
	}

	public void toString(StringBuilder b)
	{
		b.append(this.getClassBaseName());
	}

	/**
	 * Invoke dump() on all your child nodes.
	 */
	protected void dumpChildren(PrintWriter out, int depth) {}

	/**
	 * Get's the class name (minus the package name).
	 */
	public final String getClassBaseName()
	{
		String name = getClass().getName();

		int dot = name.lastIndexOf('.');
		if (dot >= 0) {
			name = name.substring(dot + 1);
		}

	 name = name.replace('$', '.');

		return name;
	}

}

