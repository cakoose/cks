package cks.surface.model;

import cks.io.SourcePos;
import cks.io.StringUtil;
import cks.io.Token;

import java.io.PrintWriter;
import java.util.List;
import java.util.Map;

public class VTag
{
	public static abstract class Tag extends Value implements Child
	{
		protected Tag(SourcePos loc) { super(loc); }
		public Node getNode() { return this; }
	}

	// The tag name, linked to the tag body.
	public static final class Full extends Tag
	{
		public final String name; // Might be null.
		public final Tag rest;
		
		public Full(SourcePos loc, String name, Tag rest)
		{
			super(loc);
			assert rest != null;
			this.name = name;
			this.rest = rest;
		}

		protected void dumpChildren(PrintWriter out, int depth)
		{
			rest.dump(out, depth);
			super.dumpChildren(out, depth);
		}

		public void toString(StringBuilder b)
		{
			super.toString(b);
			b.append(": ");
			b.append(this.name == null ? "-" : this.name);
		}

		public final Node getNode() { return this; }
	}

	// The tag body (everything but the tag name)
	public static final class Body extends Tag
	{
		public final Map<String,VEntry> attrs;
		public final Content content; // might be null

		public Body(SourcePos loc, Map<String,VEntry> attrs, Content content)
		{
			super(loc);
			assert attrs != null;
			assert content != null;
			this.attrs = attrs;
			this.content = content;
		}

		public void dumpChildren(PrintWriter out, int depth)
		{
			for (VEntry attr : attrs.values()) {
				attr.dump(out, depth);
			}
			if (content != null) content.dump(out, depth);
		}
	}

	public static final class Content extends Value
	{
		public final List<Child> children;

		public Content(SourcePos loc, List<Child> children)
		{
			super(loc);
			this.children = children;
		}

		public void dumpChildren(PrintWriter out, int depth)
		{
			for (Child child : children) {
				child.getNode().dump(out, depth);
			}
		}
	}

	public static final class Text extends Node implements Child
	{
		public final String value;
		public Text(SourcePos loc, String value)
		{
			super(loc);
			assert value != null;
			this.value = value;
		}

		public void toString(StringBuilder b)
		{
			super.toString(b);
			b.append(" \"");
			Token.LitString.escapedString(b, value, '"');
			b.append('"');
		}

		public final Node getNode() { return this; }
	}

	public interface Child
	{
		public Node getNode();
	}
}
