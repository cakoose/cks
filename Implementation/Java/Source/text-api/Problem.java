package cks.io;

import java.util.List;
import java.util.Collections;

public final class Problem
{
	public final Ref primary;
	public final List<Ref> additional;

	public Problem(Ref primary, List<Ref> additional)
	{
		assert primary != null;
		assert additional != null;
		this.primary = primary;
		this.additional = additional;
	}

	public Problem(Ref primary)
	{
		this(primary, Collections.<Ref>emptyList());
	}

	public Problem(SourcePos sourcePos, String message)
	{
		this(new Ref(sourcePos, message), Collections.<Ref>emptyList());
	}

	public Problem(SourcePos sourcePos, String message, SourcePos additionalSourcePos, String additionalMessage)
	{
		this(new Ref(sourcePos, message), Collections.singletonList(new Ref(additionalSourcePos, additionalMessage)));
	}

	public String toString()
	{
		StringBuilder buf = new StringBuilder();
		toString(buf);
		return buf.toString();
	}

	public static final class Ref
	{
		public final SourcePos sourcePos;
		public final String message;

		public Ref(SourcePos sourcePos, String message)
		{
			this.sourcePos = sourcePos;
			this.message = message;
		}
	}

	public void toString(StringBuilder buf)
	{
		// Primary location and message
		primary.sourcePos.toString(buf);
		buf.append(": ");
		buf.append(primary.message);

		// Additional messages.
		for (Ref ref : additional) {
			buf.append("; ");
			buf.append(ref.message);
			buf.append(" at [");
			ref.sourcePos.toStringRelative(buf, primary.sourcePos);
			buf.append("]");
		}
	}
}
