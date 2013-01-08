package cks.io;

public class SourcePos
{
	public final int line;
	public final int column;

	private static final char LINE_COL_SEP = '.';

	public SourcePos(int line, int column)
	{
		assert line > 0 : "bad line: " + line;
		assert column > 0 : "bad column: " + column;
		this.line = line;
		this.column = column;
	}

	public void toString(StringBuilder builder)
	{
		builder.append(this.line);
		builder.append(LINE_COL_SEP);
		builder.append(this.column);
	}

	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		toString(builder);
		return builder.toString();
	}

	/**
	 * Shows this location relative to another location.	If the two
	 * locations are in different files, this function behaves the
	 * same as toString() does.	If both locations are in the same file,
	 * only the line and column are shown.	If both locations are in
	 * the same file and on the same line, only the column numbers
	 * are shown.	If both locations are exactly equal, the column number
	 * is still shown.
	 */
	public String toStringRelative(SourcePos other)
	{
		StringBuilder builder = new StringBuilder();
		toStringRelative(builder, other);
		return builder.toString();
	}

	public void toStringRelative(StringBuilder builder, SourcePos other)
	{
		if (this.line == other.line) {
			// "col <column>"
			builder.append("col ");
			builder.append(this.column);
		} else {
			// "<line>.<column>"
			builder.append(this.line);
			builder.append(LINE_COL_SEP);
			builder.append(this.column);
		}
	}

}
