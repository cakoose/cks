using StringBuilder = System.Text.StringBuilder;

namespace Cks.Text.Reader.Parser {

class StringUtil
{
	public static string ReadableChar(char c)
	{
		StringBuilder buf = new StringBuilder();
		ReadableChar(buf, c);
		return buf.ToString();
	}

	public static void ReadableChar(StringBuilder buf, char c)
	{
		if (c == '"' || c == '\'') {
			// For quotes, surround with brackets.
			buf.Append('[');
			buf.Append(c);
			buf.Append(']');
		}
		else if (c >= 32 && c <= 126) {
			buf.Append('"');
			buf.Append(c);
			buf.Append('"');
		}
		else {
			buf.Append("0x");
			Hex(buf, c);

			// Add an aditional.
			int pos = FriendlyChars.IndexOf(c);
			if (pos >= 0) {
				// Lookup special character name.
				buf.Append(" (");
				buf.Append(FriendlyCharNames[pos]);
				buf.Append(')');
			}
		}
	}

	private static readonly string FriendlyChars = ""
		+ (char)0x08 // Backspace
		+ (char)0x09 // Tab
		+ (char)0x0a // NewLine
		+ (char)0x0b // VerticalTab
		+ (char)0x0c // FormFeed
		+ (char)0x0d // CarriageReturn
		+ (char)0x1b // Escape
		+ (char)0x7f // Delete
		;
	private static readonly string[] FriendlyCharNames = {
		"Backspace", "Tab", "NewLine", "VerticalTab", "FormFeed",
		"CarriageReturn", "Escape", "Delete",
	};

	// -----------------------------------------------------------
	
	public static readonly string HexDigits = "0123456789abcdef";

	// Append between 1-4 hex digits (omit leading zeroes)
	public static void Hex(StringBuilder buf, char c)
	{
		uint i = c;
		int h1 = (int) (i >> 12);
		int h2 = (int) (i >>  8) & 0xf;
		int h3 = (int) (i >>  4) & 0xf;
		int h4 = (int) (i >>  0) & 0xf;

		// Avoid appending leading zeros.
		if (h1 == 0) {
			if (h2 == 0) {
				if (h3 == 0) {
					goto l3;
				}
				goto l2;
			}
			goto l1;
		}

		buf.Append(HexDigits[h1]);
		l1:
		buf.Append(HexDigits[h2]);
		l2:
		buf.Append(HexDigits[h3]);
		l3:
		buf.Append(HexDigits[h4]);
	}

}

}
