package cks.io;

import java.util.ArrayList;

public class Codes
{
	public static final class Entry
	{
		public final int code;
		public final String name;

		private Entry(int code, String name)
		{
			this.code = code;
			this.name = name;
		}
	}

	public static final int FirstCode = 128;
	public static final int LastCode;
	private static final ArrayList<Entry> EntryList = new ArrayList<Entry>();
	public static final Entry[] Entries;

	private static int mk(String name)
	{
		int code = FirstCode + EntryList.size();
		EntryList.add(new Entry(code, name));
		return code;
	}

	// token code constants
	public static final int LitInt = mk("integer literal");
	public static final int LitReal = mk("real number literal");
	public static final int LitString = mk("string literal");
	public static final int Ident = mk("identifier");
	public static final int RightArrow = mk("->");

	public static final int CloseTagStart = mk("\"</\"");
	public static final int LeafTagEnd = mk("\"/>\"");
	public static final int Text = mk("text");
	
	public static final int Eof = mk("end of file");

	static {
		LastCode = FirstCode + EntryList.size();
		Entries = EntryList.toArray(new Entry[EntryList.size()]);
	}
}
