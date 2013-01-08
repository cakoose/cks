package cks.dynamic;

import cks.type.model.TRecord;
import cks.io.Formatter;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;

public final class GRecord extends GValue
{
	public final TRecord type;
	public final GValue[] values;

	public GRecord(TRecord type, GValue[] values)
	{
		assert type.allMembers.size() == values.length;
		this.type = type;
		this.values = values;
	}

	public void writeBinary(OutputStream out)
		throws IOException
	{
		for (GValue value : values) {
			value.writeBinary(out);
		}
	}

	public void write(Formatter out)
		throws IOException
	{
		out.beginRecord();
		int i = 0;
		for (String fieldName: type.allMembers.keySet()) {
			out.preField(fieldName);
			values[i].write(out);
			i++;
		}
		out.endRecord();
	}

	public boolean equals(Object o) { return o instanceof GRecord && equals((GRecord)o); }
	public boolean equals(GRecord o)
	{
		if (type != o.type) return false;
		assert values.length == o.values.length;
		return Arrays.equals(values, o.values);
	}

	public int hashCode()
	{
		int h = type.hashCode();
		for (GValue value : values) {
			h <<= 1;
			h ^= value.hashCode();
		}
		return h;
	}
}
