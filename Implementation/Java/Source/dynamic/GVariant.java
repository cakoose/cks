package cks.dynamic;

import cks.type.model.TMember;
import cks.type.model.TOpaque;
import cks.type.model.TVariant;
import cks.io.BinaryWriter;
import cks.io.Formatter;

import java.io.IOException;
import java.io.OutputStream;

public final class GVariant extends GValue
{
	public final TVariant type;
	public final TMember option;
	public final GValue value;

	public GVariant(TVariant type, TMember option, GValue value)
	{
		this.type = type;
		this.option = option;
		this.value = value;
	}

	public void writeBinary(OutputStream out) throws IOException
	{
		int numOptions = type.allMembers.size();
		if (numOptions <= (1 << 8)) {
			BinaryWriter.writeOptionIndex1(out, option.getIndex());
		}
		else if (numOptions <= (1 << 16)) {
			BinaryWriter.writeOptionIndex2(out, option.getIndex());
		}
		else if (numOptions <= (1 << 16)) {
			BinaryWriter.writeOptionIndex3(out, option.getIndex());
		}
		else {
			BinaryWriter.writeOptionIndex4(out, option.getIndex());
		}

		value.writeBinary(out);
	}

	public void write(Formatter out) throws IOException
	{
		if (option.type.base.getTarget() == TOpaque.Void) {
			out.voidVariant(option.name);
		} else {
			out.beginVariant(option.name);
			value.write(out);
			out.endVariant();
		}
	}

	public boolean equals(Object o) { return o instanceof GVariant && equals((GVariant)o); }
	public boolean equals(GVariant o)
	{
		if (option != o.option) return false;
		return value.equals(o.value);
	}

	public int hashCode()
	{
		int h = option.hashCode() ^ value.hashCode();
		return h;
	}
}
