package cks.dynamic;

import cks.io.*;
import cks.Maybe;
import cks.surface.SurfaceConverter;

import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class GOpaque<T> extends GValue
{
	public final Spec<T> spec;
	public final T value;

	public GOpaque(Spec<T> spec, T value)
	{
		assert spec != null;
		assert value != null;
		this.spec = spec;
		this.value = value;
	}

	public boolean equals(Object o) { return o instanceof GOpaque && equals((GOpaque)o); }
	public boolean equals(GOpaque o)
	{
		return value.equals(o.value);
	}

	public int hashCode()
	{
		return value.hashCode();
	}

	public void writeBinary(OutputStream out) throws IOException
	{
		spec.getBinaryWriter().write(out, this.value);
	}

	public void write(Formatter out) throws IOException
	{
		spec.getSerializer().write(out, this.value);
	}

	public static final Spec<cks.Void> SpecVoid;
	public static final Spec<cks.Maybe<GValue>> SpecMaybe;

	public static final HashMap<String,Spec> specMap = new HashMap<String,Spec>();
	static {
		specMap.put("Int", spec0(BinaryWriter.Int, TextWriter.Int, BinaryReader.Int, SurfaceConverter.Int));
		specMap.put("Nat", spec0(BinaryWriter.Nat, TextWriter.Nat, BinaryReader.Nat, SurfaceConverter.Nat));
		specMap.put("Int8", spec0(BinaryWriter.Int8, TextWriter.Int8, BinaryReader.Int8, SurfaceConverter.Int8));
		specMap.put("Int16", spec0(BinaryWriter.Int16, TextWriter.Int16, BinaryReader.Int16, SurfaceConverter.Int16));
		specMap.put("Int32", spec0(BinaryWriter.Int32, TextWriter.Int32, BinaryReader.Int32, SurfaceConverter.Int32));
		specMap.put("Int64", spec0(BinaryWriter.Int64, TextWriter.Int64, BinaryReader.Int64, SurfaceConverter.Int64));
		specMap.put("Nat8", spec0(BinaryWriter.Nat8, TextWriter.Nat8, BinaryReader.Nat8, SurfaceConverter.Nat8));
		specMap.put("Nat16", spec0(BinaryWriter.Nat16, TextWriter.Nat16, BinaryReader.Nat16, SurfaceConverter.Nat16));
		specMap.put("Nat32", spec0(BinaryWriter.Nat32, TextWriter.Nat32, BinaryReader.Nat32, SurfaceConverter.Nat32));
		specMap.put("Nat64", spec0(BinaryWriter.Nat64, TextWriter.Nat64, BinaryReader.Nat64, SurfaceConverter.Nat64));
		specMap.put("Bool", spec0(BinaryWriter.Bool, TextWriter.Bool, BinaryReader.Bool, SurfaceConverter.Bool));
		specMap.put("String", spec0(BinaryWriter.String, TextWriter.String, BinaryReader.String, SurfaceConverter.String));
		specMap.put("Void", SpecVoid = spec0(BinaryWriter.Void, TextWriter.Void, BinaryReader.Void, SurfaceConverter.Void));

		specMap.put("List", new Spec1<java.util.List<GValue>>() {
			public BinaryWriter<java.util.List<GValue>> getBinaryWriter() { return new BinaryWriter.List<GValue>(GValueBinaryWriter); }
			public TextWriter<List<GValue>> getSerializer() { return new TextWriter.List<GValue>(GValueTextWriter); }
			public BinaryReader<java.util.List<GValue>> getBinaryReader(BinaryReader<GValue> arg) { return new BinaryReader.List<GValue>(arg); }
			public SurfaceConverter<java.util.List<GValue>> getSurfaceConverter(SurfaceConverter<GValue> arg) { return new SurfaceConverter.List<GValue>(arg); }
		});
		specMap.put("Set", new Spec1<java.util.Set<GValue>>() {
			public BinaryWriter<java.util.Set<GValue>> getBinaryWriter() { return new BinaryWriter.Set<GValue>(GValueBinaryWriter); }
			public TextWriter<Set<GValue>> getSerializer() { return new TextWriter.Set<GValue>(GValueTextWriter); }
			public BinaryReader<java.util.Set<GValue>> getBinaryReader(BinaryReader<GValue> arg) { return new BinaryReader.Set<GValue>(arg); }
			public SurfaceConverter<java.util.Set<GValue>> getSurfaceConverter(SurfaceConverter<GValue> arg) { return new SurfaceConverter.Set<GValue>(arg); }
		});
		specMap.put("Maybe", SpecMaybe = new Spec1<cks.Maybe<GValue>>() {
			public BinaryWriter<cks.Maybe<GValue>> getBinaryWriter() { return new BinaryWriter.Maybe<GValue>(GValueBinaryWriter); }
			public TextWriter<Maybe<GValue>> getSerializer() { return new TextWriter.Maybe<GValue>(GValueTextWriter); }
			public BinaryReader<cks.Maybe<GValue>> getBinaryReader(BinaryReader<GValue> arg) { return new BinaryReader.Maybe<GValue>(arg); }
			public SurfaceConverter<cks.Maybe<GValue>> getSurfaceConverter(SurfaceConverter<GValue> arg) { return new SurfaceConverter.Maybe<GValue>(arg); }
		});
		specMap.put("Map", new Spec2<java.util.Map<GValue,GValue>>() {
			public BinaryWriter<java.util.Map<GValue,GValue>> getBinaryWriter() { return new BinaryWriter.Map<GValue,GValue>(GValueBinaryWriter, GValueBinaryWriter); }
			public TextWriter<Map<GValue,GValue>> getSerializer() { return new TextWriter.Map<GValue,GValue>(GValueTextWriter, GValueTextWriter); }
			public BinaryReader<java.util.Map<GValue,GValue>> getBinaryReader(BinaryReader<GValue> arg1, BinaryReader<GValue> arg2) { return new BinaryReader.Map<GValue,GValue>(arg1, arg2); }
			public SurfaceConverter<java.util.Map<GValue,GValue>> getSurfaceConverter(SurfaceConverter<GValue> arg1, SurfaceConverter<GValue> arg2) { return new SurfaceConverter.Map<GValue,GValue>(arg1, arg2); }
		});
	}

	private static final BinaryWriter<GValue> GValueBinaryWriter = new BinaryWriter<GValue>() {
		public void write(OutputStream out, GValue value) throws IOException
		{
			value.writeBinary(out);
		}
	};

	private static final TextWriter<GValue> GValueTextWriter = new TextWriter<GValue>() {
		public void write(Formatter out, GValue value) throws IOException
		{
			value.write(out);
		}
	};

	public static abstract class Spec<T>
	{
		public abstract BinaryWriter<T> getBinaryWriter();
		public abstract TextWriter<T> getSerializer();
		public abstract BinaryReader<T> getBinaryReader(BinaryReader<GValue>[] args);
		public abstract SurfaceConverter<T> getSurfaceConverter(SurfaceConverter<GValue>[] args);
		public abstract int getNumParams();
	}

	public static <T> Spec<T> spec0(BinaryWriter<T> binaryWriter, TextWriter<T> serializer, BinaryReader<T> binaryReader, SurfaceConverter<T> textReader)
	{
		return new Spec0<T>(binaryWriter, serializer, binaryReader, textReader);
	}

	public static final class Spec0<T> extends Spec<T>
	{
		public final BinaryWriter<T> binaryWriter;
		public final TextWriter<T> serializer;
		public final BinaryReader<T> binaryReader;
		public final SurfaceConverter<T> textReader;

		public Spec0(BinaryWriter<T> binaryWriter, TextWriter<T> serializer, BinaryReader<T> binaryReader, SurfaceConverter<T> textReader)
		{
			this.binaryWriter = binaryWriter;
			this.serializer = serializer;
			this.binaryReader = binaryReader;
			this.textReader = textReader;
		}

		public BinaryWriter<T> getBinaryWriter()
		{
			return this.binaryWriter;
		}

		public TextWriter<T> getSerializer()
		{
			return this.serializer;
		}

		public BinaryReader<T> getBinaryReader(BinaryReader<GValue>[] args)
		{
			assert args == null;
			return this.binaryReader;
		}

		public SurfaceConverter<T> getSurfaceConverter(SurfaceConverter<GValue>[] args)
		{
			assert args == null;
			return this.textReader;
		}

		public int getNumParams() { return 0; }
	}

	public static abstract class Spec1<T> extends Spec<T>
	{
		public abstract BinaryWriter<T> getBinaryWriter();
		public abstract TextWriter<T> getSerializer();
		public abstract BinaryReader<T> getBinaryReader(BinaryReader<GValue> arg);
		public abstract SurfaceConverter<T> getSurfaceConverter(SurfaceConverter<GValue> arg);

		public BinaryReader<T> getBinaryReader(BinaryReader<GValue>[] args)
		{
			assert args != null;
			assert args.length == 1;
			return getBinaryReader(args[0]);
		}

		public SurfaceConverter<T> getSurfaceConverter(SurfaceConverter<GValue>[] args)
		{
			assert args != null;
			assert args.length == 1;
			return getSurfaceConverter(args[0]);
		}

		public int getNumParams() { return 1; }
	}

	public static abstract class Spec2<T> extends Spec<T>
	{
		public abstract BinaryWriter<T> getBinaryWriter();
		public abstract TextWriter<T> getSerializer();
		public abstract BinaryReader<T> getBinaryReader(BinaryReader<GValue> arg1, BinaryReader<GValue> arg2);
		public abstract SurfaceConverter<T> getSurfaceConverter(SurfaceConverter<GValue> arg1, SurfaceConverter<GValue> arg2);

		public BinaryReader<T> getBinaryReader(BinaryReader<GValue>[] args)
		{
			assert args != null;
			assert args.length == 2;
			return getBinaryReader(args[0], args[1]);
		}

		public SurfaceConverter<T> getSurfaceConverter(SurfaceConverter<GValue>[] args)
		{
			assert args != null;
			assert args.length == 2;
			return getSurfaceConverter(args[0], args[1]);
		}

		public int getNumParams() { return 2; }
	}
}
