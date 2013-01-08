package cks.dynamic;

import cks.io.BinaryReader;
import cks.type.model.*;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Map;

import static cakoose.util.LangUtil.cast;

public class GBinaryReader
{
	public static GValue read(TExpr expr, InputStream in) throws BinaryReader.FormatException, IOException
	{
		try {
			return GReader.read(Handler, Collections.<TParam, GReader.Closure>emptyMap(), expr, in);
		}
		// Forward along the two exceptions we expect.  This nonsense is necessary because Java doesn't
		// let us say: GReader.Handler<InputStream,IOException|BinaryReader.FormatException>
		catch (BinaryReader.FormatException ex) {
			throw ex;
		}
		catch (IOException ex) {
			throw ex;
		}
		catch (RuntimeException ex) {
			throw ex;
		}
		catch (Exception ex) {
			throw new RuntimeException("unexpected checked exception", ex);
		}
	}

	private static final GReader.Handler<InputStream,Exception> Handler = new GReader.Handler<InputStream,Exception>()
	{
		protected GValue readOpaque(TOpaque opaque, InputStream in, GReader.Closure[] args)
			throws Exception
		{
			GOpaque.Spec<?> spec = GOpaque.specMap.get(opaque.textName);
			if (spec == null) throw new AssertionError("GBinaryReader can't handle opaque type \"" + opaque.textName + "\"");

			BinaryReader<?> r;
			if (args == null) {
				if (spec.getNumParams() != 0) {
					throw new AssertionError("GBinaryReader thinks \"" + opaque.textName + " takes " + spec.getNumParams() + " args, but we were given 0 args");
				}
				r = spec.getBinaryReader(null);
			} else {
				BinaryReader<GValue>[] rargs = cast(new BinaryReader[args.length]);
				for (int i = 0; i < args.length; i++) {
					GReader.Closure arg = args[i];
					rargs[i] = mkReader(arg.env, arg.expr);
				}
				r = spec.getBinaryReader(rargs);
			}
			@SuppressWarnings("unchecked")
			GOpaque o = new GOpaque(spec, r.readImpl(in));
			return o;
		}

		protected GValue readRecord(final Map<TParam, GReader.Closure> env, TRecord recordType, InputStream in)
			throws Exception
		{
			GValue[] values = new GValue[recordType.allMembers.size()];
			int i = 0;
			for (final TMember fieldSpec : recordType.allMembers.values()) {
				assert fieldSpec.getIndex() == i;
				values[i] = GReader.read(Handler, env, fieldSpec.type, in);
				i++;
			}

			return new GRecord(recordType, values);
		}

		protected GValue readVariant(Map<TParam, GReader.Closure> env, TVariant variantType, InputStream in)
			throws Exception
		{
			int numOptions = variantType.allMembers.size();
			int index;
			if (numOptions <= (1 << 8)) {
				index = BinaryReader.readOptionIndex1(in);
			}
			else if (numOptions <= (1 << 16)) {
				index = BinaryReader.readOptionIndex2(in);
			}
			else if (numOptions <= (1 << 16)) {
				index = BinaryReader.readOptionIndex3(in);
			}
			else {
				index = BinaryReader.readOptionIndex4(in);
			}
			if (index > numOptions) {
				throw new BinaryReader.FormatException("option index out of range: " + index);
			}

			TMember optionSpec = variantType.getMember(index);

			return new GVariant(variantType, optionSpec, GReader.read(Handler, env, optionSpec.type, in));
		}
	};

	private static BinaryReader<GValue> mkReader(final Map<TParam, GReader.Closure> env, final TExpr type)
	{
		return new BinaryReader<GValue>() {
			public GValue readImpl(InputStream in) throws IOException, BinaryReader.FormatException
			{
				try {
					return GReader.read(Handler, env, type, in);
				}
				// Forward along the two exceptions we expect.  This nonsense is necessary because Java doesn't
				// let us say: GReader.Handler<InputStream,IOException|BinaryReader.FormatException>
				catch (BinaryReader.FormatException ex) {
					throw ex;
				}
				catch (IOException ex) {
					throw ex;
				}
				catch (RuntimeException ex) {
					throw ex;
				}
				catch (Exception ex) {
					throw new RuntimeException("unexpected checked exception", ex);
				}
			}
		};
	}
}


