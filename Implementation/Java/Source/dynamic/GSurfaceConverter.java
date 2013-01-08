package cks.dynamic;

import cks.io.ProblemException;
import cks.type.model.*;
import cks.surface.SurfaceConverter;
import cks.surface.MInteger;
import cks.surface.model.VEntry;
import cks.surface.model.VTag;
import cks.surface.model.Value;

import java.util.Collections;
import java.util.Map;

import static cakoose.util.LangUtil.cast;

public class GSurfaceConverter
{
	public static GValue read(TExpr expr, Value v) throws ProblemException
	{
		return GReader.read(Handler, Collections.<TParam, GReader.Closure>emptyMap(), expr, v);
	}

	private static final GReader.Handler<Value,ProblemException> Handler = new GReader.Handler<Value,ProblemException>()
	{
		protected GValue readOpaque(TOpaque opaque, Value value, GReader.Closure[] args)
			throws ProblemException
		{
			GOpaque.Spec<?> spec = GOpaque.specMap.get(opaque.textName);
			if (spec == null) throw new AssertionError("GSurfaceConverter can't handle opaque type \"" + opaque.textName + "\"");

			SurfaceConverter<?> r;
			if (args == null) {
				if (spec.getNumParams() != 0) {
					throw new AssertionError("GSurfaceConverter thinks \"" + opaque.textName + " takes " + spec.getNumParams() + " args, but we were given 0 args");
				}
				r = spec.getSurfaceConverter(null);
			} else {
				SurfaceConverter<GValue>[] rargs = cast(new SurfaceConverter[args.length]);
				for (int i = 0; i < args.length; i++) {
					GReader.Closure arg = args[i];
					rargs[i] = mkReader(arg.env, arg.expr);
				}
				r = spec.getSurfaceConverter(rargs);
			}
			@SuppressWarnings("unchecked")
			GOpaque o = new GOpaque(spec, r.convert(value));
			return o;
		}

		protected GValue readRecord(final Map<TParam, GReader.Closure> env, TRecord recordType, Value value)
			throws ProblemException
		{
			String implicitName = null;
			if (recordType.implicit != null) {
				implicitName = recordType.implicit.name;
			}
			Map<String,VEntry> rawFields = SurfaceConverter.getFields(value, implicitName);

			GValue[] checkedValues = new GValue[recordType.allMembers.size()];
			MInteger numFieldsUsed = new MInteger(0);
			int i = 0;
			for (final TMember fieldSpec : recordType.allMembers.values()) {
				assert fieldSpec.getIndex() == i;
				checkedValues[i] = SurfaceConverter.getField(value, numFieldsUsed, rawFields, fieldSpec.name, mkReader(env, fieldSpec.type));
				i++;
			}

			if (numFieldsUsed.value < rawFields.size()) {
				SurfaceConverter.reportExtraFields(rawFields, recordType.allMembers.keySet().toArray(new String[recordType.allMembers.size()]));
			}

			return new GRecord(recordType, checkedValues);
		}

		protected GValue readVariant(Map<TParam, GReader.Closure> env, TVariant variantType, Value value)
			throws ProblemException
		{
			String name;
			Value rest;
			if (value instanceof VEntry) {
				VEntry e = (VEntry) value;
				name = e.name;
				rest = e.value;
			}
			else if (value instanceof VTag.Full) {
				VTag.Full tag = (VTag.Full) value;
				name = tag.name;
				rest = tag.rest;
			}
			else {
				throw ProblemException.pex(value.loc, "expecting a variant value");
			}
			TMember optionSpec = variantType.allMembers.get(name);
			if (optionSpec == null) {
				throw ProblemException.pex(rest.loc, "variant type has no option named \"" + name + "\"");
			}
			return new GVariant(variantType, optionSpec, GReader.read(Handler, env, optionSpec.type, rest));
		}
	};

	private static final GReader.Handler<VTag.Text,ProblemException> TextHandler = new GReader.Handler<VTag.Text,ProblemException>()
	{
		protected GValue readOpaque(TOpaque opaque, VTag.Text value, GReader.Closure[] args)
			throws ProblemException
		{
			GOpaque.Spec<?> spec = GOpaque.specMap.get(opaque.textName);
			if (spec == null) throw new AssertionError("GSurfaceConverter can't handle opaque type \"" + opaque.textName + "\"");

			SurfaceConverter<?> r;
			if (args == null) {
				if (spec.getNumParams() != 0) {
					throw new AssertionError("GSurfaceConverter thinks \"" + opaque.textName + " takes " + spec.getNumParams() + " args, but we were given 0 args");
				}
				r = spec.getSurfaceConverter(null);
			} else {
				SurfaceConverter<GValue>[] rargs = cast(new SurfaceConverter[args.length]);
				for (int i = 0; i < args.length; i++) {
					GReader.Closure arg = args[i];
					rargs[i] = mkReader(arg.env, arg.expr);
				}
				r = spec.getSurfaceConverter(rargs);
			}
			@SuppressWarnings("unchecked")
			GOpaque o = new GOpaque(spec, r.convertText(value));
			return o;
		}

		protected GValue readRecord(final Map<TParam, GReader.Closure> env, TRecord recordType, VTag.Text value)
			throws ProblemException
		{
			SurfaceConverter.expectAllWhitespace(value, "record value");
			return null;
		}

		protected GValue readVariant(Map<TParam, GReader.Closure> env, TVariant variantType, VTag.Text value)
			throws ProblemException
		{
			if (variantType.implicit == null) {
				SurfaceConverter.expectAllWhitespace(value, "record value");
				return null;
			}
			else {
				TMember optionSpec = variantType.implicit;
				return new GVariant(variantType, optionSpec, GReader.read(TextHandler, env, optionSpec.type, value));
			}
		}
	};

	private static SurfaceConverter<GValue> mkReader(final Map<TParam, GReader.Closure> env, final TExpr type)
	{
		return new SurfaceConverter<GValue>() {
			public GValue convert(Value v) throws ProblemException
			{
				return GReader.read(Handler, env, type, v);
			}

			public cks.Maybe<GValue> getDefault()
			{
				TOpaque o = getOpaque(env, type);
				if (o == TOpaque.Void) {
					return cks.Maybe.<GValue>Just(new GOpaque<cks.Void>(GOpaque.SpecVoid, cks.Void.Instance));
				}
				else if (o == TOpaque.Maybe) {
					return cks.Maybe.<GValue>Just(new GOpaque<cks.Maybe<GValue>>(GOpaque.SpecMaybe, cks.Maybe.<GValue>Nothing()));
				}
				else {
					return cks.Maybe.Nothing();
				}
			}

			public GValue convertText(VTag.Text c)
				throws ProblemException
			{
				return GReader.read(TextHandler, env, type, c);
			}
		};
	}

	private static TOpaque getOpaque(Map<TParam, GReader.Closure> env, TExpr expr)
	{
		TBinding target = expr.base.getTarget();
		if (target instanceof TParam) {
			TParam param = (TParam) target;
			GReader.Closure paramClosure = env.get(param);
			if (paramClosure == null) throw new AssertionError("couldn't resolve type parameter: " + param);
			return getOpaque(paramClosure.env, paramClosure.expr);
		}
		else if (target instanceof TOpaque) {
			return (TOpaque) target;
		}
		else {
			return null;
		}
	}

}
