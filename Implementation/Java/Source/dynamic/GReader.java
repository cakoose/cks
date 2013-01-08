package cks.dynamic;

import static cakoose.util.LangUtil.badType;

import cks.type.model.TBinding;
import cks.type.model.TDef;
import cks.type.model.TExpr;
import cks.type.model.TOpaque;
import cks.type.model.TParam;
import cks.type.model.TRecord;
import cks.type.model.TVariant;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class GReader
{
	public static final class Closure
	{
		public final TExpr expr;
		public final Map<TParam,Closure> env;

		public Closure(TExpr expr, Map<TParam, Closure> env)
		{
			this.expr = expr;
			this.env = env;
		}
	}

	public static <Input,E extends Throwable> GValue read(Handler<Input,E> handler, Closure closure, Input input) throws E
	{
		return read(handler, closure.env, closure.expr, input);
	}

	public static <Input,E extends Throwable> GValue read(Handler<Input,E> handler, Map<TParam,Closure> env, TExpr expr, Input input) throws E
	{
		TBinding target = expr.base.getTarget();

		if (target instanceof TOpaque) {
			Closure[] argClosures = null;
			if (expr.args != null) {
				argClosures = new Closure[expr.args.length];
				for (int i = 0; i < expr.args.length; i++) {
					argClosures[i] = new Closure(expr.args[i], env);
				}
			}
			TOpaque opaque = (TOpaque) target;
			return handler.readOpaque(opaque, input, argClosures);
		}
		else if (target instanceof TDef) {
			TDef def = (TDef) target;

			Map<TParam,Closure> childEnv;
			assert (def.params == null) == (expr.args == null);

			if (def.params != null) {
				assert def.params.size() == expr.args.length;
				Iterator<TParam> paramI = def.params.values().iterator();
				childEnv = new HashMap<TParam,Closure>();
				for (int i = 0; i < expr.args.length; i++) {
					assert paramI.hasNext();
					childEnv.put(paramI.next(), new Closure(expr.args[i], env));
				}
			}
			else {
				childEnv = Collections.emptyMap();
			}

			if (def.desc instanceof TExpr) {
				return read(handler, childEnv, (TExpr)def.desc, input);
			}
			else if (def.desc instanceof TVariant) {
				return handler.readVariant(childEnv, (TVariant)def.desc, input);
			}
			else if (def.desc instanceof TRecord) {
				return handler.readRecord(childEnv, (TRecord)def.desc, input);
			}
			else {
				throw badType(def.desc);
			}
		}
		else if (target instanceof TParam) {
			TParam param = (TParam) target;
			Closure paramClosure = env.get(param);
			if (paramClosure == null) throw new AssertionError("couldn't resolve type parameter: " + param);
			return read(handler, paramClosure.env, paramClosure.expr, input);
		}
		else {
			throw badType(target);
		}
	}

	public static abstract class Handler<Input,E extends Throwable> {
		protected abstract GValue readOpaque(TOpaque opaque, Input input, Closure[] args) throws E;
		protected abstract GValue readRecord(Map<TParam,Closure> env, TRecord recordType, Input input) throws E;
		protected abstract GValue readVariant(Map<TParam,Closure> env, TVariant variantType, Input input) throws E;
	}
}
