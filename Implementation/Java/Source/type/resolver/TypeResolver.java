package cks.type.resolver;

import cks.type.model.*;
import cks.io.Problem;
import cks.io.SourcePos;
import cks.Maybe;
import cakoose.util.collection.RevertMap;
import static cakoose.util.LangUtil.badType;
import static cakoose.util.LangUtil.cast;

import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.Stack;
import java.util.Iterator;
import java.util.LinkedHashMap;


public class TypeResolver
{
	public static Maybe<List<Problem>> run(Map<String,? extends TBinding> globalBindings, TModule module)
	{
		TypeResolver r = new TypeResolver(globalBindings);
		r.resolve(module.defs, null);

		InheritanceChecker c = r.new InheritanceChecker();
		c.check(module);

		if (r.problems.isEmpty()) {
			return Maybe.Nothing();
		} else {
			return Maybe.Just(r.problems);
		}

	}

	private final RevertMap<String,TBinding> bindings;
	private final List<Problem> problems;

	public TypeResolver(Map<String,? extends TBinding> globalBindings)
	{
		this.bindings = new RevertMap<String,TBinding>();
		this.bindings.putAll(globalBindings);
		this.problems = new ArrayList<Problem>();
	}

	private void resolve(Map<String,TDef> defs, TDef outer)
	{
		bindings.putAll(defs);
		for (TDef def : defs.values()) {
			def.setOuter(outer);
			resolve(def);
		}
	}

	public static abstract class State
	{
		public static final class InProgress extends State {}

		public static final InProgress InProgress = new InProgress();

		public static abstract class Finished extends State
		{
			public static final class Success extends Finished
			{
				public final Map<String,TMember> members;
				public final TBinding root;

				public Success(Map<String, TMember> members, TBinding root)
				{
					this.members = members;
					this.root = root;
				}
			}

			public static final class Error extends Finished {}
			public static final Error Error = new Error();
		}

	}

	private final class InheritanceChecker
	{
		public Stack<TDef> checkStack;
		public State[] states;

		private void check(TModule module)
		{
			checkStack = new Stack<TDef>();
			states = new State[module.totalNumDefs];

			check(module.defs);
		}

		private void check(Map<String,TDef> defs)
		{
			for (TDef def : defs.values()) {
				check(def);
			}
		}

		private State.Finished check(TDef def)
		{
			checkStack.push(def);

			State state = states[def.index];

			if (state == null) {
				// We haven't visited this TDef yet.  Continue...
			}
			else if (state instanceof State.Finished) {
				return (State.Finished) state;
			}
			else if (state == State.InProgress) {
				// Cycle!
				Iterator<TDef> it = checkStack.iterator();

				// Skip over the stuff that isn't in the cycle.
				while (true) {
					assert it.hasNext(); // We know that 'def' *must* be on the stack.
					if (it.next() == def) break;
				}

				ArrayList<Problem.Ref> additional = new ArrayList<Problem.Ref>();
				while (it.hasNext()) {
					TDef pathPart = it.next();
					additional.add(new Problem.Ref(pathPart.loc, "type \"" + pathPart.name + "\""));
				}
				addErr(new Problem(new Problem.Ref(def.loc, "cyclic definition"), additional));

				return State.Finished.Error;
			}
			else {
				throw badType(state);
			}

			states[def.index] = State.InProgress;

			TDesc desc = def.desc;
			State.Finished f;
			if (desc instanceof TCompound) {
				TCompound comp = cast(desc);
				f = check(def, comp);
			}
			else if (desc instanceof TExpr) {
				f = check((TExpr)desc);
			}
			else {
				throw badType(desc);
			}
			states[def.index] = f;

			if (desc instanceof TCompound) {
				check(((TCompound)desc).defs);
			}

			checkStack.pop();

			return f;
		}

		private State.Finished check(TDef def, TCompound compound)
		{
			if (compound.parentRef == null) {
				compound.allMembers = compound.localMembers;
			}
			else {
				State.Finished parentState = check(compound.parentRef);
				TBinding parentDef = compound.parentRef.base.getTargetMaybe();

				compound.allMembers = new LinkedHashMap<String,TMember>();

				boolean errors = false;

				if (parentDef == null) {
					errors = true;
				}
				else if (parentState instanceof State.Finished.Success) {
					State.Finished.Success p = cast(parentState);
					TBinding inheritRoot = p.root;

					if (inheritRoot instanceof TDef) {
						TDesc inheritDesc = ((TDef)inheritRoot).desc;
						if (inheritDesc instanceof TCompound) {
							if (!inheritDesc.getClass().equals(compound.getClass())) {
								TCompound inheritDescC = cast(inheritDesc);
								addErr(def.loc, compound.getCategory() + " type \"" + def.name + "\" can't inherit from " + inheritDescC.getCategory() + " type \"" + parentDef.name + "\"");
								return State.Finished.Error;
							}
						}
						else {
							throw new AssertionError("unreachable"); // 'root' should never be an alias.
						}
					}
					else {
						addErr(def.loc, "can't inherit from built-in type \"" + parentDef.name + "\"");
						return State.Finished.Error;
					}

					compound.allMembers.putAll(p.members);

					for (TMember ancestorMember : p.members.values()) {
						TMember conflict = compound.localMembers.get(ancestorMember.name);
						addErr(conflict.loc, "member \"" + conflict.name + "\" conflicts with existing member defined in ancestor",
							ancestorMember.loc, "original definition");
						errors = true;
					}
				}
				else if (parentState instanceof State.Finished.Error) {
					errors = true; // Maybe avoid setting this and let resolution get further?
				}
				else {
					throw badType(parentState);
				}

				if (errors) return State.Finished.Error;

				compound.allMembers.putAll(compound.localMembers);
			}

			// Number the members.
			int currentIndex = compound.allMembers.size() - compound.localMembers.size();
			compound.setMemberIndexStart(currentIndex);

			for (TMember m : compound.localMembers.values()) {
				m.setIndex(currentIndex++);
			}

			compound.allMembersArray = compound.allMembers.values().toArray(new TMember[compound.allMembers.size()]);

			return new State.Finished.Success(compound.allMembers, def);
		}

		private State.Finished check(TExpr expr)
		{
			if (expr.args != null) {
				for (TExpr arg : expr.args) {
					check(arg);
				}
			}
			return lookup(expr.base);
		}

		private State.Finished lookup(TExpr.Ref ref)
		{
			if (ref instanceof TExpr.Ref.Direct) {
				// These are already fully resolved.
				TExpr.Ref.Direct r = cast(ref);
				return new State.Finished.Success(null, r.target);
			}
			else if (ref instanceof TExpr.Ref.Ident) {
				TExpr.Ref.Ident i = cast(ref);
				TBinding target = i.getTargetMaybe();
				if (target instanceof TOpaque) {
					return new State.Finished.Success(null, target);
				}
				else if (target instanceof TDef) {
					return check((TDef)target);
				}
				else if (target instanceof TParam) {
					return new State.Finished.Success(null, target);
				}
				else if (target == null) {
					return State.Finished.Error;
				}
				else {
					throw badType(target);
				}
			}
			else {
				throw badType(ref);
			}
		}

	}

	private void resolve(TDef def)
	{
		// Type parameters.
		bindings.mark();
		if (def.params != null) {
			assert !def.params.isEmpty();
			bindings.putAll(def.params);
		}

		if (def.desc instanceof TCompound) {
			resolve(def, (TCompound)def.desc);
		}
		else if (def.desc instanceof TExpr) {
			TExpr desc = cast(def.desc);
			TKind kind = resolve(desc);
			if (kind == null) return;
			if (kind != TKind.Base) {
				addErr(def.loc, "type alias \"" + def.name + "\": expecting a type, found a type function with kind " + describeKind(kind));
			}
			TBinding parentDef = desc.base.getTargetMaybe();
			if (parentDef instanceof TDef) {
				((TDef)parentDef).addChild(def);
			}
		}
		else {
			throw badType(def.desc);
		}

		bindings.revert();

		// Kind of ugly to descend into the nested types here, but we don't
		// want to allow nested types to use type parameters from an outer
		// type.  Possible cleaner solutions:
		// 1. Allow nested types to use outer type parameters (means we'll have
		//    to introduce implicit type parameters on nested types).
		// 2. Allow nested types on alias definitions as well, so we don't
		//    need the "instanceof TCompound" check below.
		if (def.desc instanceof TCompound) {
			resolve(((TCompound)def.desc).defs, def);
		}
	}

	private void resolve(TDef def, TCompound desc)
	{
		if (desc.parentRef != null) {
			resolve(desc.parentRef);
			TBinding parentDef = desc.parentRef.base.getTargetMaybe();
			if (parentDef instanceof TDef) {
				((TDef)parentDef).addChild(def);
			}
		}

		// Nested types.
		bindings.mark();
		bindings.putAll(desc.defs);

		for (TMember member : desc.localMembers.values()) {
			TKind kind = resolve(member.type);
			if (kind != null && kind != TKind.Base) {
				addErr(member.loc, "for member \"" + member.name + "\": expecting a type, found a type function with kind " + describeKind(kind));
			}
		}

		bindings.revert();
	}

	private TKind resolve(TExpr expr)
	{
		TExpr.Ref ref = expr.base;
		TBinding binding;
		if (ref instanceof TExpr.Ref.Direct) {
			// Already resolved.  Nothing to do.
			binding = ref.getTarget();
		}
		else if (ref instanceof TExpr.Ref.Ident) {
			// Look up identifier.
			TExpr.Ref.Ident id = cast(ref);
			binding = bindings.get(id.ident);
			if (binding == null) {
				addErr(id.loc, "unrecognized type identifier \"" + id.ident + "\"");
				return null;
			}
			id.setTarget(binding);
		}
		else {
			throw badType(ref);
		}

		TKind kind = binding.getKind();

		// Check arguments.
		if (expr.args == null) {
			return kind;
		}
		else {
			assert expr.args.length > 0;

			if (kind == TKind.Base) {
				addErr(ref.loc, "\"" + ref.getName() + "\" not a type function; not expecting type arguments");
				return null;
			}
			TKind.Func func = cast(kind);

			// Check function argument kinds.
			if (func.params.length != expr.args.length) {
				addErr(ref.loc, "incorrect number of arguments to \"" + ref.getName() + "\"; expecting " + func.params.length + ", found " + expr.args.length);
			}
			else {
				for (int i = 0; i < func.params.length; i++) {
					TKind paramKind = func.params[i];
					TExpr arg = expr.args[i];
					TKind argKind = resolve(arg);
					if (argKind != null) {
						if (!isCompatible(paramKind, argKind)) {
							addErr(ref.loc, "argument " + (i+1) + " to type \"" + ref.getName() + "\" has invalid kind; expecting (" + describeKind(paramKind) + "), found (" + describeKind(argKind) + ")");
						}
					}
				}
			}

			return TKind.Base;
		}
	}

	private static String describeKind(TKind kind)
	{
		if (kind == TKind.Base) return "*";

		TKind.Func func = cast(kind);
		String sep = "";
		StringBuilder buf = new StringBuilder();
		buf.append("(");
		for (TKind param : func.params) {
			buf.append(sep); sep = ", ";
			buf.append(describeKind(param));
		}
		buf.append(") -> *");
		return buf.toString();
	}

	private static boolean isCompatible(TKind expected, TKind found)
	{
		return expected.equals(found);
		// No support for sub-kinding...
	}

	private void addErr(Problem p)
	{
		problems.add(p);
	}

	private void addErr(SourcePos loc, String message)
	{
		addErr(new Problem(loc, message));
	}

	private void addErr(SourcePos loc, String message, SourcePos loc2, String message2)
	{
		addErr(new Problem(loc, message, loc2, message2));
	}
}
