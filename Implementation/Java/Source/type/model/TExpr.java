package cks.type.model;

import cks.io.SourcePos;

/**
 * Normal Syntax: TypeName ( TypeArgs... )
 *
 * (TypeArgs are optional).
 */
public final class TExpr extends TDesc
{
	public final Ref base;
	public final TExpr[] args; // 'null' if no type args

	public TExpr(Ref base, TExpr[] args)
	{
		this.base = base;
		this.args = args;
	}

	public TExpr(Ref base)
	{
		this(base, null);
	}

	public SourcePos getLoc() { return base.loc; }

	// --------------------------------------------------------------

	public static abstract class Ref
	{
		public final SourcePos loc;

		protected Ref(SourcePos loc)
		{
			this.loc = loc;
		}

		public abstract TBinding getTarget();
		public abstract String getName();
		public TBinding getTargetMaybe() { return getTarget(); }

		public static final class Ident extends Ref
		{
			public final String ident;

			public Ident(SourcePos loc, String ident)
			{
				super(loc);
				if (ident == null) throw new IllegalArgumentException("'ident' can't be null");
				this.ident = ident;
			}

			private TBinding resolved;

			public void setTarget(TBinding resolved)
			{
				assert this.resolved == null;
				this.resolved = resolved;
			}

			public TBinding getTarget()
			{
				assert resolved != null;
				return resolved;
			}

			public TBinding getTargetMaybe()
			{
				return resolved;
			}

			public String getName() { return ident; }
		}

		/**
		 * For special types like implicit void and lists ("[...]").
		 */
		public static final class Direct extends Ref
		{
			public final TBinding target;

			public Direct(SourcePos loc, TBinding target)
			{
				super(loc);
				this.target = target;
			}

			public TBinding getTarget() { return target; }
			public String getName() { return target.name; }
		}
	}
}
