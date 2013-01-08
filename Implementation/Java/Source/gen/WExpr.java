package cks.gen;

import cks.type.model.*;

/**
 * TExpr wrapper with a different hash/equality.
 * - hash/equality doesn't take source position into account.
 *
 * We use this when we want to know if two type expressions
 * represent the same type.
 */
public class WExpr
{
	public final TExpr expr;
	public WExpr(TExpr expr) { this.expr = expr; }

	public int hashCode() { return hashCode(expr); }
	public boolean equals(Object o)
	{
		if (o.getClass() != getClass()) return false;
		return equals(this.expr, ((WExpr) o).expr);
	}

	public static int hashCode(TExpr expr)
	{
		int h = expr.base.hashCode();
		if (expr.args != null) {
			for (int i = 0; i < expr.args.length; i++) {
				h <<= 1;
				h ^= expr.args[i].hashCode();
			}
		}
		return h;
	}

	public static boolean equals(TExpr a, TExpr b)
	{
		if (a.base.getTarget() != b.base.getTarget()) return false;

		if (a.args == null) return (b.args == null);
		if (b.args == null) return false;
		if (a.args.length != b.args.length) return false;

		for (int i = 0; i < a.args.length; i++) {
			if (!equals(a.args[i], b.args[i])) return false;
		}

		return true;
	}
}
