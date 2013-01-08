package cks.type.printer;

import cks.type.model.*;
import cks.io.IndentPrinter;
import static cakoose.util.LangUtil.cast;
import static cakoose.util.LangUtil.badType;

import java.util.Map;
import java.io.IOException;

public class TypePrinter
{
	public static void print(IndentPrinter out, TModule module)
		throws IOException
	{
		printDefs(out, module.defs);
	}

	public static void printDefs(IndentPrinter out, Map<String,TDef> defs)
		throws IOException
	{
		for (TDef def : defs.values()) {
			print(out, def);
		}
	}

	public static void print(IndentPrinter out, TDef def)
		throws IOException
	{
		out.print("def ");
		out.print(def.name);
		out.print(" = ");
		TDesc d = def.desc;
		if (d instanceof TRecord) {
			print(out, (TRecord)d);
		}
		else if (d instanceof TVariant) {
			print(out, (TVariant)d);
		}
		else if (d instanceof TExpr) {
			print(out, (TExpr)d);
			out.println();
		}
	}

	private static void print(IndentPrinter out, TVariant v)
		throws IOException
	{
		out.println("<");
		out.indent();

		printMembers(out, v.localMembers);

		printDefs(out, v.defs);

		out.dedent();
		out.println(">");
	}

	private static void print(IndentPrinter out, TRecord r)
		throws IOException
	{
		out.println("{");
		out.indent();

		printMembers(out, r.localMembers);

		printDefs(out, r.defs);

		out.dedent();
		out.println("}");
	}


	private static void printMembers(IndentPrinter out, Map<String, TMember> members)
		throws IOException
	{
		for (TMember m : members.values()) {
			out.print(m.name);
			out.print(": ");
			print(out, m.type);
			out.println();
		}
	}

	private static void print(IndentPrinter out, TExpr e)
		throws IOException
	{
		TExpr.Ref r = e.base;
		if (r instanceof TExpr.Ref.Direct) {
			TBinding target = r.getTarget();
			if (target == TOpaque.Void) {
				assert e.args == null;
				out.print("()");
			}
			else if (target == TOpaque.List) {
				assert e.args.length == 1;
				out.print("[");
				print(out, e.args[0]);
				out.print("]");
			}
			else if (target == TOpaque.Maybe) {
				assert e.args.length == 1;
				print(out, e.args[0]);
				out.print("?");
			}
			else {
				out.print("#" + target.name);
				printArgs(out, e.args);
			}
		}
		else if (r instanceof TExpr.Ref.Ident) {
			TExpr.Ref.Ident id = cast(r);
			out.print(id.ident);
			printArgs(out, e.args);
		}
		else {
			throw badType(r);
		}
	}

	private static void printArgs(IndentPrinter out, TExpr[] args)
		throws IOException
	{
		if (args != null) {
			out.print("(");
			String sep = "";
			for (TExpr arg : args) {
				out.print(sep); sep = ", ";
				print(out, arg);
			}
			out.print(")");
		}
	}
}

