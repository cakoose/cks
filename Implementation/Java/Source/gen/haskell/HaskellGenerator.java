package cks.gen.haskell;

import cks.type.model.*;
import cks.io.Problem;
import cks.io.ProblemException;
import static cks.io.ProblemException.pex;

import cks.Maybe;
import cks.io.IndentPrinter;
import static cakoose.util.LangUtil.badType;
import static cakoose.util.LangUtil.cast;

import java.io.IOException;
import java.io.Writer;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

public class HaskellGenerator
{
	public static Maybe<List<Problem>> generate(Writer outFile, String moduleName, TModule module)
		throws IOException
	{
		try {
			IndentPrinter out = new IndentPrinter(outFile);
			HaskellGenerator gen = new HaskellGenerator(out);

			out.println("-- Automatically generated CKS bindings file.  Do not edit.");
			out.println();

			out.println("{-# OPTIONS -fno-warn-unused-imports #-}");
			out.println("{-# OPTIONS -fno-warn-missing-signatures #-}");
			out.println();

			out.println("module " + moduleName + " (");
			out.indent();
			gen.dumpExports(module.defs);
			out.dedent();
			out.println(") where");
			out.println();
			
			out.println("import qualified Prelude as P'");
			out.println("import Prelude ((.), (++),)");
			out.println("import qualified Data.Int as I'");
			out.println("import qualified Data.Word as W'");
			out.println("import qualified Data.Array as Arr'");
			out.println("import qualified Control.Monad as Mon'");
			out.println("import qualified Data.Map as Map'");
			out.println("import qualified Data.Set as Set'");
			out.println("import qualified Data.ByteString as B'");
			out.println("import qualified Data.Cks.TextModel as Mod'");
			out.println("import qualified Data.Cks.TextReader as TR'");
			out.println("import qualified Data.Cks.TextWriter as TW'");
			out.println("import qualified Data.Cks.BinaryWriter as BW'");
			out.println("import qualified Data.Cks.BinaryReader as BR'");
			out.println("import qualified Data.Cks.MaybeE as ME'");
			out.println("import qualified Data.Cks.TreeDoc as TD'");
			out.println();

			gen.gen(module.defs);

			return Maybe.Nothing();
		}
		catch (ProblemException ex) {
			return Maybe.Just(ex.problems);
		}
	}

	private void dumpExports(Map<String,TDef> defs)
		throws IOException
	{
		for (TDef def : defs.values()) {
			dumpExports(def);
		}
	}

	private void dumpExports(TDef def)
		throws IOException
	{
		TDesc desc = def.desc;

		String name = typeName(def);

		out.print(name);

		if (desc instanceof TExpr) {
		}
		else if (desc instanceof TCompound) {
			out.print("(..)");
		}
		else {
			throw badType(desc);
		}
		out.print(",");

		out.print(" marshalText_" + name + ",");
		out.print(" writeText_" + name + ",");
		out.print(" writeTextIndented_" + name + ",");
		out.print(" readBinary_" + name + ",");
		out.print(" writeBinary_" + name + ",");
		out.println();

		if (desc instanceof TCompound) {
			dumpExports(((TCompound)desc).defs);
		}
	}

	public final IndentPrinter out;

	public HaskellGenerator(IndentPrinter out)
	{
		this.out = out;
	}

	private void gen(Map<String, TDef> defs)
		throws ProblemException, IOException
	{
		for (TDef def : defs.values()) {
			gen(def);
		}
	}

	private void gen(TDef def)
		throws ProblemException, IOException
	{
		out.println();
		
		String name = typeName(def);

		// Type parameters.
		String params = "";
		if (def.params != null) {
			for (TParam param : def.params.values()) {
				params += " t'" + param.name;
			}
		}

		TDesc desc = def.desc;
		if (desc instanceof TExpr) {
			TExpr e = cast(desc);
			// Alias.
			out.println("type " + name + params + " = " + ref(e));
			out.println();
			out.println("marshalText_" + name + params + " = " + getMarshalTextFunc(e));
			out.println("writeText_" + name + params + " = " + getWriteTextFunc(e));
			out.println("writeTextIndented_" + name + params + " = " + getWriteTextIndentedFunc(e));
			out.println("readBinary_" + name + params + " = " + getReadBinaryFunc(e));
			out.println("writeBinary_" + name + params + " = " + getWriteBinaryFunc(e));
		}
		else if (desc instanceof TRecord) {
			TRecord r = cast(desc);

			if (r.parentRef != null) {
				throw pex(r.parentRef.getLoc(), "Internal Error: The Haskell bindings generator doesn't support record inheritance yet.");
			}

			// Type definition
			{
				out.println("data " + name + params + " = " + name + "_");
				out.indent();

				out.print("{ ");
				String sep = "";
				for (TMember m : r.localMembers.values()) {
					out.print(sep); sep = ", ";
					String fieldName = fieldName(name, m);
					out.println(fieldName + " :: " + ref(m.type));
				}
				out.println("} deriving (P'.Eq, P'.Ord)");

				out.dedent();
				out.println();
			}

			// TextMarshaller
			{
				out.println("marshalText_" + name + params + " = (P'.Nothing, \\v -> do");
				out.indent();
				out.println("(rloc, m'0) <- TR'.extractFields v");
				int fieldPos = 1;
				for (TMember m : r.localMembers.values()) {
					out.println("(f'" + fieldPos + ", m'" + fieldPos + ") <- TR'.marshalText_RecordField rloc \"" + m.name + "\" " + getMarshalTextFunc(m.type) + " m'" + (fieldPos-1));
					fieldPos++;
				}
				out.println("TR'.checkExtraFields m'" + (fieldPos-1));
				out.println("P'.return " + name + "_");
				out.indent();
				out.print("{ ");
				fieldPos = 1;
				String sep = "";
				for (TMember m : r.localMembers.values()) {
					out.print(sep); sep = ", ";
					String fieldName = fieldName(name, m);
					out.println(fieldName + " = f'" + fieldPos);
					fieldPos++;
				}
				out.println("})");
				out.dedent();

				out.dedent();
				out.println();
			}

			// TextWriter
			{
				out.println("writeText_" + name + params + " v = do");
				out.indent();

				{
					int fieldNumber = 0;
					for (TMember m : r.localMembers.values()) {
						String fieldName = fieldName(name, m);
						out.println("f'" + fieldNumber + " <- " + getWriteTextFunc(m.type) + " (" + fieldName + " v)");
						fieldNumber++;
					}
				}

				{
					out.println("ME'.Result (\"{\"");
					out.indent();
					String sep = "";
					int fieldNumber = 0;
					for (TMember m : r.localMembers.values()) {
						out.print(sep); sep = "++\", \"";
						out.println("++\"" + m.name + " = \"++f'" + fieldNumber);
						fieldNumber++;
					}
					out.println("++\"}\")");
					out.dedent();
				}

				out.dedent();
				out.println();
			}

			// TextIndentedWriter
			{
				out.println("writeTextIndented_" + name + params + " v = do");
				out.indent();

				{
					int fieldNumber = 0;
					for (TMember m : r.localMembers.values()) {
						String fieldName = fieldName(name, m);
						out.println("f'" + fieldNumber + " <- " + getWriteTextIndentedFunc(m.type) + " (" + fieldName + " v)");
						fieldNumber++;
					}
				}

				{
					out.print("ME'.Result (TD'.branchS' \"{\" \"}\" [");
					String sep = "";
					int fieldNumber = 0;
					for (TMember field : r.localMembers.values()) {
						out.print(sep); sep = ", ";
						out.print("TD'.prefix \"" + field.name + " = \" f'" + fieldNumber);
						fieldNumber++;
					}
					out.println("])");
				}

				out.dedent();
				out.println();
			}

			// BinaryWriter
			{
				out.println("writeBinary_" + name + params + " v = do");
				out.indent();

				{
					int fieldNumber = 0;
					for (TMember m : r.localMembers.values()) {
						String fieldName = fieldName(name, m);
						out.println("f'" + fieldNumber + " <- " + getWriteBinaryFunc(m.type) + " (" + fieldName + " v)");
						fieldNumber++;
					}
				}

				{
					out.print("ME'.Result (B'.concat [");
					String sep = "";
					for (int fieldNumber = 0; fieldNumber < r.localMembers.size(); fieldNumber++) {
						out.print(sep); sep = ", ";
						out.print("f'");
						out.print(fieldNumber);
					}
					out.println("])");
				}

				out.dedent();
				out.println();
			}

			// BinaryReader
			{
				out.println("readBinary_" + name + params + " = do");
				out.indent();
				int fieldPos = 1;
				for (TMember m : r.localMembers.values()) {
					out.println("f'" + fieldPos + " <- " + getReadBinaryFunc(m.type) + " BR'.<?> \"record '" + name + "', field '" + m.name + "'\"");
					fieldPos++;
				}
				out.println("P'.return " + name + "_");
				out.indent();
				out.print("{ ");
				fieldPos = 1;
				String sep = "";
				for (TMember m : r.localMembers.values()) {
					out.print(sep); sep = ", ";
					String fieldName = fieldName(name, m);
					out.println(fieldName + " = f'" + fieldPos);
					fieldPos++;
				}
				out.println("}");
				out.dedent();

				out.dedent();
				out.println();
			}

			gen(r.defs);
		}
		else if (desc instanceof TVariant) {
			TVariant v = cast(desc);

			if (v.parentRef != null) {
				throw pex(v.parentRef.getLoc(), "Internal Error: The Haskell bindings generator doesn't support variant inheritance yet.");
			}

			// Type definition
			{
				out.println("data " + name + params);
				out.indent();

				String sep = "= ";
				for (TMember m : v.localMembers.values()) {
					out.print(sep); sep = "| ";
					String typeRef = ref(m.type);
					String optionName = optionName(name, m);
					out.print(optionName);
					if (!isVoid(m.type)) {
						out.print(' ');
						out.print(typeRef);
					}
					out.println();
				}
				out.println("deriving (P'.Eq, P'.Ord)");

				out.dedent();
				out.println();
			}

			// Marshal function
			{
				out.println("marshalText_" + name + params + " = TR'.marshalText_Variant map");
				out.indent();
				out.println("where");
				out.indent();
				out.println("map = Map'.fromList [");
				out.indent();
				String sep = "";
				for (TMember m : v.localMembers.values()) {
					out.print(sep); sep = ", ";
					out.print("(\"" + m.name + "\", ");
					String optionName = optionName(name, m);
					if (isVoid(m.type)) {
						out.print("(Mon'.liftM (P'.const " + optionName + "))");
					} else {
						out.print("(Mon'.liftM " + optionName + ")");
					}
					out.print(" . P'.snd " + getMarshalTextFunc(m.type) + ")");
					out.println();
				}
				out.println("]");
				out.dedent();
				out.dedent();
				out.dedent();
				out.println();
			}

			// TextWriter
			{
				out.println("writeText_" + name + params + " v = case v of");
				out.indent();
				for (TMember m : v.localMembers.values()) {
					String optionName = optionName(name, m);
					boolean isVoid = isVoid(m.type);
					if (isVoid) {
						out.println(optionName + " -> ME'.Result \"" + m.name + "\"");
					} else {
						out.println(optionName + " e -> do");
						out.indent();
						out.println("e' <- " + getWriteTextFunc(m.type) + " e");
						out.println("ME'.Result (\"" + m.name + ": \" ++ e')");
						out.dedent();
					}
				}
				out.dedent();
				out.println();
			}

			// TextIndentedWriter
			{
				out.println("writeTextIndented_" + name + params + " v = case v of");
				out.indent();
				for (TMember m : v.localMembers.values()) {
					String optionName = optionName(name, m);
					boolean isVoid = isVoid(m.type);
					if (isVoid) {
						out.println(optionName + " -> ME'.Result (TD'.line \"" + m.name + "\")");
					} else {
						out.println(optionName + " e -> do");
						out.indent();
						out.println("e' <- " + getWriteTextIndentedFunc(m.type) + " e");
						out.println("ME'.Result (TD'.prefix \"" + m.name + ": \" e')");
						out.dedent();
					}
				}
				out.dedent();
				out.println();
			}

			int indexBytes = getIndexBytes(v.localMembers.size());

			// BinaryWriter
			{
				out.println("writeBinary_" + name + params + " v = case v of");
				out.indent();
				int optionIndex = 0;
				for (TMember m : v.localMembers.values()) {
					String optionName = optionName(name, m);
					boolean isVoid = isVoid(m.type);
					if (isVoid) {
						out.println(optionName + " -> ME'.Result (BW'.writeBinary_OptionIndex" + indexBytes + " " + optionIndex + ")");
					} else {
						out.println(optionName + " e -> do");
						out.indent();
						out.println("e' <- " + getWriteBinaryFunc(m.type) + " e");
						if (indexBytes == 0) {
							out.println("ME'.Result e'");
						} else {
							out.println("ME'.Result (B'.append (BW'.writeBinary_OptionIndex" + indexBytes + " " + optionIndex + ") e')");
						}
						out.dedent();
					}
					optionIndex++;
				}
				out.dedent();
				out.println();
			}

			// BinaryReader
			{
				out.println("readBinary_" + name + params + " = do");
				out.indent();
				if (indexBytes == 0) {
					out.println("let i = 0");
				} else {
					out.println("i <- BR'.readBinary_OptionIndex" + indexBytes);
				}
				out.println("BR'.readBinary_Variant i array");
				out.println("where");
				out.indent();
				out.println("array = Arr'.listArray (0, " + (v.localMembers.size()-1) + ") [");
				out.indent();

				String sep = "";
				for (TMember m : v.localMembers.values()) {
					out.print(sep); sep = ", ";
					String optionName = optionName(name, m);
					if (isVoid(m.type)) {
						out.print("(Mon'.liftM (P'.const " + optionName + "))");
					} else {
						out.print("(Mon'.liftM " + optionName + ")");
					}
					out.print(" " + getReadBinaryFunc(m.type));
					out.println();
				}
				out.println("]");
				out.dedent();
				out.dedent();
				out.dedent();
				out.println();
			}

			gen(v.defs);
		}
		else {
			throw badType(desc);
		}
	}

	private static int getIndexBytes(int numOptions)
	{
		if (numOptions <= 1) return 0;
		if (numOptions <= (1 << 8)) return 1;
		if (numOptions <= (1 << 16)) return 2;
		if (numOptions <= (1 << 24)) return 3;
		return 4;
	}

	private static String params(Map<String,TParam> params, String empty, String beforeEach, String afterEach, String beforeAll, String afterAll, String sep)
	{
		if (params == null) {
			return empty;
		}

		StringBuilder buf = new StringBuilder();
		buf.append(beforeAll);

		String currentSep = "";
		for (TParam param : params.values()) {
			buf.append(currentSep); currentSep = sep;
			String id = "t'" + param.name;
			buf.append(beforeEach);
			buf.append(id);
			buf.append(afterEach);
		}

		buf.append(afterAll);
		return buf.toString();
	}

	private static boolean isVoid(TExpr expr)
	{
		return expr.base.getTarget() == TOpaque.Void;
	}

	private static String toUpperStart(String s)
	{
		assert s.length() > 0;
		if (Character.isLowerCase(s.charAt(0))) {
			s = Character.toUpperCase(s.charAt(0)) + s.substring(1);
		}
		return s;
	}

	private static String toLowerStart(String s)
	{
		assert s.length() > 0;
		if (Character.isUpperCase(s.charAt(0))) {
			s = Character.toLowerCase(s.charAt(0)) + s.substring(1);
		}
		return s;
	}

	public static String ref(TExpr.Ref r)
		throws ProblemException
	{
		TBinding b = r.getTarget();

		if (b instanceof TOpaque) {
			TOpaque o = cast(b);
			String s = OpaqueTypeEquivalents.get(o);
			if (s == null) {
				throw pex(r.loc, "Internal Error: Don't know how to handle opaque type \"" + o.name + "\"");
			}
			return s;
		}
		else if (b instanceof TParam) {
			return "t'" + ((TParam)b).name;
		}
		else if (b instanceof TDef) {
			return typeName((TDef)b);
		}
		else {
			throw badType(b);
		}
	}

	private static final Map<TOpaque,String> OpaqueTypeEquivalents = new HashMap<TOpaque,String>();

	static {
		Map<TOpaque,String> m = OpaqueTypeEquivalents;
		m.put(TOpaque.Void, "()");
		m.put(TOpaque.List, "[]");
		m.put(TOpaque.Set, "Set'.Set");
		m.put(TOpaque.Map, "Map'.Map");
		m.put(TOpaque.Maybe, "P'.Maybe");
		m.put(TOpaque.String, "P'.String");
		m.put(TOpaque.Bool, "P'.Bool");
		m.put(TOpaque.Any, "Cks.Any");

		m.put(TOpaque.Nat, "P'.Integer");
		m.put(TOpaque.Int, "P'.Integer");

		m.put(TOpaque.Nat64, "W'.Word64");
		m.put(TOpaque.Nat32, "W'.Word32");
		m.put(TOpaque.Nat16, "W'.Word16");
		m.put(TOpaque.Nat8 , "W'.Word8");
		m.put(TOpaque.Int64, "I'.Int64");
		m.put(TOpaque.Int32, "I'.Int32");
		m.put(TOpaque.Int16, "I'.Int16");
		m.put(TOpaque.Int8 , "I'.Int8");
	}

	private static abstract class ExprMapper<E extends Throwable>
	{
		public abstract String map(TOpaque o) throws E;
		public abstract String map(TParam p) throws E;
		public abstract String map(TDef d) throws E;
	}

	private static String ref(TExpr e)
		throws ProblemException
	{
		return mapExpr(RefExprMapper, e);
	}

	private static String getMarshalTextFunc(TExpr e)
		throws ProblemException
	{
		return mapExpr(MarshalTextExprMapper, e);
	}

	private static String getWriteTextFunc(TExpr e)
		throws ProblemException
	{
		return mapExpr(WriteTextExprMapper, e);
	}

	private static String getWriteTextIndentedFunc(TExpr e)
		throws ProblemException
	{
		return mapExpr(WriteTextIndentedExprMapper, e);
	}

	private static String getWriteBinaryFunc(TExpr e)
		throws ProblemException
	{
		return mapExpr(WriteBinaryExprMapper, e);
	}

	private static String getReadBinaryFunc(TExpr e)
		throws ProblemException
	{
		return mapExpr(ReadBinaryExprMapper, e);
	}

	private static <E extends Throwable> String mapExpr(ExprMapper<E> m, TExpr e)
		throws E
	{
		String s = mapExpr(m, e.base);

		if (e.args != null) {
			s = "(" + s;
			for (TExpr arg : e.args) {
				s += " " + mapExpr(m, arg);
			}
			s += ")";
		}

		return s;
	}

	public static <E extends Throwable> String mapExpr(ExprMapper<E> m, TExpr.Ref r)
		throws E
	{
		TBinding b = r.getTarget();

		if (b instanceof TOpaque) {
			return m.map((TOpaque)b);
		}
		else if (b instanceof TParam) {
			return m.map((TParam)b);
		}
		else if (b instanceof TDef) {
			return m.map((TDef)b);
		}
		else {
			throw badType(b);
		}
	}

	private static final ExprMapper<ProblemException> RefExprMapper = new ExprMapper<ProblemException>()
	{
		public String map(TOpaque o) { return OpaqueTypeEquivalents.get(o); }
		public String map(TParam p) { return "t'" + p.name; }
		public String map(TDef d) { return typeName(d); }
	};

	private static final ExprMapper<ProblemException> MarshalTextExprMapper = new ExprMapper<ProblemException>()
	{
		public String map(TOpaque o) { return "TR'.marshalText_" + o.textName; }
		public String map(TParam p) { return "t'" + p.name; }
		public String map(TDef d) { return "marshalText_" + typeName(d); }
	};

	private static final ExprMapper<ProblemException> WriteTextExprMapper = new ExprMapper<ProblemException>()
	{
		public String map(TOpaque o) { return "TW'.writeText_" + o.textName; }
		public String map(TParam p) { return "t'" + p.name; }
		public String map(TDef d) { return "writeText_" + typeName(d); }
	};

	private static final ExprMapper<ProblemException> WriteTextIndentedExprMapper = new ExprMapper<ProblemException>()
	{
		public String map(TOpaque o) { return "TW'.writeTextIndented_" + o.textName; }
		public String map(TParam p) { return "t'" + p.name; }
		public String map(TDef d) { return "writeTextIndented_" + typeName(d); }
	};

	private static final ExprMapper<ProblemException> ReadBinaryExprMapper = new ExprMapper<ProblemException>()
	{
		public String map(TOpaque o) { return "BR'.readBinary_" + o.textName; }
		public String map(TParam p) { return "t'" + p.name; }
		public String map(TDef d) { return "readBinary_" + typeName(d); }
	};

	private static final ExprMapper<ProblemException> WriteBinaryExprMapper = new ExprMapper<ProblemException>()
	{
		public String map(TOpaque o) { return "BW'.writeBinary_" + o.textName; }
		public String map(TParam p) { return "t'" + p.name; }
		public String map(TDef d) { return "writeBinary_" + typeName(d); }
	};

	private static String typeName(TDef def)
	{
		String prefix;
		if (def.getOuter() == null) {
			prefix = "";
		} else {
			prefix = typeName(def.getOuter()) + "_";
		}

		return prefix + toUpperStart(def.name);
	}

	private static String fieldName(String typeName, TMember f)
	{
		return toLowerStart(f.name) + "'" + typeName;
	}

	private static String optionName(String typeName, TMember o)
	{
		return typeName + "_" + o.name;
	}
}
