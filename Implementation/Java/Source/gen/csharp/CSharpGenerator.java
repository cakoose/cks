package cks.gen.csharp;

import cks.io.Problem;
import cks.io.ProblemException;
import cks.type.model.*;
import static cks.io.ProblemException.pex;

import cks.Maybe;
import cks.io.IndentPrinter;
import static cakoose.util.LangUtil.badType;
import static cakoose.util.LangUtil.cast;

import java.io.Writer;
import java.io.IOException;
import java.util.*;

public class CSharpGenerator
{
	public static class Options
	{
		public final boolean nullForMaybe;

		public Options(boolean nullForMaybe)
		{
			this.nullForMaybe = nullForMaybe;
		}
	}

	public static Maybe<List<Problem>> generate(Options options, Writer outFile, String namespaceName, TModule module)
		throws IOException
	{
		try {
			IndentPrinter out = new IndentPrinter(outFile);
			CSharpGenerator gen = new CSharpGenerator(options, out);

			gen.gen(namespaceName, module);

			return Maybe.Nothing();
		}
		catch (ProblemException ex) {
			return Maybe.Just(ex.problems);
		}
	}

	private final Options options;
	private final IndentPrinter out;

	public CSharpGenerator(Options options, IndentPrinter out)
	{
		this.options = options;
		this.out = out;
	}

	private void gen(String namespaceName, TModule module)
		throws ProblemException, IOException
	{
		out.println("using _S = System;");
		out.println("using _C = Cks.Data;");
		out.println("using _SC = System.Collections.Generic;");
		out.println("using _IO = System.IO;");
		out.println("using _Cks = Cks;");
		out.println("using _Self = " + namespaceName + ";");
		out.println();

		out.println("namespace " + namespaceName);
		out.println("{");
		out.println();

		gen(module.defs);

		out.println();
		out.println("} // namespace");
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
		TDesc desc = def.desc;

		if (desc instanceof TVariant) {
			genVariant(def, (TVariant)desc);
		}
		else if (desc instanceof TRecord) {
			genRecord(def, (TRecord)desc);
		}
		else if (desc instanceof TExpr) {
			genAlias(def, (TExpr)desc);
		}
		else {
			throw badType(desc);
		}
	}

	// -----------------------------------------------------------------
	// Record
	// -----------------------------------------------------------------

	private void genRecord(TDef def, TRecord desc)
		throws ProblemException, IOException
	{
		String typeName = typeName(def);
		String typeParams = typeParamList(def.params);
		String selfType = typeName + typeParams;

		// --------------------------------------------------------------------------
		// Data class.

		out.println("public class " + typeName + typeParams);

		if (desc.parentRef != null) {
			out.print(" : ");
			out.print(ref(desc.parentRef));
		}

		out.println();

		braceOpen();

		// Field definitions.
		for (TMember field : desc.localMembers.values()) {
			out.println("public readonly " + ref(field.type) + " " + field.name + ";");
		}

		// Constructor Args
		out.print("public " + typeName + "(");
		{
			String sep = "";
			for (TMember field : desc.allMembers.values()) {
				out.print(sep); sep = ", ";
				String fieldType = ref(field.type);
				String fieldName = field.name;
				out.print(fieldType + " " + fieldName);
			}
		}
		out.println(")");

		// Constructor body.
		{
			braceOpen();
			for (TMember field : desc.localMembers.values()) {
				String fieldName = field.name;
				String fieldRef = "this." + fieldName;
				out.println(fieldRef + " = " + fieldName + ";");
			}
			braceClose();
		}

		// .equals()
		{
			out.print("public override bool Equals(object o) "); braceOpen();
			out.println("return GetType() == o.GetType() && Equals((" + selfType + ") o);");
			braceClose();
			out.print("public bool Equals(" + selfType + " o) "); braceOpen();
			for (TMember field : desc.localMembers.values()) {
				String fieldName = field.name;
				out.println("if (!this." + fieldName + ".Equals(o." + fieldName + ")) return false;");
			}
			if (desc.parentRef != null) {
				out.println("return base.Equals(o);");
			} else {
				out.println("return true;");
			}
			braceClose();
		}

		// .hashCode()
		{
			out.print("public override int GetHashCode() "); braceOpen();
				if (desc.parentRef != null) {
					out.println("int h = base.GetHashCode();");
				} else {
					out.println("int h = 0;");
				}
				for (TMember field : desc.localMembers.values()) {
					String fieldName = field.name;
					out.println("h ^= this." + fieldName + ".GetHashCode();");
				}
				out.println("return h;");
				braceClose();
		}

		// --------------------------------------------------------------------------
		// Builder class.

		{
			out.print("public class _Builder");
			if (desc.parentRef != null) {
				out.print(" : ");
				out.print(ref(desc.parentRef) + typeParams + "._Builder");
			}
			out.println();
			braceOpen();

			// Fields (public and mutable)
			for (TMember field : desc.localMembers.values()) {
				String type = ref(field.type);
				String fieldName = field.name;
				if (isValueType(field.type)) {
					out.print("private bool set_" + fieldName + " = false;");
				}
				if (isMaybe(field.type)) {
					out.println("public " + type + " " + fieldName + " = new " + type + ".Nothing();");
				} else {
					out.println("public " + type + " " + fieldName + ";");
				}
				out.println("public _Builder set" + field.name + "(" + type + " Value) { this." + fieldName + " = Value; return this; }");
			}

			// isInitialized()
			out.println("public bool IsInitialized()");
			braceOpen();
			if (desc.parentRef != null) {
				out.println("if (!base.isInitialized()) return false;");
			}
			for (TMember field : desc.localMembers.values()) {
				String fieldName = field.name;
				if (isValueType(field.type)) {
					out.println("if (!set_" + fieldName + ") return false;");
				} else {
					out.println("if (" + fieldName + " == null) return false;");
				}
			}
			out.println("return true;");
			braceClose();

			// listUninitialized()
			if (desc.parentRef == null) {
				out.println("public _SC.List<string> ListUninitialized()"); braceOpen();
					out.println("_SC.List<string> l = new _SC.List<string>();");
					out.println("ListUninitialized(l);");
					out.println("return l;");
					braceClose();
			}
			out.println("protected void ListUninitialized(_SC.List<string> List)"); braceOpen();
				if (desc.parentRef != null) {
					out.println("base.ListUninitialized(List);");
				}
				for (TMember field : desc.localMembers.values()) {
					String fieldName = field.name;
					if (isValueType(field.type)) {
						out.println("if (!set_" + fieldName + ") List.Add(\"" + field.name + "\");");
					} else {
						out.println("if (" + fieldName + " == null) List.Add(\"" + field.name + "\");");
					}
				}
				braceClose();

			// build()
			out.println("public " + selfType + " Build()");
			braceOpen();
			out.println("if (!IsInitialized()) throw new _S.ApplicationException(\"uninitialized fields: \" + ListUninitialized());");
			out.print("return new " + selfType + "(");
			String sep = "";
			for (TMember field : desc.localMembers.values()) {
				out.print(sep); sep = ", ";
				String fieldName = field.name;
				out.print(fieldName);
			}
			out.println(");");
			braceClose();

			braceClose();
		}

		// --------------------------------------------------------------------------

		genFunc(def, selfType, TextReaderFunc, new RecordTextReaderPrintFunc(selfType, desc));
		genFunc(def, selfType, TextWriterFunc, L(FuncGenSpec.mk(TextWriterFunc, new RecordTextWriterPrintFunc(desc)), FuncGenSpec.mk(TextWriterIndentedFunc, new RecordTextWriterIndentedPrintFunc(desc))));
		genFunc(def, selfType, BinaryReaderFunc, new RecordBinaryReaderPrintFunc(selfType, desc));
		genFunc(def, selfType, BinaryWriterFunc, new RecordBinaryWriterPrintFunc(desc));

		braceClose();

		gen(desc.defs);

	}

	private final class RecordTextReaderPrintFunc extends PrintFunc<ProblemException>
	{
		private final String selfType;
		private final TRecord desc;

		private RecordTextReaderPrintFunc(String selfType, TRecord desc)
		{
			this.selfType = selfType;
			this.desc = desc;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException, ProblemException
		{
			out.println("uint NumFieldsUsed = 0;");
			out.println("_SC.Dictionary<string,_Cks.Text.Reader.Model.VEntry> Fields = " + m.helperClass + ".GetFields(Value);");

			// Load fields.
			{
				int fieldPos = 1;
				for (TMember field : desc.allMembers.values()) {
					String fieldType = ref(field.type);
					out.print(fieldType + " f" + fieldPos + " = ");
					String handlerObj = mapExpr(m, field.type).ident();
					out.println(m.helperClass + ".GetField(Value, ref NumFieldsUsed, Fields, \"" + field.name + "\", " + handlerObj + ");");
					fieldPos++;
				}
			}

			// Make sure there aren't any extra fields.
			out.println("if (NumFieldsUsed < Fields.Count) " + m.helperClass + ".ReportExtraFields(Fields, new string[] {");
			out.indent();
			{
				for (TMember field : desc.allMembers.values()) {
					out.println("\"" + field.name + "\",");
				}
			}
			out.dedent();
			out.println("});");

			// Return new object.
			out.print("return new " + selfType + "(");
			{
				String sep = "";
				for (int fieldPos = 1; fieldPos <= desc.allMembers.size(); fieldPos++) {
					out.print(sep); sep = ", ";
					out.print("f" + fieldPos);
				}
			}
			out.println(");");
		}
	}

	private final class RecordBinaryReaderPrintFunc extends PrintFunc<ProblemException>
	{
		private final String selfType;
		private final TRecord desc;

		private RecordBinaryReaderPrintFunc(String selfType, TRecord desc)
		{
			this.selfType = selfType;
			this.desc = desc;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException, ProblemException
		{
			out.println("return new " + selfType + "(");
			out.indent();
			String sep = "  ";
			for (TMember field : desc.allMembers.values()) {
				out.print(sep); sep = ", ";
				out.println(mapExpr(m, field.type).ident() + "." + m.handlerFunc + "(" + m.getArgs() + ")");
			}
			out.println(");");
			out.dedent();
		}
	}

	private final class RecordTextWriterPrintFunc extends PrintFunc<ProblemException>
	{
		private final TRecord desc;

		private RecordTextWriterPrintFunc(TRecord desc)
		{
			this.desc = desc;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException, ProblemException
		{
			out.println("Out.Write('{');");
			String sep = "";
			for (TMember field : desc.allMembers.values()) {
				out.print("Out.Write(\"");
				out.print(sep); sep = ", ";
				out.println(field.name + " = \");");
				String fieldRef = "Value." + field.name;
				out.println(mapExpr(m, field.type).ident() + "." + m.handlerFunc + "(Out, " + fieldRef + ");");
			}
			out.println("Out.Write('}');");
		}
	}

	private final class RecordTextWriterIndentedPrintFunc extends PrintFunc<ProblemException>
	{
		private final TRecord desc;

		private RecordTextWriterIndentedPrintFunc(TRecord desc)
		{
			this.desc = desc;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException, ProblemException
		{
			out.println("Out.WriteLine('{');");
			out.println("Out.Indent();");
			for (TMember field : desc.allMembers.values()) {
				out.print("Out.Write(\"" + field.name + " = \");");
				String fieldRef = "Value." + field.name;
				out.println(mapExpr(m, field.type).ident() + "." + m.handlerFunc + "(Out, " + fieldRef + ");");
			}
			out.println("Out.Dedent();");
			out.println("Out.WriteLine('}');");
		}
	}

	private final class RecordBinaryWriterPrintFunc extends PrintFunc<ProblemException>
	{
		private final TRecord desc;

		private RecordBinaryWriterPrintFunc(TRecord desc)
		{
			this.desc = desc;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException, ProblemException
		{
			for (TMember field : desc.allMembers.values()) {
				String fieldRef = "Value." + field.name;
				out.println(mapExpr(m, field.type).ident() + "." + m.handlerFunc + "(Out, " + fieldRef + ");");
			}
		}
	}

	// -----------------------------------------------------------------
	// Variant
	// -----------------------------------------------------------------

	private void genVariant(TDef def, TVariant desc)
		throws ProblemException, IOException
	{
		String typeName = typeName(def);
		String typeParams = typeParamList(def.params);
		String fullTypeName = "_Self." + typeName;
		String selfType = fullTypeName + typeParams;

		// -------------------------------------------------------------------------
		// Data class.

		out.print("public abstract class " + typeName + typeParams);

		if (desc.parentRef != null) {
			throw pex(desc.parentRef.getLoc(), "Internal Error: The Java generator does not support variant inheritance yet.");
		}

		out.println();

		braceOpen();

		out.println("public abstract string _Name { get; }");
		out.println("public abstract uint _Index { get; }");
		out.println("public abstract void _WriteBinary(_IO.BinaryWriter Out" + typeParamObjectList(def.params, BinaryWriterFunc.handlerClass) + ");");
		out.println("public abstract void _WriteText(_IO.TextWriter Out" + typeParamObjectList(def.params, TextWriterFunc.handlerClass) + ");");
		out.println("public abstract void _WriteTextIndented(_Cks.Text.Writer.IndentPrinter Out" + typeParamObjectList(def.params, TextWriterIndentedFunc.handlerClass) + ");");

		for (TMember option : desc.localMembers.values()) {
			out.println("public sealed class " + option.name + "_ : " + selfType);
			braceOpen();

			String typeRef = ref(option.type);
			if (!isVoid(option.type)) {
				out.println("public readonly " + typeRef + " Value;");
				out.println("public " + option.name + "_(" + typeRef + " Value)"); braceOpen();
					out.println("this.Value = Value;");
					braceClose();
			} else {
				out.println("public " + option.name + "_() {}");
			}

			// Equals()
			{
				if (!isVoid(option.type)) {
					out.print("public override bool Equals(object o) "); braceOpen();
						out.println("return GetType() == o.GetType() && Equals((" + option.name + "_) o);");
						braceClose();
					out.print("public bool Equals(" + option.name + "_ o) "); braceOpen();
						out.println("return Value.Equals(o.Value);");
						braceClose();
				} else {
					out.print("public override bool Equals(object o) "); braceOpen();
						out.println("return GetType() == o.GetType();");
						braceClose();
				}
			}

			// GetHashCode()
			{
				out.print("public override int GetHashCode() "); braceOpen();
				if (isVoid(option.type)) {
					out.println("return " + option.getIndex() + ";");
				} else {
					out.println("return " + option.getIndex() + " + Value.GetHashCode();");
				}
				braceClose();
			}

			out.println("public override string _Name { get { return \"" + option.name + "\"; } }");
			out.println("public override uint _Index { get { return " + option.getIndex() + "; } }");

			out.print("public override void _WriteBinary(_IO.BinaryWriter Out" + typeParamObjectList(def.params, BinaryWriterFunc.handlerClass) + ") ");
			if (isVoid(option.type)) {
				out.println("{}");
			} else {
				out.println("{ " + mapExpr(BinaryWriterFunc, option.type).ident() + ".Write(Out, Value); }");
			}

			out.print("public override void _WriteText(_IO.TextWriter Out" + typeParamObjectList(def.params, TextWriterFunc.handlerClass) + ") ");
			if (isVoid(option.type)) {
				out.println("{}");
			} else {
				out.println("{ Out.Write(\": \"); " + mapExpr(TextWriterFunc, option.type).ident() + ".Write(Out, Value); }");
			}

			out.print("public override void _WriteTextIndented(_Cks.Text.Writer.IndentPrinter Out" + typeParamObjectList(def.params, TextWriterIndentedFunc.handlerClass) + ") ");
			if (isVoid(option.type)) {
				out.println("{ Out.WriteLine(); }");
			} else {
				out.println("{ Out.Write(\": \"); " + mapExpr(TextWriterIndentedFunc, option.type).ident() + ".WriteIndented(Out, Value); }");
			}

			braceClose();
		}

		String paramObjectArgs = typeArgObjectList(def.params);
		String descriptorClassRef = fullTypeName + "_";

		genFunc(def, selfType, TextReaderFunc, new VariantTextReaderPrintFunc(descriptorClassRef, paramObjectArgs));
		genFunc(def, selfType, TextWriterFunc, L(FuncGenSpec.mk(TextWriterFunc, new VariantTextWriterPrintFunc(paramObjectArgs)), FuncGenSpec.mk(TextWriterIndentedFunc, new VariantTextWriterIndentedPrintFunc(paramObjectArgs))));
		genFunc(def, selfType, BinaryReaderFunc, new VariantBinaryReaderPrintFunc(desc, descriptorClassRef, paramObjectArgs));
		genFunc(def, selfType, BinaryWriterFunc, new VariantBinaryWriterPrintFunc(desc, paramObjectArgs));

		braceClose();

		// -------------------------------------------------------------------------
		// Descriptor class.

		out.println("public static class " + typeName + "_"); braceOpen();

		out.println("public abstract class BinaryReaderDelegate"); braceOpen();
			out.println("public abstract " + selfType + " ReadImpl" + typeParams + "(_IO.BinaryReader In" + typeParamObjectList(def.params, BinaryReaderFunc.handlerClass) + ");");
			braceClose();

		out.println("public abstract class TextReaderDelegate"); braceOpen();
			out.println("public abstract " + selfType + " Marshal" + typeParams + "(_Cks.Text.Reader.Model.Value Value" + typeParamObjectList(def.params, TextReaderFunc.handlerClass) + ");");
			braceClose();

		for (TMember option : desc.allMembers.values()) {
			out.println("private sealed class BinaryReaderDelegate_" + option.name + " : BinaryReaderDelegate"); braceOpen();
				out.print("public override " + selfType + " ReadImpl" + typeParams + "(_IO.BinaryReader In" + typeParamObjectList(def.params, BinaryReaderFunc.handlerClass) + ") ");
				braceOpen();
					if (isVoid(option.type)) {
						out.println("return new " + selfType + "." + option.name + "_();");
					} else {
						out.println("return new " + selfType + "." + option.name + "_(" + mapExpr(BinaryReaderFunc, option.type).ident() + "." + BinaryReaderFunc.handlerFunc + "(In));");
					}
					braceClose();
				braceClose();

			out.println("private sealed class TextReaderDelegate_" + option.name + " : TextReaderDelegate");
			braceOpen();
				out.print("public override " + selfType + " Marshal" + typeParams + "(_Cks.Text.Reader.Model.Value Value" + typeParamObjectList(def.params, TextReaderFunc.handlerClass) + ") ");
				braceOpen();
					if (isVoid(option.type)) {
						out.println("return new " + selfType + "." + option.name + "_();");
					} else {
						out.println("return new " + selfType + "." + option.name + "_(" + mapExpr(TextReaderFunc, option.type).ident() + "." + TextReaderFunc.handlerFunc + "(Value));");
					}
					braceClose();
				braceClose();
		}

		out.println("public static readonly BinaryReaderDelegate[] BinaryReaderDelegates = new BinaryReaderDelegate[] ");
		braceOpen();
			for (TMember option : desc.allMembers.values()) {
				out.println("new BinaryReaderDelegate_" + option.name + "(),");
			}
			braceClose(";");

		out.println("public static readonly _SC.Dictionary<string,TextReaderDelegate> TextReaderDelegates = new _SC.Dictionary<string,TextReaderDelegate>();");
		out.print("static " + typeName + "_() "); braceOpen();
			out.println("_SC.Dictionary<string,TextReaderDelegate> m = TextReaderDelegates;");
			for (TMember option : desc.allMembers.values()) {
				out.println("m.Add(\"" + option.name + "\", new TextReaderDelegate_" + option.name + "());");
			}
			braceClose();

		braceClose();

		// -------------------------------------------------------------------------

		gen(desc.defs);
	}

	private static <E extends Throwable> List<FuncGenSpec<E>> L(FuncGenSpec<E> a, FuncGenSpec<E> b)
	{
		ArrayList<FuncGenSpec<E>> l = new ArrayList<FuncGenSpec<E>>(2);
		l.add(a);
		l.add(b);
		return l;
	}

	private static class FuncGenSpec<E extends Throwable>
	{
		public final FuncExprMapper m;
		public final PrintFunc<E> pf;

		private FuncGenSpec(FuncExprMapper m, PrintFunc<E> pf)
		{
			this.m = m;
			this.pf = pf;
		}

		public static <E extends Throwable> FuncGenSpec<E> mk(FuncExprMapper m, PrintFunc<E> pf)
		{
			return new FuncGenSpec<E>(m, pf);
		}
	}

	private final class VariantTextReaderPrintFunc extends PrintFunc<ProblemException>
	{
		private final String descriptorClassRef;
		private final String paramObjectArgs;

		private VariantTextReaderPrintFunc(String descriptorClassRef, String paramObjectArgs)
		{
			this.descriptorClassRef = descriptorClassRef;
			this.paramObjectArgs = paramObjectArgs;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException
		{
			out.println("_Cks.Text.Reader.Model.VEntry e = " + m.helperClass + ".GetVariant(Value);");
			out.println(descriptorClassRef + ".TextReaderDelegate d;");
			out.println("if (!" + descriptorClassRef + ".TextReaderDelegates.TryGetValue(e.Name, out d)) throw new _Cks.Text.Reader.Model.ProblemException(new _Cks.Text.Reader.Model.Problem(e.SourcePos, \"variant type has no option named \\\"\" + e.Name + \"\\\"\"));");
			out.println("return d.Marshal(e.Value" + paramObjectArgs + ");");
		}
	}

	private final class VariantBinaryReaderPrintFunc extends PrintFunc<ProblemException>
	{
		private final TVariant desc;
		private final String descriptorClassRef;
		private final String paramObjectArgs;

		private VariantBinaryReaderPrintFunc(TVariant desc, String descriptorClassRef, String paramObjectArgs)
		{
			this.desc = desc;
			this.descriptorClassRef = descriptorClassRef;
			this.paramObjectArgs = paramObjectArgs;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException
		{
			int numOptions = desc.allMembers.size();
			if (numOptions == 0) {
				out.println("throw new _S.ApplicationException(\"no options\");");
			}

			int maxOption = numOptions - 1;
			int numBytes = (Integer.highestOneBit(maxOption) + 7) / 8;
			if (numBytes == 0) {
				out.println("return " + descriptorClassRef + ".BinaryReaderDelegates[0].ReadImpl(In);");
			} else {
				out.println("uint OptionIndex = " + m.helperClass + ".ReadOptionIndex" + numBytes + "(In);");
				out.println("if (OptionIndex > " + maxOption + ") throw new _Cks.Binary.Reader.BinaryFormatException(\"option index out of range: \" + OptionIndex);");
				out.println("return " + descriptorClassRef + ".BinaryReaderDelegates[OptionIndex].ReadImpl(In" + paramObjectArgs + ");");
			}
		}
	}

	private final class VariantTextWriterPrintFunc extends PrintFunc<ProblemException>
	{
		private final String paramObjectArgs;
		private VariantTextWriterPrintFunc(String paramObjectArgs)
		{
			this.paramObjectArgs = paramObjectArgs;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException
		{
			out.println("Out.Write(Value._Name);");
			out.println("Value._WriteText(Out" + paramObjectArgs + ");");
		}
	}

	private final class VariantTextWriterIndentedPrintFunc extends PrintFunc<ProblemException>
	{
		private final String paramObjectArgs;
		private VariantTextWriterIndentedPrintFunc(String paramObjectArgs)
		{
			this.paramObjectArgs = paramObjectArgs;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException
		{
			out.println("Out.Write(Value._Name);");
			out.println("Value._WriteTextIndented(Out" + paramObjectArgs + ");");
		}
	}

	private final class VariantBinaryWriterPrintFunc extends PrintFunc<ProblemException>
	{
		private final TVariant desc;
		private final String paramObjectArgs;

		private VariantBinaryWriterPrintFunc(TVariant desc, String paramObjectArgs)
		{
			this.desc = desc;
			this.paramObjectArgs = paramObjectArgs;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException
		{
			int numOptions = desc.allMembers.size();
			if (numOptions == 0) {
				out.println("throw new _S.ApplicationException(\"no options\");");
			}

			int maxOption = numOptions - 1;
			int numBytes = (Integer.highestOneBit(maxOption) + 7) / 8;
			if (numBytes > 0) {
				out.println(m.helperClass + ".WriteOptionIndex" + numBytes + "(Out, Value._Index);");
			}
			out.println("Value._WriteBinary(Out" + paramObjectArgs + ");");
		}
	}

	// -----------------------------------------------------------------
	// Alias
	// -----------------------------------------------------------------

	private void genAlias(TDef def, TExpr desc)
		throws ProblemException, IOException
	{
		String typeName = typeName(def);
		String typeParams = typeParamList(def.params);

		// Descriptor class (no data class)
		out.println("public static class " + typeName + typeParams);
		braceOpen();

		String selfType = ref(desc);

		AliasRelayPrintFunc pf = new AliasRelayPrintFunc(desc, false);
		AliasRelayPrintFunc pfRet = new AliasRelayPrintFunc(desc, true);

		genFunc(def, selfType, TextReaderFunc, pfRet);
		genFunc(def, selfType, TextWriterFunc, L(FuncGenSpec.mk(TextWriterFunc, pf), FuncGenSpec.mk(TextWriterIndentedFunc, pf)));
		genFunc(def, selfType, BinaryReaderFunc, pfRet);
		genFunc(def, selfType, BinaryWriterFunc, pf);

		braceClose();
	}

	private final class AliasRelayPrintFunc extends PrintFunc<ProblemException>
	{
		public final TExpr expr;
		public final boolean ret;

		private AliasRelayPrintFunc(TExpr expr, boolean ret)
		{
			this.expr = expr;
			this.ret = ret;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException, ProblemException
		{
			if (ret) {
				out.print("return ");
			}
			String funcObject = mapExpr(m, expr).ident();
			out.print(funcObject);
			out.print('.');
			out.print(m.handlerFunc);
			out.print('(');
			out.print(m.getArgs());
			out.println(");");
		}
	}

	private static abstract class PrintFunc<E extends Throwable> {
		public abstract void print(IndentPrinter out, FuncExprMapper m) throws IOException, E;
	}

	private <E extends Throwable> void genFunc(TDef def, String selfType, FuncExprMapper m, PrintFunc<E> printFunc)
		throws IOException, E
	{
		genFunc(def, selfType, m, Collections.singletonList(new FuncGenSpec<E>(m, printFunc)));
	}

	private <E extends Throwable> void genFunc(TDef def, String selfType, FuncExprMapper m, Iterable<FuncGenSpec<E>> specs)
		throws IOException, E
	{
		String parentClass = m.handlerClass + "<" + selfType + ">";

		if (def.params == null) {
			// Singleton.
			out.println("public static readonly " + parentClass + " " + m.handlerName + " = new _" + m.handlerName + "();");

			out.println("private sealed class _" + m.handlerName + " : " + parentClass);
			braceOpen();

			for (FuncGenSpec<E> spec : specs) {
				genFuncBody(spec.m, selfType, spec.pf);
			}

			braceClose(";");
		}
		else {
			// Instantiable (i.e. has type parameters).
			out.println("public sealed class " + m.handlerName + " : " + parentClass);
			braceOpen();

			// Handler functions for type parameters.
			for (TParam p : def.params.values()) {
				out.println("private readonly " + m.handlerClass + "<" + p.name + "> m" + p.name + ";");
			}

			// Constructor to set handler functions.
			out.print("public " + m.handlerName + "(");
			{
				String sep = "";
				for (TParam p : def.params.values()) {
					out.print(sep); sep = ", ";
					out.print(m.handlerClass + "<" + p.name + "> m" + p.name);
				}
			}
			out.println(")");
			braceOpen();
			for (TParam p : def.params.values()) {
				out.println("this.m" + p.name + " = m" + p.name + ";");
			}
			braceClose();

			for (FuncGenSpec<E> spec : specs) {
				genFuncBody(spec.m, selfType, spec.pf);
			}

			braceClose();
		}
	}

	private String typeParamList(Map<String, TParam> params)
	{
		if (params == null) return "";
		StringBuilder buf = new StringBuilder();
		String sep = "";
		buf.append('<');
		for (TParam t : params.values()) {
			buf.append(sep); sep = ",";
			buf.append(t.name);
		}
		buf.append('>');
		return buf.toString();
	}

	private String typeParamObjectList(Map<String, TParam> params, String type)
	{
		if (params == null) return "";
		StringBuilder buf = new StringBuilder();
		for (TParam t : params.values()) {
			buf.append(", ");
			buf.append(type + "<" + t.name + "> m" + t.name);
		}
		return buf.toString();
	}

	private String typeArgObjectList(Map<String, TParam> params)
	{
		if (params == null) return "";
		StringBuilder buf = new StringBuilder();
		for (TParam t : params.values()) {
			buf.append(", ");
			buf.append("m" + t.name);
		}
		return buf.toString();
	}

	private <E extends Throwable> void genFuncBody(FuncExprMapper m, String typeRef, PrintFunc<E> printFunc)
		throws IOException, E
	{
		String returnType = m.getReturnType(typeRef);
		String returnTypeString = "void";
		if (returnType != null) returnTypeString = returnType;
		out.println("public override " + returnTypeString + " " + m.handlerFunc + "(" + m.getParams(typeRef) + ")");
		braceOpen();
		printFunc.print(out, m);
		braceClose();
	}

	private static final Map<TOpaque,OpaqueSpec> OpaqueTypeEquivalents = new HashMap<TOpaque,OpaqueSpec>();

	static {
		Map<TOpaque,OpaqueSpec> m = OpaqueTypeEquivalents;
		m.put(TOpaque.Void, op("_C.Void"));
		m.put(TOpaque.List, op("_C.List"));
		m.put(TOpaque.Set, op("_C.HashSet"));
		m.put(TOpaque.Map, op("_C.Dictionary"));
		m.put(TOpaque.Maybe, op("_C.Maybe"));
		m.put(TOpaque.String, op("string"));
		m.put(TOpaque.Bool, op("bool"));

		m.put(TOpaque.Nat, op("_C.BigInteger"));
		m.put(TOpaque.Int, op("_C.BigInteger"));

		m.put(TOpaque.Nat64, op("ulong", true));
		m.put(TOpaque.Nat32, op("uint", true));
		m.put(TOpaque.Nat16, op("ushort", true));
		m.put(TOpaque.Nat8 , op("byte", true));
		m.put(TOpaque.Int64, op("long", true));
		m.put(TOpaque.Int32, op("int", true));
		m.put(TOpaque.Int16, op("short", true));
		m.put(TOpaque.Int8 , op("sbyte", true));
	}

	public static final OpaqueSpec op(String name) { return new OpaqueSpec(name, false); }
	public static final OpaqueSpec op(String name, boolean isValueType) { return new OpaqueSpec(name, isValueType); }

	private static final class OpaqueSpec
	{
		public final String name;
		public final boolean isValueType;

		private OpaqueSpec(String name, boolean isValueType)
		{
			this.name = name;
			this.isValueType = isValueType;
		}
	}

	// --------------------------------------------------------------------
	// Return a string that will parse as a Java language reference to the
	// given type expression.

	private static abstract class ExprMapper<T, E extends Throwable>
	{
		public abstract T map(TOpaque o) throws E;
		public abstract T map(TParam p) throws E;
		public abstract T map(TDef d) throws E;
		public abstract T group(T base, Iterable<? extends T> args) throws E;
	}

	private final ExprMapper<String,RuntimeException> RefExprMapper = new ExprMapper<String,RuntimeException>()
	{
		public String map(TOpaque o) { return OpaqueTypeEquivalents.get(o).name; }
		public String map(TParam p) { return p.name; }
		public String map(TDef d) { return fullTypeName(d); }
		public String group(String base, Iterable<? extends String> args)
		{
			if (args == null) return base;
			StringBuilder buf = new StringBuilder();
			buf.append(base);
			buf.append('<');
			String sep = "";
			for (String arg : args) {
				buf.append(sep); sep = ",";
				buf.append(arg);
			}
			buf.append('>');
			return buf.toString();
		}
	};

	private static final class TypedIdent
	{
		public final String before;
		public final String after;
		public final String type;

		private TypedIdent(String before, String after, String type)
		{
			this.before = before;
			this.after = after;
			this.type = type;
		}

		private String ident()
		{
			return this.before + this.after;
		}
	}

	private abstract class FuncExprMapper extends ExprMapper<TypedIdent,RuntimeException>
	{
		public final String handlerClass;
		public final String helperClass;
		public final String handlerName;
		public final String handlerFunc;

		protected FuncExprMapper(String handlerClass, String helperClass, String handlerName, String handlerFunc)
		{
			this.handlerClass = handlerClass;
			this.helperClass = helperClass;
			this.handlerName = handlerName;
			this.handlerFunc = handlerFunc;
		}

		public TypedIdent map(TOpaque o) { return new TypedIdent(helperClass + "." + o.textName, "", OpaqueTypeEquivalents.get(o).name); }
		public TypedIdent map(TParam p) { return new TypedIdent("m" + p.name, "", p.name); }
		public TypedIdent map(TDef d) { return new TypedIdent(fullTypeName(d), "." + handlerName, fullTypeName(d)); }

		public TypedIdent group(TypedIdent base, Iterable<? extends TypedIdent> args)
		{
			if (args == null) return base;

			StringBuilder t = new StringBuilder();
			StringBuilder i = new StringBuilder();

			i.append('(');
			t.append('<');
			String sep = "";
			for (TypedIdent arg : args) {
				i.append(sep); t.append(sep); sep = ", ";
				i.append(arg.ident());
				t.append(arg.type);
			}
			i.append(')');
			t.append('>');

			String identArgs = i.toString();
			String typeArgs = t.toString();

			return new TypedIdent("new " + base.before + typeArgs, base.after + identArgs, base.type + typeArgs);
		}

		public abstract String getReturnType(String selfType);
		public abstract String getArgs();
		public abstract String getParams(String selfType);
	}

	private FuncExprMapper TextReaderFunc = new FuncExprMapper("_Cks.Text.Reader.TextReader", "_Cks.Text.Reader.TextReaders", "_TextReader", "Marshal")
	{
		public String getReturnType(String selfType)
		{
			return selfType;
		}

		public String getArgs()
		{
			return "Value";
		}

		public String getParams(String selfType)
		{
			return "_Cks.Text.Reader.Model.Value Value";
		}
	};

	private FuncExprMapper TextWriterFunc = new FuncExprMapper("_Cks.Text.Writer.TextWriter", "_Cks.Text.Writer.TextWriters", "_TextWriter", "Write")
	{
		public String getReturnType(String selfType)
		{
			return null;
		}

		public String getArgs()
		{
			return "Out, Value";
		}

		public String getParams(String selfType)
		{
			return "_IO.TextWriter Out, " + selfType + " Value";
		}
	};

	private FuncExprMapper TextWriterIndentedFunc = new FuncExprMapper("_Cks.Text.Writer.TextWriter", "_Cks.Text.Writer.TextWriters", "_TextWriter", "WriteIndented")
	{
		public String getReturnType(String selfType)
		{
			return null;
		}

		public String getArgs()
		{
			return "Out, Value";
		}

		public String getParams(String selfType)
		{
			return "_Cks.Text.Writer.IndentPrinter Out, " + selfType + " Value";
		}
	};

	private FuncExprMapper BinaryWriterFunc = new FuncExprMapper("_Cks.Binary.Writer.BinaryWriter", "_Cks.Binary.Writer.BinaryWriters", "_BinaryWriter", "Write")
	{
		public String getReturnType(String selfType)
		{
			return null;
		}

		public String getArgs()
		{
			return "Out, Value";
		}

		public String getParams(String selfType)
		{
			return "_IO.BinaryWriter Out, " + selfType + " Value";
		}
	};

	private FuncExprMapper BinaryReaderFunc = new FuncExprMapper("_Cks.Binary.Reader.BinaryReader", "_Cks.Binary.Reader.BinaryReaders", "_BinaryReader", "ReadImpl")
	{
		public String getReturnType(String selfType)
		{
			return selfType;
		}

		public String getArgs()
		{
			return "In";
		}

		public String getParams(String selfType)
		{
			return "_IO.BinaryReader In";
		}
	};

	private String ref(TExpr expr) throws ProblemException { return mapExpr(RefExprMapper, expr); }

	private <T, E extends Throwable> T mapExpr(ExprMapper<T,E> m, TExpr expr)
		throws ProblemException, E
	{
		return mapExpr(m, expr, Collections.<TParam,T>emptyMap());
	}

	private <T, E extends Throwable> T mapExpr(ExprMapper<T,E> m, TExpr expr, Map<TParam,T> paramMap)
		throws ProblemException, E
	{
		T base;
		ArrayList<T> args = null;

		if (expr.args != null) {
			args = cast(new ArrayList<T>(expr.args.length));
			for (int i = 0; i < expr.args.length; i++) {
				args.add(mapExpr(m, expr.args[i], paramMap));
			}
		}

		TBinding target = expr.base.getTarget();

		if (target instanceof TOpaque) {
			TOpaque o = cast(target);
			base = m.map(o);
			if (base == null) {
				throw pex(expr.base.loc, "Internal Error: Don't know how to handle opaque type \"" + o.name + "\"");
			}
		}
		else if (target instanceof TDef) {
			TDef def = cast(target);
			TDesc desc = def.desc;
			if (desc instanceof TExpr) {
				Map<TParam,T> childParamMap;
				TExpr alias = cast(def.desc);
				if (def.params == null) {
					assert args == null;
					childParamMap = Collections.emptyMap();
				} else {
					// Translate the params.
					assert args != null;
					assert args.size() == def.params.size();
					Iterator<TParam> paramI = def.params.values().iterator();
					Iterator<T> argI = args.iterator();
					childParamMap = new HashMap<TParam,T>();
					while (paramI.hasNext()) {
						assert argI.hasNext();
						childParamMap.put(paramI.next(), argI.next());
					}
				}

				// Keep going.
				return mapExpr(m, alias, childParamMap);
			}
			else if (desc instanceof TCompound) {
				// We definitely generated a class for this, so
				// we can reference it directly.
				base = m.map(def);
			}
			else {
				throw badType(desc);
			}
		}
		else if (target instanceof TParam) {
			// If a type parameter has a substitution (caused by an alias),
			// then use that.  Otherwise just use the parameter's name.
			TParam tp = cast(target);
			T ref = paramMap.get(tp);
			if (ref != null) {
				base = ref;
			} else {
				base = m.map(tp);
			}
		}
		else {
			throw badType(target);
		}

		return m.group(base, args);
	}

	// --------------------------------------------------------------------

	private boolean isValueType(TExpr expr)
	{
		TBinding target = expr.base.getTarget();
		if (target instanceof TOpaque) {
			TOpaque otarget = cast(target);
			return OpaqueTypeEquivalents.get(otarget).isValueType;
		}
		return false;
	}

	// --------------------------------------------------------------------

	private void braceOpen() throws IOException
	{
		out.println("{");
		out.indent();
	}

	private void braceClose() throws IOException
	{
		out.dedent();
		out.println("}");
	}

	private void braceClose(String extra) throws IOException
	{
		out.dedent();
		out.print("}");
		out.println(extra);
	}

	private static boolean isVoid(TExpr expr)
	{
		return expr.base.getTarget() == TOpaque.Void;
	}

	private static boolean isMaybe(TExpr expr)
	{
		return expr.base.getTarget() == TOpaque.Maybe;
	}

	private static String typeName(TDef def)
	{
		String prefix;
		if (def.getOuter() == null) {
			prefix = "";
		} else {
			prefix = typeName(def.getOuter()) + "_";
		}

		return prefix + def.name;
	}

	private String fullTypeName(TDef def)
	{
		String prefix;
		if (def.getOuter() == null) {
			prefix = "_Self.";
		} else {
			prefix = fullTypeName(def.getOuter()) + "_";
		}

		return prefix + def.name;
	}
}
