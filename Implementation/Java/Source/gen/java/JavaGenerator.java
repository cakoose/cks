package cks.gen.java;

import cks.io.Problem;
import cks.io.ProblemException;
import static cks.io.ProblemException.pex;
import static cakoose.util.LangUtil.badType;
import static cakoose.util.LangUtil.cast;

import cks.type.model.*;

import cks.Maybe;
import cks.io.IndentPrinter;

import java.io.Writer;
import java.io.File;
import java.io.IOException;
import java.io.FileWriter;
import java.util.*;

public class JavaGenerator
{
	public static class Options
	{
		public final boolean nullForMaybe;
		public final boolean binarySupport;
		public final TextSupport textSupport;
		public final boolean surfaceConverter;
		public final boolean builder;

		public Options(boolean nullForMaybe, boolean binarySupport, TextSupport textSupport, boolean surfaceConverter, boolean builder)
		{
			this.binarySupport = binarySupport;
			this.nullForMaybe = nullForMaybe;
			this.textSupport = textSupport;
			this.surfaceConverter = surfaceConverter;
			this.builder = builder;
		}

		public enum TextSupport { ReadWrite, WriteOnly, None, ; }
	}

	public static Maybe<List<Problem>> generate(Options options, File outDir, String packageName, TModule module)
		throws IOException
	{
		try {
			for (TDef def : module.defs.values()) {
				File outFile = new File(outDir, def.name + ".java");
				Writer fout = new FileWriter(outFile);
				try {
					IndentPrinter out = new IndentPrinter(fout, "\t");

					out.println("package " + packageName + ";");
					out.println();

					JavaGenerator gen = new JavaGenerator(options, out, packageName);
					gen.genTop(def);

					fout.close();
				}
				finally {
					try { fout.close(); } catch (IOException ex) {}
				}
			}
			return Maybe.Nothing();
		}
		catch (ProblemException ex) {
			return Maybe.Just(ex.problems);
		}
	}

	private final Options options;
	private final IndentPrinter out;
	private final String packageName;

	public JavaGenerator(Options options, IndentPrinter out, String packageName)
	{
		this.options = options;
		this.out = out;
		this.packageName = packageName;
	}

	private boolean tr() { return options.textSupport == Options.TextSupport.ReadWrite; }
	private boolean tw() { return options.textSupport != Options.TextSupport.WriteOnly; }
	private boolean sc() { return options.surfaceConverter; }
	private boolean builder() { return options.builder; }

	private void genTop(TDef def)
		throws ProblemException, IOException
	{
		out.print("public");
		genDef(def);
	}

	private void genNested(Iterable<TDef> defs)
		throws ProblemException, IOException
	{
		for (TDef d : defs) {
			genNested(d);
		}
	}

	private void genNested(TDef def)
		throws ProblemException, IOException
	{
		out.print("public static");
		genDef(def);
	}

	private void genDef(TDef def)
		throws ProblemException, IOException
	{
		TDesc desc = def.desc;

		StringBuilder buf = new StringBuilder();
		buf.append(def.name);

		// Type parameters.
		if (def.params != null) {
			buf.append("<");
			String sep = "";
			for (TParam p : def.params.values()) {
				buf.append(sep); sep = ",";
				buf.append(p.name);
			}
			buf.append(">");
		}

        String ref = buf.toString();

		if (desc instanceof TVariant) {
			out.print(" abstract class ");
			out.print(ref);
			genVariant(def, (TVariant)desc);
		}
		else if (desc instanceof TRecord) {
			out.print(" class ");
			out.print(ref);
			genRecord(def, (TRecord)desc);
		}
		else if (desc instanceof TExpr) {
			out.print(" abstract class ");
			out.print(ref);
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
		if (desc.parentRef != null) {
			out.print(" extends ");
			out.print(ref(desc.parentRef));
		}

		out.println();

		braceOpen();

		String typeName = fullTypeName(def);
		String selfType = typeName + typeParamList(def.params);

		// Field definitions.
		for (TMember field : desc.localMembers.values()) {
			out.println("public final " + ref(field.type) + " " + safe(toLowerStart(field.name)) + ";");
		}

		// Constructor Args
		out.print("public " + def.name + "(");
		{
			String sep = "";
			for (TMember field : desc.allMembers.values()) {
				out.print(sep); sep = ", ";
				String fieldType = ref(field.type);
				String fieldName = safe(toLowerStart(field.name));
				out.print(fieldType + " " + fieldName);
			}
		}
		out.println(")");

		// Constructor body.
		{
			braceOpen();
            out.print("super(");
            String sep = "";
            for (int i = 0; i < (desc.allMembersArray.length - desc.localMembers.size()); i++) {
                out.print(sep); sep = ", ";
                out.print(safe(toLowerStart(desc.allMembersArray[i].name)));
            }
            out.println(");");
			for (TMember field : desc.localMembers.values()) {
				String fieldName = safe(toLowerStart(field.name));
				String fieldRef = "this." + fieldName;
                if (!isPrimitive(field.type) && !couldBeLegitimatelyNull(field.type)) {
                    out.println("assert " + fieldName + " != null : \"\\\"" + fieldName + "\\\" is null\";");
                }
				out.println(fieldRef + " = " + fieldName + ";");
			}
			braceClose();
		}

		// .toString()
		{
			out.print("public String toString() "); braceOpen();
			out.println("java.lang.StringBuilder b = new StringBuilder();");
			out.println("b.append('{');");
			String sep = "";
			for (TMember field : desc.allMembers.values()) {
				out.print("b.append(\"");
				out.print(sep); sep = ", ";
				out.println(field.name + " = \");");
				String fieldName = safe(toLowerStart(field.name));
				out.println("b.append(" + callToString("this." + fieldName, field.type) + ");");
			}
			out.println("b.append('}');");
			out.println("return b.toString();");
			braceClose();
		}

		// .equals()
		{
			out.print("public boolean equals(Object o) "); braceOpen();
			out.println("return getClass().equals(o.getClass()) && equals((" + typeName + ") o);");
			braceClose();
			out.print("public boolean equals(" + typeName + " o) "); braceOpen();
			for (TMember field : desc.localMembers.values()) {
				String fieldName = safe(toLowerStart(field.name));
				out.println("if (!(" + callEquals("this." + fieldName, "o." + fieldName, field.type) + ")) return false;");
			}
			if (desc.parentRef != null) {
				out.println("return super.equals(o);");
			} else {
				out.println("return true;");
			}
			braceClose();
		}

		// .hashCode()
		{
			out.print("public int hashCode() "); braceOpen();
				if (desc.parentRef != null) {
					out.println("int h = super.hashCode();");
				} else {
					out.println("int h = 0;");
				}
				for (TMember field : desc.localMembers.values()) {
					String fieldName = safe(toLowerStart(field.name));
					out.println("h ^= " + callHashCode("this." + fieldName, field.type) + ";");
				}
				out.println("return h;");
				braceClose();
		}

		// Builder class
		if (builder()) {
			out.print("public static final class _Builder" + typeParamList(def.params));
			if (desc.parentRef != null) {
				out.print(" extends ");
				out.print(ref(desc.parentRef) + "._Builder");
			}
			out.println();
			braceOpen();

			// Fields (public and mutable)
			for (TMember field : desc.localMembers.values()) {
				String type = ref(field.type);
				String fieldName = safe(toLowerStart(field.name));
				if (isPrimitive(field.type)) {
					out.print("private boolean set_" + fieldName + " = false;");
				}
				out.print("private " + type + " " + fieldName);
				if (isMaybe(field.type)) {  // Maybes get default values.
					if (options.nullForMaybe) {
						out.println(" = null");
					} else {
						out.println(" = cks.Maybe.Nothing()");
					}
				}
				out.println(";");
				out.print("public _Builder set" + safe(field.name) + "(" + type + " value) {");
				if (isPrimitive(field.type)) {
					out.print("this.set_" + fieldName + " = true; ");
				}
				out.println("this." + fieldName + " = value; return this; }");
			}

			// isInitialized()
			out.println("public boolean isInitialized()");
			braceOpen();
			if (desc.parentRef != null) {
				out.println("if (super.isInitialized()) return false;");
			}
			for (TMember field : desc.localMembers.values()) {
				String fieldName = safe(toLowerStart(field.name));
				if (isPrimitive(field.type)) {
					out.println("if (!set_" + fieldName + ") return false;");
				} else {
					out.println("if (" + fieldName + " == null) return false;");
				}
			}
			out.println("return true;");
			braceClose();

			// listUninitialized()
			if (desc.parentRef == null) {
				out.println("public final java.util.ArrayList<String> listUninitialized()"); braceOpen();
					out.println("java.util.ArrayList<String> l = new java.util.ArrayList<String>();");
					out.println("listUninitialized(l);");
					out.println("return l;");
					braceClose();
			}
			out.println("protected void listUninitialized(java.util.ArrayList<String> list)"); braceOpen();
				if (desc.parentRef != null) {
					out.println("super.listUninitialized(list);");
				}
				for (TMember field : desc.localMembers.values()) {
					String fieldName = safe(toLowerStart(field.name));
					if (isPrimitive(field.type)) {
						out.println("if (!set_" + fieldName + ") list.add(\"" + field.name + "\");");
					} else {
						out.println("if (" + fieldName + " == null) list.add(\"" + field.name + "\");");
					}
				}
				braceClose();

			// build()
			out.println("public " + typeName + " build()");
			braceOpen();
			out.println("if (!isInitialized()) throw new java.lang.RuntimeException(\"uninitialized fields: \" + listUninitialized());");
			out.print("return new " + selfType + "(");
			String sep = "";
			for (TMember field : desc.localMembers.values()) {
				out.print(sep); sep = ", ";
				String fieldName = safe(toLowerStart(field.name));
				out.print(fieldName);
			}
			out.println(");");
			braceClose();

			braceClose();
		}

        // FieldMapping
        if (tr()) {
            out.println("private static final java.util.HashMap<String,Integer> _FieldMapping = new java.util.HashMap<String,Integer>();");
            out.println("static"); braceOpen();
            for (TMember field : desc.allMembers.values()) {
                out.println("_FieldMapping.put(\"" + field.name + "\", " + field.getIndex() + ");");
            }
            braceClose();
        }

		if (sc()) genFunc(def, selfType, SurfaceConverterFunc, new RecordSurfaceConverterPrintFunc(selfType, desc));
		if (tr()) genFunc(def, selfType, TextReaderFunc, new RecordTextReaderPrintFunc(selfType, desc));
		if (tw()) genFunc(def, selfType, TextWriterFunc, new RecordTextWriterPrintFunc(desc));
		genFunc(def, selfType, BinaryReaderFunc, new RecordBinaryReaderPrintFunc(selfType, desc));
		genFunc(def, selfType, BinaryWriterFunc, new RecordBinaryWriterPrintFunc(desc));

		genNested(desc.defs.values());

		braceClose();
	}

	private final class RecordSurfaceConverterPrintFunc extends PrintFunc<ProblemException>
	{
		private final String selfType;
		private final TRecord desc;

		private RecordSurfaceConverterPrintFunc(String selfType, TRecord desc)
		{
			this.selfType = selfType;
			this.desc = desc;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException, ProblemException
		{
			out.println("cks.surface.MInteger numFieldsUsed = new cks.surface.MInteger(0);");
			String implicit;
			if (desc.implicit == null) {
				implicit = "null";
			} else {
				implicit = "\"" + desc.implicit.name + "\"";
			}
			out.println("java.util.Map<String,cks.surface.model.VEntry> fields = " + m.handlerClass + ".getFields(value, " + implicit + ");");

			// Load fields.
			{
				int fieldPos = 1;
				for (TMember field : desc.localMembers.values()) {
					String fieldType = ref(field.type);
					out.print(fieldType + " f" + fieldPos + " = ");
					String handlerObj = mapExpr(m, field.type);
					out.println(m.handlerClass + ".getField(value, numFieldsUsed, fields, \"" + field.name + "\", " + handlerObj + ");");
					fieldPos++;
				}
			}

			// Make sure there aren't any extra fields.
			out.println("if (numFieldsUsed.value < fields.size()) " + m.handlerClass + ".reportExtraFields(fields, new String[] {");
			out.indent();
			{
				for (TMember field : desc.localMembers.values()) {
					out.println("\"" + field.name + "\",");
				}
			}
			out.dedent();
			out.println("});");

			// Return new object.
			out.print("return new " + selfType + "(");
			{
				String sep = "";
				for (int fieldPos = 1; fieldPos <= desc.localMembers.size(); fieldPos++) {
					out.print(sep); sep = ", ";
					out.print("f" + fieldPos);
				}
			}
			out.println(");");
		}
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
			out.println("cks.io.SourcePos top = in.beginRecord();");
			for (TMember field : desc.allMembers.values()) {
				out.println(ref(field.type) + " f" + field.name + " = " + dummyValue(field.type) + ";");
                out.println("boolean got_" + field.name + " = false;");
			}

            out.print("while (true)"); braceOpen();

                out.println("String fn = in.getField();");
                out.println("if (fn == null) break;");
                out.println("Integer fi = _FieldMapping.get(fn);");
                out.println("if (fi == null) throw cks.io.ProblemException.pex(in.getLastIdentSourcePos(), \"record has no field named \\\"\" + fn + \"\\\"\");");
                out.print("switch (fi)"); braceOpen();

                for (TMember field : desc.allMembers.values()) {
                    out.println("case " + field.getIndex() + ":"); braceOpen();
                        out.println("if (got_" + field.name + ") throw cks.io.ProblemException.pex(in.getLastIdentSourcePos(), \"duplicate field \\\"\" + fn + \"\\\"\");");
                        out.println("got_" + field.name + " = true;");
                        out.println("f" + field.name + " = " + handlerVariantCall(field.type, m) + ";");
                        out.println("break;");
                    braceClose();
                }

                braceClose();

            braceClose();

            // Make sure all the fields are present.
            for (TMember field : desc.allMembers.values()) {
                out.println("if (!got_" + field.name + ") f" + field.name + " = " + mapExpr(m, field.type) + ".getFieldDefault(top, \"" + field.name + "\");");
            }

            out.println("return new " + selfType + "(");
            out.indent();
            String sep = "  ";
            for (TMember field : desc.allMembers.values()) {
                out.print(sep); sep = ", ";
                out.println("f" + field.name);
            }
            out.println(");");
            out.dedent();
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
				out.println(handlerVariantCall(field.type, m));
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
			out.println("out.beginRecord();");
			for (TMember field : desc.allMembers.values()) {
				out.println("out.preField(\"" + field.name + "\");");
				String fieldRef = "value." + safe(toLowerStart(field.name));
				out.println(mapExpr(m, field.type) + "." + handlerVariant(field.type, m) + "(out, " + fieldRef + ");");
			}
			out.println("out.endRecord();");
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
			if (desc.parentRef != null) {
				out.println("super.write(out);");
			}
			for (TMember field : desc.localMembers.values()) {
				String fieldRef = "value." + safe(toLowerStart(field.name));
				out.println(mapExpr(m, field.type) + "." + handlerVariant(field.type, m) + "(out, " + fieldRef + ");");
			}
		}
	}

	// -----------------------------------------------------------------
	// Variant
	// -----------------------------------------------------------------

	private void genVariant(TDef def, TVariant desc)
		throws ProblemException, IOException
	{
		if (desc.parentRef != null) {
			throw pex(desc.parentRef.getLoc(), "Internal Error: The Java generator does not support variant inheritance yet.");
		}

		out.println();

		String typeName = fullTypeName(def);
		String typeParams = typeParamList(def.params);
		String selfType = typeName + typeParams;

		braceOpen();

		out.println("public abstract String _name();");
		out.println("public abstract int _index();");
		out.println("public abstract void _writeBinary(java.io.OutputStream out" + typeParamObjectList(def.params, BinaryWriterFunc.handlerClass) + ") throws java.io.IOException;");
		if (tw()) out.println("public abstract void _writeOut(cks.io.Formatter out" + typeParamObjectList(def.params, TextWriterFunc.handlerClass) + ") throws java.io.IOException;");

		out.println("public static abstract class _BinaryReaderDelegate"); braceOpen();
			out.println("public abstract " + typeParams + " " + typeName + typeParams + " read(java.io.InputStream in" + typeParamObjectList(def.params, BinaryReaderFunc.handlerClass) + ") throws " + BinaryReaderFunc.exceptions + ";");
			braceClose();

        if (sc()) {
            out.println("public static abstract class _SurfaceConverterDelegate"); braceOpen();
                out.println("public abstract " + typeParams + " " + typeName + typeParams + " marshal(cks.surface.model.Value value" + typeParamObjectList(def.params, SurfaceConverterFunc.handlerClass) + ") throws " + SurfaceConverterFunc.exceptions + ";");
                braceClose();
        }

		if (tr()) {
			out.println("public static abstract class _TextReaderDelegate"); braceOpen();
				out.println("public abstract " + typeParams + " " + typeName + typeParams + " read(cks.io.Parser in" + typeParamObjectList(def.params, TextReaderFunc.handlerClass) + ") throws " + TextReaderFunc.exceptions + ";");
				braceClose();
		}

		for (TMember option : desc.localMembers.values()) {
			out.println("public static final class " + option.name + "_" + typeParams + " extends " + typeName + typeParams);
			braceOpen();

			String typeRef = ref(option.type);
			if (!isVoid(option.type)) {
				out.println("public final " + typeRef + " value;");
				out.println("public " + option.name + "_(" + typeRef + " value)"); braceOpen();
                    if (!isPrimitive(option.type) && !couldBeLegitimatelyNull(option.type)) {
                        out.println("assert value != null : \"\\\"value\\\" is null\";");
                    }
					out.println("this.value = value;");
					braceClose();
			} else {
				out.println("private " + option.name + "_() {}");
				out.println("public static final " + option.name + "_ _Instance = new " + option.name + "_();");
			}

			// .equals()
			{
				if (!isVoid(option.type)) {
					out.print("public boolean equals(Object o) "); braceOpen();
						out.println("return getClass().equals(o.getClass()) && equals((" + option.name + "_) o);");
						braceClose();
					out.print("public boolean equals(" + option.name + "_ o) "); braceOpen();
						out.println("return " + callEquals("value", "o.value", option.type) + ";");
						braceClose();
				}
			}

			// .hashCode()
			{
				out.print("public int hashCode() "); braceOpen();
				out.println("return " + option.getIndex() + " + " + callHashCode("value", option.type) + ";");
				braceClose();
			}

			out.println("public String _name() { return \"" + option.name + "\"; }");

			out.println("public int _index() { return " + option.getIndex() + "; }");

			out.print("public void _writeBinary(java.io.OutputStream out" + typeParamObjectList(def.params, BinaryWriterFunc.handlerClass) + ") throws " + BinaryWriterFunc.exceptions + " ");
			if (isVoid(option.type)) {
				out.println("{}");
			} else {
                out.println("{ " + handlerVariantCall(option.type, BinaryWriterFunc) + "; }");
			}

			if (tw()) {
				out.print("public void _writeOut(cks.io.Formatter out" + typeParamObjectList(def.params, TextWriterFunc.handlerClass) + ") throws " + TextWriterFunc.exceptions + " ");
				if (isVoid(option.type)) {
					out.println("{ out.voidVariant(\"" + option.name + "\"); }");
				} else {
					out.println("{ out.beginVariant(\"" + option.name + "\"); " + mapExpr(TextWriterFunc, option.type) + ".write(out, value); out.endVariant(); }");
				}
			}

			out.print("public final String toString() ");
			if (isVoid(option.type)) {
				out.println("{ return \"" + option.name + "\"; }");
			} else {
				out.println("{ return \"" + option.name + ": \" + " + callToString("value", option.type) + "; }");
			}

			braceClose();

			if (!isVoid(option.type)) {
				out.println("public static " + typeParams + " " + option.name + "_" + typeParams + " " + option.name + "_(" + typeRef + " value)"); braceOpen();
					out.println("return new " + option.name + "_" + typeParams + "(value);");
					braceClose();
			} else {
				out.println("public static " + typeParams + " " + option.name + "_" + typeParams + " " + option.name + "_()"); braceOpen();
					out.println("@SuppressWarnings(\"unchecked\")");
					out.println(option.name + "_" + typeParams + " i = (" + option.name + "_" + typeParams + ") " + option.name + "_._Instance;");
					out.println("return i;");
					braceClose();
			}
		}

		out.println("public static final _BinaryReaderDelegate[] _BinaryReaderDelegates = new _BinaryReaderDelegate[] ");
		braceOpen();
		for (TMember option : desc.allMembers.values()) {
			out.println("new _BinaryReaderDelegate()"); braceOpen();
			out.print("public " + typeParams + " " + typeName + typeParams + " read(java.io.InputStream in" + typeParamObjectList(def.params, BinaryReaderFunc.handlerClass) + ") throws " + BinaryReaderFunc.exceptions + " ");
			braceOpen();
			if (isVoid(option.type)) {
				out.println("return " + option.name + "_();");
			} else {
				out.println("return " + option.name + "_(" + mapExpr(BinaryReaderFunc, option.type) + "." + handlerVariant(option.type, BinaryReaderFunc) + "(in));");
			}
			braceClose();
			braceClose(",");
		}
		braceClose(";");

		if (sc()) {
			out.println("public static final java.util.Map<java.lang.String,_SurfaceConverterDelegate> _SurfaceConverterDelegates = new java.util.HashMap<java.lang.String,_SurfaceConverterDelegate>();");
			out.print("static ");
			braceOpen();
			out.println("java.util.Map<java.lang.String,_SurfaceConverterDelegate> m = _SurfaceConverterDelegates;");
			for (TMember option : desc.allMembers.values()) {
				out.print("m.put(\"" + option.name + "\", ");
				out.println("new _SurfaceConverterDelegate()");
				braceOpen();
				out.print("public " + typeParams + " " + typeName + typeParams + " marshal(cks.surface.model.Value value" + typeParamObjectList(def.params, SurfaceConverterFunc.handlerClass) + ") throws " + SurfaceConverterFunc.exceptions + " ");
				braceOpen();
				if (isVoid(option.type)) {
					out.println("return " + option.name + "_();");
				} else {
					out.println("return " + option.name + "_(" + mapExpr(SurfaceConverterFunc, option.type) + "." + handlerVariant(option.type, SurfaceConverterFunc) + "(value));");
				}
				braceClose();
				braceClose(");");
			}
			braceClose(";");
		}

        if (tr()) {
            out.println("public static final java.util.Map<java.lang.String,_TextReaderDelegate> _TextReaderDelegates = new java.util.HashMap<java.lang.String,_TextReaderDelegate>();");
            out.print("static ");
            braceOpen();
            out.println("java.util.Map<java.lang.String,_TextReaderDelegate> m = _TextReaderDelegates;");
            for (TMember option : desc.allMembers.values()) {
                out.print("m.put(\"" + option.name + "\", ");
                out.println("new _TextReaderDelegate()");
                braceOpen();
                out.print("public " + typeParams + " " + typeName + typeParams + " read(cks.io.Parser in" + typeParamObjectList(def.params, TextReaderFunc.handlerClass) + ") throws " + TextReaderFunc.exceptions + " ");
                braceOpen();
                out.println(ref(option.type) + " val;");
                out.print("if (in.hasVariantValue())"); braceOpen();
                    out.println("val = " + handlerVariantCall(option.type, TextReaderFunc) + ";");
                braceClose("else"); braceOpen();
                    out.println("val = " + mapExpr(TextReaderFunc, option.type) + ".getOptionDefault(in);");
                braceClose();
                if (isVoid(option.type)) {
                    out.println("return " + option.name + "_();");
                } else {
                    out.println("return " + option.name + "_(val);");
                }
                braceClose();
                braceClose(");");
            }
            braceClose(";");
        }

		String paramObjectArgs = typeArgObjectList(def.params);

        if (sc()) genFunc(def, selfType, SurfaceConverterFunc, new VariantSurfaceConverterPrintFunc(paramObjectArgs));
		if (tr()) genFunc(def, selfType, TextReaderFunc, new VariantTextReaderPrintFunc(paramObjectArgs));
		if (tw()) genFunc(def, selfType, TextWriterFunc, new VariantTextWriterPrintFunc(paramObjectArgs));
		genFunc(def, selfType, BinaryReaderFunc, new VariantBinaryReaderPrintFunc(desc, paramObjectArgs));
		genFunc(def, selfType, BinaryWriterFunc, new VariantBinaryWriterPrintFunc(desc, paramObjectArgs));

		genNested(desc.defs.values());

		braceClose();
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

    private final class VariantSurfaceConverterPrintFunc extends PrintFunc<ProblemException>
    {
        private final String paramObjectArgs;

        private VariantSurfaceConverterPrintFunc(String paramObjectArgs)
        {
            this.paramObjectArgs = paramObjectArgs;
        }

        public void print(IndentPrinter out, FuncExprMapper m)
            throws IOException
        {
            out.println("cks.surface.model.VEntry e = " + m.handlerClass + ".getVariant(value);");
            out.println("_SurfaceConverterDelegate d = _SurfaceConverterDelegates.get(e.name);");
            out.println("if (d == null) throw cks.io.ProblemException.pex(e.loc, \"variant type has no option named \\\"\" + e.name + \"\\\"\");");
            out.println("return d.marshal(e.value" + paramObjectArgs + ");");
        }
    }

	private final class VariantTextReaderPrintFunc extends PrintFunc<ProblemException>
	{
		private final String paramObjectArgs;

		private VariantTextReaderPrintFunc(String paramObjectArgs)
		{
			this.paramObjectArgs = paramObjectArgs;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException
		{
            out.println("String name = in.beginVariant();");
			out.println("_TextReaderDelegate d = _TextReaderDelegates.get(name);");
			out.println("if (d == null) throw cks.io.ProblemException.pex(in.getLastIdentSourcePos(), \"variant type has no option named \\\"\" + name + \"\\\"\");");
			out.println("return d.read(in" + paramObjectArgs + ");");
		}
	}

	private final class VariantBinaryReaderPrintFunc extends PrintFunc<ProblemException>
	{
		private final TVariant desc;
		private final String paramObjectArgs;

		private VariantBinaryReaderPrintFunc(TVariant desc, String paramObjectArgs)
		{
			this.desc = desc;
			this.paramObjectArgs = paramObjectArgs;
		}

		public void print(IndentPrinter out, FuncExprMapper m)
			throws IOException
		{
			int numOptions = desc.allMembers.size();
			if (numOptions == 0) {
				out.println("throw java.lang.AssertionError(\"no options\");");
			}

			int maxOption = numOptions - 1;
			int numBytes = (Integer.highestOneBit(maxOption) + 7) / 8;
			if (numBytes == 0) {
				out.println("return _BinaryReaderDelegates[0].read(in);");
			} else {
				out.println("int optionIndex = " + m.handlerClass + ".readOptionIndex" + numBytes + "(in);");
				out.println("if (optionIndex > " + maxOption + ") throw new " + m.handlerClass + ".FormatException(\"option index out of range: \" + optionIndex);");
				out.println("return _BinaryReaderDelegates[optionIndex].read(in" + paramObjectArgs + ");");
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
			out.println("value._writeOut(out" + paramObjectArgs + ");");
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
				out.println("throw java.lang.AssertionError(\"no options\");");
			}

			int maxOption = numOptions - 1;
			int numBytes = (Integer.highestOneBit(maxOption) + 7) / 8;
			if (numBytes > 0) {
				out.println(m.handlerClass + ".writeOptionIndex" + numBytes + "(out, value._index());");
			}
			out.println("value._writeBinary(out" + paramObjectArgs + ");");
		}
	}

	// -----------------------------------------------------------------
	// Alias
	// -----------------------------------------------------------------

	private void genAlias(TDef def, TExpr desc)
		throws ProblemException, IOException
	{
		out.println();
		braceOpen();

		String selfType = ref(desc);

		AliasRelayPrintFunc pf = new AliasRelayPrintFunc(desc, false);
		AliasRelayPrintFunc pfRet = new AliasRelayPrintFunc(desc, true);

		if (sc()) genFunc(def, selfType, SurfaceConverterFunc, pfRet);
        if (tr()) genFunc(def, selfType, TextReaderFunc, pfRet);
		if (tw()) genFunc(def, selfType, TextWriterFunc, pf);
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
			String funcObject = mapExpr(m, expr);
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
			out.println("public static final " + parentClass + " " + m.handlerName + " = new " + parentClass + "()");
			braceOpen();

			for (FuncGenSpec<E> spec : specs) {
				genFuncBody(spec.m, selfType, spec.pf);
			}

			braceClose(";");
		}
		else {
			String typeParams = typeParamList(def.params);

			// Instantiable (i.e. has type parameters).
			out.println("public static final class " + m.handlerName + typeParams + " extends " + parentClass);
			braceOpen();

			// Handler functions for type parameters.
			for (TParam p : def.params.values()) {
				out.println("private final " + m.handlerClass + "<" + p.name + "> m" + p.name + ";");
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

			// Fake constructor that we use to get Java to do the type inference for us.
			out.print("public static " + typeParams + " " + m.handlerName + typeParams + " mk(");
			{
				String sep = "";
				for (TParam p : def.params.values()) {
					out.print(sep); sep = ", ";
					out.print(m.handlerClass + "<" + p.name + "> m" + p.name);
				}
			}
			out.println(")");
			braceOpen();
			{
				out.print("return new " + m.handlerName + typeParams + "(");
				String sep = "";
				for (TParam p : def.params.values()) {
					out.print(sep); sep = ", ";
					out.print("m" + p.name);
				}
				out.println(");");
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
		out.println("public " + returnTypeString + " " + m.handlerFunc + "(" + m.getParams(typeRef) + ")");
		out.indent();
		out.println("throws " + m.exceptions);
		out.dedent();
		braceOpen();
		printFunc.print(out, m);
		braceClose();
	}

	private static final Map<TOpaque,String> OpaqueTypeEquivalents = new HashMap<TOpaque,String>();
	private static final Map<TOpaque,PrimitiveSpec> OpaqueTypePrimitiveEquivalents = new HashMap<TOpaque,PrimitiveSpec>();

	private static abstract class PrimitiveSpec
	{
		public final String name;
		public final String fullName;
        public final String defaultValue;

		private PrimitiveSpec(String name, String fullName, String defaultValue)
		{
			this.name = name;
			this.fullName = fullName;
            this.defaultValue = defaultValue;
		}

		public abstract String hashCode(String value);
	}

	static {
		{
			Map<TOpaque,String> m = OpaqueTypeEquivalents;
			m.put(TOpaque.Void, "cks.Void");
			m.put(TOpaque.List, "java.util.List");
			m.put(TOpaque.Set, "java.util.Set");
			m.put(TOpaque.Map, "java.util.Map");
			m.put(TOpaque.Maybe, "cks.Maybe");
			m.put(TOpaque.String, "java.lang.String");
			m.put(TOpaque.Bool, "java.lang.Boolean");

			m.put(TOpaque.Nat, "java.math.BigInteger");
			m.put(TOpaque.Int, "java.math.BigInteger");

			m.put(TOpaque.Nat64, "java.math.BigInteger");
			m.put(TOpaque.Nat32, "java.lang.Long");
			m.put(TOpaque.Nat16, "java.lang.Integer");
			m.put(TOpaque.Nat8 , "java.lang.Short");
			m.put(TOpaque.Int64, "java.lang.Long");
			m.put(TOpaque.Int32, "java.lang.Integer");
			m.put(TOpaque.Int16, "java.lang.Short");
			m.put(TOpaque.Int8 , "java.lang.Byte");
		}

		PrimitiveSpec psLong = new PrimitiveSpec("long", "java.lang.Long", "0") {
			public String hashCode(String value) {
				return "(int) (" + value + " ^ (" + value + " >>> 32))";
			}
		};

		PrimitiveSpec psInteger = new PrimitiveSpec("int", "java.lang.Integer", "0") {
			public String hashCode(String value) {
				return value;
			}
		};

		PrimitiveSpec psShort = new PrimitiveSpec("short", "java.lang.Short", "0") {
			public String hashCode(String value) {
				return value;
			}
		};

        PrimitiveSpec psByte = new PrimitiveSpec("byte", "java.lang.Byte", "0") {
            public String hashCode(String value) {
                return value;
            }
        };

		PrimitiveSpec psBool = new PrimitiveSpec("boolean", "java.lang.Boolean", "false") {
			public String hashCode(String value) {
                return "(value ? 0 : 1)";
			}
		};

		{
			Map<TOpaque,PrimitiveSpec> m = OpaqueTypePrimitiveEquivalents;
			m.put(TOpaque.Nat32, psLong);
			m.put(TOpaque.Nat16, psInteger);
			m.put(TOpaque.Nat8 , psShort);
			m.put(TOpaque.Int64, psLong);
			m.put(TOpaque.Int32, psInteger);
			m.put(TOpaque.Int16, psShort);
            m.put(TOpaque.Int8 , psByte);
			m.put(TOpaque.Bool , psBool);
		}
	}

	// --------------------------------------------------------------------
	// Return a string that will parse as a Java language reference to the
	// given type expression.

	private static abstract class ExprMapper<T, E extends Throwable>
	{
		public abstract T map(boolean isMaybeArg, TOpaque o, List<T> args) throws E;
		public abstract T map(TParam p) throws E;
		public abstract T map(TDef d) throws E;
		public abstract T group(T base, Iterable<? extends T> args) throws E;
	}

	private final ExprMapper<String,RuntimeException> RefExprMapper = new ExprMapper<String,RuntimeException>()
	{
		public String map(boolean isMaybeArg, TOpaque o, List<String> args)
		{
			if (isNullMaybe(isMaybeArg, o)) {
				assert args.size() == 1;
				String inner = args.get(0);
				args.clear();
				return inner;
			}
			return OpaqueTypeEquivalents.get(o);
		}

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

	private abstract class FuncExprMapper extends ExprMapper<String,RuntimeException>
	{
		public final String handlerClass;
		public final String handlerName;
		public final String handlerFunc;
		public final String handlerFuncPrimitive;
		public final String exceptions;

		protected FuncExprMapper(String handlerClass, String handlerName,
		                         String handlerFunc, String handlerFuncPrimitive,
		                         String exceptions)
		{
			this.handlerClass = handlerClass;
			this.handlerName = handlerName;
			this.handlerFunc = handlerFunc;
			this.handlerFuncPrimitive = handlerFuncPrimitive;
			this.exceptions = exceptions;
		}

		public String map(boolean isMaybeArg, TOpaque o, List<String> args)
		{
			if (isNullMaybe(isMaybeArg, o)) {
				return handlerClass + "." + o.textName + "Null";
			}
			return handlerClass + "." + o.textName;
		}

		public String map(TParam p) { return "m" + p.name; }
		public String map(TDef d) { return fullTypeName(d) + "." + handlerName; }

		public String group(String base, Iterable<? extends String> args)
		{
			if (args == null) return base;

			StringBuilder buf = new StringBuilder();
			buf.append(base);
			buf.append(".mk(");
			String sep = "";
			for (String arg : args) {
				buf.append(sep); sep = ", ";
				buf.append(arg);
			}
			buf.append(')');
			return buf.toString();
		}

		public abstract String getReturnType(String selfType);
		public abstract String getArgs();
		public abstract String getParams(String selfType);
	}

	private FuncExprMapper SurfaceConverterFunc = new FuncExprMapper("cks.surface.SurfaceConverter", "_SurfaceConverter", "convert", "convert", "cks.io.ProblemException")
	{
		public String getReturnType(String selfType) { return selfType; }
		public String getArgs() { return "value"; }
		public String getParams(String selfType) { return "cks.surface.model.Value value"; }
	};

	private FuncExprMapper TextReaderFunc = new FuncExprMapper("cks.io.TextReader", "_TextReader", "readImpl", "readPrimitive", "java.io.IOException, cks.io.ProblemException")
	{
		public String getReturnType(String selfType) { return selfType; }
		public String getArgs() { return "in"; }
		public String getParams(String selfType) { return "cks.io.Parser in"; }
	};

	private FuncExprMapper TextWriterFunc = new FuncExprMapper("cks.io.TextWriter", "_TextWriter", "write", "write", "java.io.IOException")
	{
		public String getReturnType(String selfType) { return null; }
		public String getArgs() { return "out, value"; }
		public String getParams(String selfType) { return "cks.io.Formatter out, " + selfType + " value"; }
	};

	private FuncExprMapper BinaryWriterFunc = new FuncExprMapper("cks.io.BinaryWriter", "_BinaryWriter", "write", "writePrimitive", "java.io.IOException")
	{
		public String getReturnType(String selfType) { return null; }
		public String getArgs() { return "out, value"; }
		public String getParams(String selfType) { return "java.io.OutputStream out, " + selfType + " value"; }
	};

	private FuncExprMapper BinaryReaderFunc = new FuncExprMapper("cks.io.BinaryReader", "_BinaryReader", "readImpl", "readPrimitive", "java.io.IOException, cks.io.BinaryReader.FormatException")
	{
		public String getReturnType(String selfType) { return selfType; }
		public String getArgs() { return "in"; }
		public String getParams(String selfType) { return "java.io.InputStream in"; }
	};

    private String ref(TExpr expr) throws ProblemException
    {
        // Primitive types are more efficient than boxed types.  We can use
        // primitive types at the top level.
        TBinding target = expr.base.getTarget();
        if (target instanceof TOpaque) {
            TOpaque otarget = cast(target);
            PrimitiveSpec ps = OpaqueTypePrimitiveEquivalents.get(otarget);
            if (ps != null) return ps.name;
        }

        return mapExpr(RefExprMapper, expr);
    }

    private String dummyValue(TExpr expr)
    {
        // Primitive types are more efficient than boxed types.  We can use
        // primitive types at the top level.
        TBinding target = expr.base.getTarget();
        if (target instanceof TOpaque) {
            TOpaque otarget = cast(target);
            PrimitiveSpec ps = OpaqueTypePrimitiveEquivalents.get(otarget);
            if (ps != null) return ps.defaultValue;
        }

        return "null";
    }

	private <T, E extends Throwable> T mapExpr(ExprMapper<T,E> m, TExpr expr)
		throws ProblemException, E
	{
		return mapExpr(m, expr, Collections.<TParam,T>emptyMap(), false);
	}

	// 'paramMap' tracks type parameter instantiations across type aliases.
	private <T, E extends Throwable> T mapExpr(ExprMapper<T,E> m, TExpr expr, Map<TParam,T> paramMap, boolean isMaybeArg)
		throws ProblemException, E
	{
		T base;
		ArrayList<T> args = null;

		if (expr.args != null) {
			args = cast(new ArrayList<T>(expr.args.length));
			for (int i = 0; i < expr.args.length; i++) {
				args.add(mapExpr(m, expr.args[i], paramMap, isMaybe(expr)));
			}
		}

		TBinding target = expr.base.getTarget();

		if (target instanceof TOpaque) {
			TOpaque o = cast(target);
			base = m.map(isMaybeArg, o, args);
			if (args != null && args.isEmpty()) {
				// The 'map' function may have cleared the 'args' array.
				args = null;
			}
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
				return mapExpr(m, alias, childParamMap, false);
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

	private static String toLowerStart(String s)
	{
		assert s.length() > 0;
		if (Character.isUpperCase(s.charAt(0))) {
			s = Character.toLowerCase(s.charAt(0)) + s.substring(1);
		}
		return s;
	}

	private static String safe(String s)
	{
		// Safe for use as a Java identifier.
		// Mapping must be unique.
		if (JavaKeywords.contains(s)) {
			return s + "_";
		} else {
			return s;
		}
	}

	private static final Set<String> JavaKeywords = new HashSet<String>();
	static {
		JavaKeywords.add("abstract");
		JavaKeywords.add("boolean");
		JavaKeywords.add("break");
		JavaKeywords.add("byte");
		JavaKeywords.add("case");
		JavaKeywords.add("catch");
		JavaKeywords.add("char");
		JavaKeywords.add("class");
		JavaKeywords.add("const");
		JavaKeywords.add("continue");
		JavaKeywords.add("default");
		JavaKeywords.add("do");
		JavaKeywords.add("double");
		JavaKeywords.add("else");
		JavaKeywords.add("enum");
		JavaKeywords.add("extends");
		JavaKeywords.add("final");
		JavaKeywords.add("finally");
		JavaKeywords.add("float");
		JavaKeywords.add("for");
		JavaKeywords.add("goto");
		JavaKeywords.add("if");
		JavaKeywords.add("implements");
		JavaKeywords.add("import");
		JavaKeywords.add("instanceof");
		JavaKeywords.add("int");
		JavaKeywords.add("interface");
		JavaKeywords.add("long");
		JavaKeywords.add("native");
		JavaKeywords.add("new");
		JavaKeywords.add("package");
		JavaKeywords.add("private");
		JavaKeywords.add("protected");
		JavaKeywords.add("public");
		JavaKeywords.add("return");
		JavaKeywords.add("short");
		JavaKeywords.add("static");
		JavaKeywords.add("strictfp");
		JavaKeywords.add("super");
		JavaKeywords.add("switch");
		JavaKeywords.add("synchronized");
		JavaKeywords.add("this");
		JavaKeywords.add("throw");
		JavaKeywords.add("throws");
		JavaKeywords.add("transient");
		JavaKeywords.add("void");
		JavaKeywords.add("volatile");
		JavaKeywords.add("while");
	}

	private static boolean isVoid(TExpr expr)
	{
		return expr.base.getTarget() == TOpaque.Void;
	}

	private static boolean isMaybe(TExpr expr)
	{
		return expr.base.getTarget() == TOpaque.Maybe;
	}

	private boolean isNullMaybe(boolean isMaybeArg, TOpaque o)
	{
		return options.nullForMaybe && !isMaybeArg && o == TOpaque.Maybe;
	}

	private boolean isNullMaybe(TExpr expr)
	{
		return options.nullForMaybe && isMaybe(expr);
	}

	private boolean isPrimitive(TExpr expr)
	{
		TBinding target = expr.base.getTarget();
		if (target instanceof TOpaque) {
			TOpaque otarget = cast(target);
			if (OpaqueTypePrimitiveEquivalents.containsKey(otarget)) {
				return true;
			}
		}
		return false;
	}

	private PrimitiveSpec getPrimitive(TExpr expr)
	{
		TBinding target = expr.base.getTarget();
		if (target instanceof TOpaque) {
			TOpaque otarget = cast(target);
			return (OpaqueTypePrimitiveEquivalents.get(otarget));
		}
		return null;
	}

	private String fullTypeName(TDef def)
	{
		String prefix;
		if (def.getOuter() == null) {
			prefix = packageName + ".";
		} else {
			prefix = fullTypeName(def.getOuter()) + ".";
		}

		return prefix + def.name;
	}

	private boolean isNullTypeParam(TExpr type)
	{
		return options.nullForMaybe && type.base.getTarget() instanceof TParam;
	}

    private boolean couldBeLegitimatelyNull(TExpr type)
    {
        return isNullMaybe(type) || isNullTypeParam(type);
    }

	private String callEquals(String arg1, String arg2, TExpr type)
	{
		if (isVoid(type)) {
			return "true";
		}

		if (couldBeLegitimatelyNull(type)) {
			return arg1 + " == null && " + arg2 + " == null || " + arg1 + " != null && " + arg2 + " != null && (" + arg1 + ".equals(" + arg2 + "))";
		}

		// If we're storing it as a primitive, use "==" to compare.
		PrimitiveSpec ps = getPrimitive(type);
		if (ps != null) {
			return arg1 + " == " + arg2;
		}

		return arg1 + ".equals(" + arg2 + ")";
	}

	private String callHashCode(String arg, TExpr type)
	{
		if (isVoid(type)) {
			return "0";
		}
		if (couldBeLegitimatelyNull(type)) {
			return "(" + arg + " == null ? 0 : (1 + " + arg + ".hashCode()))";
		}
		PrimitiveSpec ps = getPrimitive(type);
		if (ps != null) {
			return ps.hashCode(arg);
		}
		return arg + ".hashCode()";
	}

	private String callToString(String arg, TExpr type)
	{
		if (isVoid(type)) {
			return "0";
		}
		if (couldBeLegitimatelyNull(type)) {
			return "(" + arg + " == null ? \"None\" : (\"Just: \" + " + arg + ".toString()))";
		}
		PrimitiveSpec ps = getPrimitive(type);
		if (ps != null) {
			return ps.hashCode(arg);
		}
		return arg + ".toString()";
	}

	private String handlerVariant(TExpr type, FuncExprMapper mapper)
	{
		if (isPrimitive(type)) return mapper.handlerFuncPrimitive;
		return mapper.handlerFunc;
	}

    private String handlerVariantCall(TExpr type, FuncExprMapper mapper)
        throws ProblemException
    {
        return mapExpr(mapper, type) + "." + handlerVariant(type, mapper) + "(" + mapper.getArgs() + ")";
    }
}

