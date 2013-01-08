package cks.type.parser;

import cks.io.*;
import cks.type.model.*;
import static cks.io.ProblemException.pex;

import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;

import java.io.IOException;

public final class TypeParser
{
	private CksTextTokenizer tm;
	private int defIndex;

	private TypeParser(CksTextTokenizer tm)
	{
		this.tm = tm;
		this.defIndex = 0;
	}

	public static TModule parse(CksTextTokenizer ts)
		throws IOException, ProblemException
	{
		return new TypeParser(ts).parse();
	}

	private TModule parse()
		throws ProblemException, IOException
	{
		Map<String,TDef> type = pDefs();
		expect(Codes.Eof);
		return new TModule(type, defIndex);
	}

	private Map<String,TDef> pDefs()
		throws ProblemException, IOException
	{
		Map<String,TDef> defs = new LinkedHashMap<String,TDef>();

		// Need at least one TDef at the top level.
		while (true) {
			Token token = take();
			if (token.type != Codes.Ident) {
				throw pex(token.loc, "expecting a type definition starting with \"def\"");
			}
			Token.Ident ident = (Token.Ident) token;
			if (!ident.text.equals("def")) {
				throw pex(token.loc, "expecting a type definition starting with \"def\"");
			}
			if (peek().type != Codes.Ident) {
				throw pex(peek().loc, "expecting a type name after \"def\"");
			}
			pDef(ident, defs);
			if (peek().type == Codes.Eof) break;
		}

		return defs;
	}

	private TDesc pDesc()
		throws ProblemException, IOException
	{
		TDesc desc;

		Token token = peek();
		switch (token.type) {

			case '<': {
				desc = pVariant(null);
				break;
			}

			case '{': {
				desc = pRecord(null);
				break;
			}

			default: {
				TExpr base = pExpr();
				if (peek().type == '+') {
					next();
					// Inheritance.
					switch(peek().type) {
						case '<': {
							desc = pVariant(base);
							break;
						}
						case '{': {
							desc = pRecord(base);
							break;
						}
						default:
							throw pex(peek().loc, "expecting a variant or record specification after '+'");
					}
				}
				else {
					desc = base;
				}
			}
		}

		return desc;
	}

	private TExpr pExpr()
		throws ProblemException, IOException
	{
		TExpr expr;

		Token token = peek();
		int type = token.type;
		if (type == '(') {
			next();
			expect(')');
			expr = new TExpr(new TExpr.Ref.Direct(token.loc, TOpaque.Void), null);
		}
		else if (type == Codes.Ident) {
			Token.Ident id = (Token.Ident) token;
			TExpr[] args = null;
			next();

			// Type arguments.
			if (peek().type == '(') {
				Token start = take();
				if (peek().type == ')') {
					throw pex(start.loc, "cannot have an empty type argument list");
				}
				List<TExpr> argList = new ArrayList<TExpr>();
				while (true) {
					argList.add(pExpr());
					if (peek().type == ')') {
						next();
						break;
					}
					expect(',');
				}
				args = argList.toArray(new TExpr[argList.size()]);
			}

			expr = new TExpr(new TExpr.Ref.Ident(token.loc, id.text), args);
		}
		else if (type == '?') {
			next();
			expr = new TExpr(new TExpr.Ref.Direct(token.loc, TOpaque.Maybe), null);
		}
		else {
			throw pex(token.loc, "expecting a type expression");
		}

		// The "maybe" type suffix.
		while (peek().type == '?') {
			Token t = take();
			expr = new TExpr(new TExpr.Ref.Direct(t.loc, TOpaque.Maybe), new TExpr[] {expr});
		}

		return expr;
	}

	private TRecord pRecord(TExpr parentRef)
		throws ProblemException, IOException
	{
		return pCompound('{', '}', CompoundFactory.Record, parentRef, false);
	}

	private TVariant pVariant(TExpr parentRef)
		throws ProblemException, IOException
	{
		return pCompound('<', '>', CompoundFactory.Variant, parentRef, true);
	}

	private void pDef(Token.Ident deftok, Map<String,TDef> defs)
		throws ProblemException, IOException
	{
		assert deftok.type == Codes.Ident ;
		assert deftok.text.equals("def") ;

		Token t = take();
		assert t.type == Codes.Ident;
		Token.Ident name = (Token.Ident) t;

		LinkedHashMap<String,TParam> params = pDeclareParams();

		expect('=');

		TDesc desc = pDesc();
		TDef def = new TDef(name.text, deftok.loc, this.defIndex++, params, desc);

		TDef displaced = defs.put(name.text, def);
		if (displaced != null) {
			throw pex(name.loc, "duplicate type definition \"" + name.text + "\"",
				displaced.loc, "original definition");
		}
	}

	private LinkedHashMap<String,TParam> pDeclareParams()
		throws ProblemException, IOException
	{
		if (peek().type != '(') return null;
		next();

		LinkedHashMap<String,TParam> params = new LinkedHashMap<String,TParam>();

		if (peek().type == ',') next();

		while (peek().type != ')') {
			Token.Ident name = expectIdent();

			// TODO: higher-kinded type parameters
			TParam newGuy = new TParam(name.loc, name.text, TKind.Base);
			TParam displaced = params.put(name.text, newGuy);

			if (displaced != null) {
				throw pex(name.loc, "duplicate parameter \"" + name.text + "\"",
					displaced.loc, "original parameter");
			}

			if (peek().type == ')') break;
			expect(',');
		}
		next();

		return params;
	}

	private static abstract class CompoundFactory<T extends TCompound> {
		public abstract T make(SourcePos loc, TExpr parentRef, LinkedHashMap<String,TMember> members, TMember implicit, LinkedHashMap<String,TDef> defs);

		public static final CompoundFactory<TRecord> Record = new CompoundFactory<TRecord>() {
			public TRecord make(SourcePos loc, TExpr parentRef, LinkedHashMap<String,TMember> members, TMember implicit, LinkedHashMap<String,TDef> defs)
			{ return new TRecord(loc, parentRef, members, implicit, defs); }
		};
		public static final CompoundFactory<TVariant> Variant = new CompoundFactory<TVariant>() {
			public TVariant make(SourcePos loc, TExpr parentRef, LinkedHashMap<String,TMember> members, TMember implicit, LinkedHashMap<String,TDef> defs)
			{ return new TVariant(loc, parentRef, members, implicit, defs); }
		};
	}

	private <T extends TCompound>
	T pCompound(int startMark, int endMark, CompoundFactory<? extends T> factory, TExpr parentRef, boolean allowImplicitVoid)
		throws ProblemException, IOException
	{
		Token first = take();
		assert first.type == startMark;

		LinkedHashMap<String,TMember> members = new LinkedHashMap<String,TMember>();
		LinkedHashMap<String,TDef> defs = new LinkedHashMap<String,TDef>();
		TMember implicit = null; // The "implicit" member (for mixed container)

		boolean needDivider = false;

		// The list of div-separated members.
		while (peekNoEof().type != endMark) {
			if (needDivider && !tm.lastWhitespaceHadEol()) {
				throw pex(peek().loc, "multiple entries on the same line need to be separated by commas");
			}

			Token.Ident name = expectIdent();

			// Named type definition
			if (name.text.equals("def") && peek().type == Codes.Ident) {
				pDef(name, defs);
			}
			// Member entry
			else {
				TExpr type;
				boolean thisMemberIsImplicit = false;

				// Marker for the "implicit" member.
				if (peek().type == '*') {
					if (implicit != null) {
						throw pex(name.loc, "extra implicit member marker on \"" + name.text + "\"",
							implicit.loc, "first marker");
					}
					next();
					thisMemberIsImplicit = true;
				}

				if (peek().type == ':') {
					// If there's a ':' then parse the type.
					next();
					type = pExpr();
				} else if (allowImplicitVoid) {
					// Defaults to '()' (Void)
					type = new TExpr(new TExpr.Ref.Direct(name.loc, TOpaque.Void), null);
				} else {
					throw pex(peek().loc, "expecting \":\" followed by a type expression");
				}

				TMember newMember = new TMember(name.loc, name.text, type);
				TMember displaced = members.put(name.text, newMember);
				if (displaced != null) {
					throw pex(name.loc, "duplicate member \"" + name.text + "\"",
						displaced.loc, "original definition");
				}

				if (thisMemberIsImplicit) implicit = newMember;
			}

			if (peek().type == ',') {
				next();
				needDivider = false;
			} else {
				needDivider = true;
			}
		}

		next();

		return factory.make(first.loc, parentRef, members, implicit, defs);
	}

	private Token take()
		throws IOException, ProblemException
	{
		Token token = tm.take();
		return token;
	}

	private Token peek()
		throws IOException, ProblemException
	{
		Token token = tm.peek();
		return token;
	}

	private Token peekNoEof()
		throws IOException, ProblemException
	{
		Token token = tm.peek();
		if (token.type == Codes.Eof) throw pex(token.loc, "unexpected end of file");
		return token;
	}

	private void next()
		throws IOException, ProblemException
	{
		tm.next();
	}

	private Token.Ident expectIdent()
		throws IOException, ProblemException
	{
		return tm.expectIdent();
	}

	private Token expect(int type)
		throws IOException, ProblemException
	{
		return tm.expect(type);
	}

	/*
	private TExpr tokenDump()
		throws ProblemException, IOException
	{
		while (true) {
			Token token = tm.take();
			System.out.println("# " + token);
			if (token.type == Codes.EOF) break;
		}
		return null;
	}
	*/
}
