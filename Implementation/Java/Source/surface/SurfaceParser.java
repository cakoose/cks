package cks.surface;

import cks.io.CksTextTokenizer;
import cks.io.Codes;
import cks.io.Token;
import cks.io.ProblemException;
import static cks.io.ProblemException.pex;
import cks.io.SourcePos;
import cks.surface.model.*;

import java.util.Iterator;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

import java.io.IOException;

public final class SurfaceParser
{
	private final CksTextTokenizer tm;

	private SurfaceParser(CksTextTokenizer tm)
	{
		this.tm = tm;
	}

	public static Value parse(CksTextTokenizer tm)
		throws IOException, ProblemException
	{
		return new SurfaceParser(tm).parse();
	}

	private Value parse()
		throws ProblemException, IOException
	{
		Value value = pValue();
		expect(Codes.Eof);
		return value;
	}

	private Value pValue()
		throws ProblemException, IOException
	{
		Token t = peek();
		if (t.type == Codes.Ident) {
			return pVariant();
		}
		else if (t.type == '{') {
			return pRecord();
		}
		else if (t.type == '[') {
			return pCollection();
		}
		else if (t.type == '<') {
			next();
			return pTag(t);
		}
		else if (t.type == '(') {
			next();
			expect(')');
			return new VPrimitive.Void(t.loc);
		}
		else if (t.type == Codes.LitString) {
			next();
			return new VPrimitive.String(t.loc, ((Token.LitString)t).value);
		}
		else if (t.type == Codes.LitInt) {
			next();
			return new VPrimitive.Int(t.loc, ((Token.LitInt)t).value);
		}
		else {
			throw expectFail(t, "the start of a new value");
		}
	}

	private Value pVariant()
		throws ProblemException, IOException
	{
		Token t = take();
		assert t.type == Codes.Ident;
		Token.Ident label = (Token.Ident) t;

		String name = label.text;

		Value value;
		if (peek().type == ':') {
			// Value follows.  Parse it.
			next();
			value = pValue();
		}
		else {
			// No value specified.
			value = new VPrimitive.Void(label.loc);
		}

		return new VEntry(label.loc, name, value);
	}

	private Value pRecord()
		throws ProblemException, IOException
	{
		Token open = take();
		assert open.type == '{' : open;

		Map<String,VEntry> fields = new LinkedHashMap<String,VEntry>();
		pEntries(fields);

		Token close = take();
		if (close.type != '}') {
			throw expectFail(close, "either more entries or a closing brace for record",
				open.loc, "opening brace");
		}

		return new VRecord(open.loc, fields);
	}

	private void pEntries1(Map<String,VEntry> entries, Token.Ident id)
		throws ProblemException, IOException
	{

		assert id.type == Codes.Ident;

		Token eq = take();
		assert eq.type == '=';
		Value value = pValue(); // entry value

		addEntry(entries, id.loc, id.text, value);

		boolean needDivider = true;
		if (peek().type == ',') {
			next();
			needDivider = false;
		}

		pEntries(entries, needDivider);
	}

	private void pEntries(Map<String,VEntry> entries)
		throws ProblemException, IOException
	{
		pEntries(entries, false);
	}

	private void pEntries(Map<String,VEntry> entries, boolean needDivider)
		throws ProblemException, IOException
	{
		while (true) {
			Token t = peek();
			boolean hasMore = t.type == Codes.Ident || t.type == '<';
			if (!hasMore) break;

			if (needDivider && !tm.lastWhitespaceHadEol()) {
				throw pex(t.loc, "multiple entries on the same line need to be separated by commas");
			}

			next();
			SourcePos fieldNameLoc;
			String fieldName;
			Value value;

			if (t.type == Codes.Ident) {
				// Normal syntax: "Key = Value"
				Token.Ident id = (Token.Ident) t;
				fieldNameLoc = id.loc;
				fieldName = id.text;
				expect('=');
				value = pValue(); // entry value

				if (peek().type == ',') {
					next();
					needDivider = false;
				} else {
					needDivider = true;
				}
			}
			else if (t.type == '<') {
				// Tag syntax: <FieldName>...</FieldName>
				VTag.Tag tag = pTag(t);
				if (!(tag instanceof VTag.Full)) {
					throw pex(tag.loc, "tags used as record fields must have a tag name");
				}
				VTag.Full full = (VTag.Full) tag;
				fieldNameLoc = full.loc;
				fieldName = full.name;
				value = full.rest;

				if (peek().type == ',') {
					next();
				}
				needDivider = false;
			}
			else {
				// We should have already checked to make sure this can't happen.
				throw new AssertionError("internal error: unexpected token: " + t);
			}

			addEntry(entries, fieldNameLoc, fieldName, value);

		}
	}

	private void addEntry(Map<String, VEntry> entries, SourcePos loc, String name, Value value)
		throws ProblemException
	{
		VEntry displaced = entries.put(name, new VEntry(loc, name, value));

		// make sure this entry hasn't been defined already
		if (displaced != null) {
			throw ProblemException.pex(loc, "duplicate definition of \"" + name + "\"",
				displaced.loc, "original definition");
		}
	}

	private Value pCollection()
		throws ProblemException, IOException
	{
		Token open = take();
		assert open.type == '[' : open;

		List<VCollection.Element> elements = new ArrayList<VCollection.Element>();
		pCollectionElements(elements);

		Token close = take();
		if (close.type != ']') {
			throw expectFail(close, "either more elements or a closing bracket for list",
				open.loc, "opened");
		}

		return new VCollection(open.loc, elements);
	}

	private void pCollectionElements(List<VCollection.Element> elements)
		throws ProblemException, IOException
	{
		boolean needDivider = false;

		while (true) {

			boolean hasMore = peek().type != ']';
			if (!hasMore) break;

			if (needDivider && !tm.lastWhitespaceHadEol()) {
				throw pex(peek().loc, "multiple list elements on the same line need to be separated by commas");
			}

			// Collection element
			Value first = pValue();
			VCollection.Element e;

			if (peek().type == Codes.RightArrow) {
				// There's more: "->" Value
				Token arrow = take();
				Value second = pValue();
				e = new VCollection.Element(first, arrow.loc, second);
			} else {
				e = new VCollection.Element(first, null, null);
			}
			elements.add(e);

			if (peek().type == ',') {
				next();
				needDivider = false;
			} else {
				needDivider = true;
			}
		}
	}

	private VTag.Tag pTag(Token open)
		throws ProblemException, IOException
	{
		return pTag(open, new ArrayList<Token.Ident>());
	}

	private VTag.Tag pTag(Token open, List<Token.Ident> names)
		throws ProblemException, IOException
	{
		assert open.type == '<';


		Map<String,VEntry> attrs = new LinkedHashMap<String,VEntry>();

		Token t = peek();
		if (t.type == Codes.Ident) {
			next();
			Token.Ident id = (Token.Ident) t;
			names.add(id);
			// See if it's a field=value thing or just a option thing.
			if (peek().type != '=') {
				if (peek().type == ':') {  // The colon is optional
					next();
				}
				VTag.Tag rest = pTag(open, names);
				return new VTag.Full(t.loc, id.text, rest);
			}
			else {
				pEntries1(attrs, id);
			}
		}
		else {
			pEntries(attrs);
		}

		t = take();
		if (t.type == '>') {
			// Opening tag is finished.  Continue with tag body.
			List<VTag.Child> children = pTagChildren(open, names);
			return new VTag.Body(t.loc, attrs, new VTag.Content(t.loc, children));
		}
		else if (t.type == '/') {
			Token t2 = take();
			if (t2.type != '>') {
				throw expectFail(t, "either an identifier or \">\" or \"/>\"");
			}
			// Leaf tag.
			return new VTag.Body(t2.loc, attrs, new VTag.Content(t.loc, Collections.<VTag.Child>emptyList()));
		}
		else {
			throw expectFail(t, "either an identifier or \">\" or \"/>\"");
		}
	}

	private List<VTag.Child> pTagChildren(Token open, List<Token.Ident> names)
		throws ProblemException, IOException
	{
		// Tag contents (the stuff between the open and close tag)
		List<VTag.Child> children = new ArrayList<VTag.Child>();

		while (true) {
			Token.Text tagContent = tm.takeTagContent();

			if (tagContent != null) {
				children.add(new VTag.Text(tagContent.loc, tagContent.value));
			}

			Token t = peek();
			if (t.type == '<') {
				t = take();
				if (peek().type == '/') {
					next();
					break;
				} else {
					children.add(pTag(t));
				}
			}
			else {
				assert false : t; // There shouldn't be anything else here.
				throw new RuntimeException("internal error: unexpected token: " + t);
			}
		}

		Iterator<Token.Ident> namesI = names.iterator();
		while (true) {
			Token t = peek();
			if (t.type != Codes.Ident) break;
			next();
			Token.Ident id = (Token.Ident) t;
			if (!namesI.hasNext()) {
				throw pex(t.loc, "closing tag uses \"" + id.text + "\" but opening tag doesn't",
					open.loc, "opening tag");
			}

			Token.Ident expectedName = namesI.next();
			if (!id.text.equals(expectedName.text)) {
				throw pex(t.loc, "closing tag uses \"" + id.text + "\" where \"" + expectedName.text + "\" is expected",
					expectedName.loc, "opening tag");
			}

			if (peek().type == ':') {
				next();
			}
			else if (peek().type == '>') {
				break;
			}
		}

		expect('>');

		return children;
	}

	// ------------------------------------------------

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

	private static ProblemException expectFail(Token found, String thing)
	{
		StringBuilder message = new StringBuilder();
		message.append("expecting ").append(thing);
		message.append(", found ");
		found.toStringContent(message);
		return pex(found.loc, message.toString());
	}

	private static ProblemException expectFail(Token found, String thing, SourcePos source2, String message2)
	{
		StringBuilder message = new StringBuilder();
		message.append("expecting ").append(thing);
		message.append(", found ");
		found.toStringContent(message);
		return pex(found.loc, message.toString(), source2, message2);
	}
}
