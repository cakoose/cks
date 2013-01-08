package cks.type.model;

import cks.io.SourcePos;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

public final class TDef extends TBinding
{
	public final SourcePos loc;
	public final int index; // All TDefs in a module are contiguously indexed.
	public final LinkedHashMap<String,TParam> params; // If 'null', no type params.
	public final TDesc desc;
	private TDef outer;

	public final TKind kind;

	public TDef(String name, SourcePos loc, int index, LinkedHashMap<String,TParam> params, TDesc desc)
	{
		super(name);
		this.loc = loc;
		this.index = index;
		this.params = params;
		this.desc = desc;

		if (params == null) {
			this.kind = TKind.Base;
		}
		else {
			TKind[] paramKinds = new TKind[params.size()];
			Iterator<TParam> paramI = params.values().iterator();
			for (int i = 0; i < paramKinds.length; i++) {
				assert paramI.hasNext();
				paramKinds[i] = paramI.next().getKind();
			}
			this.kind = new TKind.Func(paramKinds);
		}
	}

	public TDef getOuter()
	{
		return outer;
	}

	public void setOuter(TDef outer)
	{
		assert this.outer == null;
		this.outer = outer;
	}

	public TKind getKind()
	{
		return kind;
	}

	private final List<TDef> children = new ArrayList<TDef>();
	public void addChild(TDef child) { children.add(child); }
	public List<TDef> getChildren() { return children; }

	private boolean isResolved = false;
	public final boolean isResolved() { return isResolved; }
	public void setResolved() { this.isResolved = true; }
}
