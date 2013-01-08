package cks.type.model;

import cks.io.SourcePos;

import java.util.LinkedHashMap;

public class TRecord extends TCompound
{
  public TRecord(SourcePos location, TExpr parentRef, LinkedHashMap<String,TMember> members, TMember implicit, LinkedHashMap<String,TDef> defs)
  {
    super(location, parentRef, members, implicit, defs);
  }

	// properties ---------------------------------------------------

  private TRecord parent;
  protected TRecord getParent() { return this.parent; }
  protected void setParent(TCompound parent) { this.parent = (TRecord) parent; }

	public String getCategory() { return "record"; }
  //public void accept(TVisitor v) { v.visit(this); }
}
