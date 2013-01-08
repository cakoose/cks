package cks.type.model;

import cks.io.SourcePos;

import java.util.LinkedHashMap;

public class TVariant extends TCompound
{
  public TVariant(SourcePos location, TExpr parentRef, LinkedHashMap<String,TMember> members, TMember implicit, LinkedHashMap<String,TDef> defs)
  {
    super(location, parentRef, members, implicit, defs);
  }

	public String getCategory() { return "variant"; }
}
