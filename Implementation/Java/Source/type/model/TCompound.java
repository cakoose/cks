package cks.type.model;

import cks.io.SourcePos;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

public abstract class TCompound extends TDesc
{
	public final SourcePos loc;
	public final TExpr parentRef;
	public final LinkedHashMap<String,TMember> localMembers;
	public final TMember implicit;
	public final Map<String,TDef> defs;

	public LinkedHashMap<String,TMember> allMembers;
	public TMember[] allMembersArray;

	public abstract String getCategory();

	public TCompound(SourcePos loc, TExpr parentRef, LinkedHashMap<String,TMember> localMembers, TMember implicit, LinkedHashMap<String,TDef> defs)
	{
		this.loc = loc;
		this.parentRef = parentRef;
		this.localMembers = localMembers;
		this.implicit = implicit;
		this.defs = defs;

		assert implicit == null || localMembers.values().contains(implicit);
	}

	private int memberIndexStart = -1;

	public int getMemberIndexStart()
	{
		assert memberIndexStart >= 0;
		return memberIndexStart;
	}

	public void setMemberIndexStart(int memberIndexStart)
	{
		assert this.memberIndexStart == -1;
		assert memberIndexStart >= 0;
		this.memberIndexStart = memberIndexStart;
	}

	public TMember getMember(int index)
	{
		if (index < 0 || index >= allMembersArray.length) throw new IllegalArgumentException("index out of range: " + index);
		return allMembersArray[index];
	}
}
