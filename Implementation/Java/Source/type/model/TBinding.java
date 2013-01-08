package cks.type.model;

/**
 * A named type.
 */
public abstract class TBinding
{
	public final String name;

	public TBinding(String name)
	{
		if (name == null) throw new IllegalArgumentException("'name' can't be null");
		this.name = name;
	}

	public abstract TKind getKind();

	public String toString() { return name; }
}
