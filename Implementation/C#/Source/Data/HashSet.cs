using C = System.Collections.Generic;

namespace Cks.Data {

// Same as System.Collections.Generic.HashSet except Equals() and
// GetHashCode() actually work.  How is it possible to be that stupid?
public sealed class HashSet<T> : C.HashSet<T> 
{
	public HashSet()
	{
	}

	public override bool Equals(object o)
	{
		return (this.GetType() == o.GetType()) && Equals(this, (HashSet<T>)o);
	}

	private static bool Equals(C.HashSet<T> a, C.HashSet<T> b)
	{
		if (a.Count != b.Count) return false;
		foreach (T e in a) {
			if (!b.Contains(e)) return false;
		}
		return true;
	}

	public override int GetHashCode()
	{
		int h = 0;
		foreach (T e in this) {
			h ^= e.GetHashCode();
		}
		return h;
	}
}

}

