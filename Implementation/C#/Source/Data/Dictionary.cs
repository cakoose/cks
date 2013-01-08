using C = System.Collections.Generic;

namespace Cks.Data {

// Same as System.Collections.Generic.Dictionary except Equals() and
// GetHashCode() actually work.  How is it possible to be that stupid?
public sealed class Dictionary<K,V> : C.Dictionary<K,V> 
{
	public Dictionary()
	{
	}

	public override bool Equals(object o)
	{
		return (this.GetType() == o.GetType()) && Equals(this, (Dictionary<K,V>)o);
	}

	private static bool Equals(C.Dictionary<K,V> a, C.Dictionary<K,V> b)
	{
		if (a.Count != b.Count) return false;
		foreach (K k in a.Keys) {
			V bVal;
			bool bHas = b.TryGetValue(k, out bVal);
			if (!bHas) return false;
			V aVal = a[k];
			if (!aVal.Equals(bVal)) return false;
		}
		return true;
	}

	public override int GetHashCode()
	{
		int h = 0;
		foreach (K k in this.Keys) {
			V v = this[k];
			h ^= k.GetHashCode() + v.GetHashCode();
		}
		return h;
	}
}

}
