using C = System.Collections.Generic;

namespace Cks.Data {

// Same as System.Collections.Generic.List except Equals() and
// GetHashCode() actually work.  How is it possible to be that stupid?
public sealed class List<T> : C.List<T> 
{
	public List() {}
	public List(int Size) : base(Size) {}

	public override bool Equals(object o)
	{
		return (this.GetType() == o.GetType()) && Equals(this, (List<T>)o);
	}

	private static bool Equals(C.List<T> a, C.List<T> b)
	{
		if (a.Count != b.Count) return false;

		C.List<T>.Enumerator ea = a.GetEnumerator();
		C.List<T>.Enumerator eb = b.GetEnumerator();

		while (ea.MoveNext()) {
			if (!eb.MoveNext()) return false;
			if (!ea.Current.Equals(eb.Current)) return false;
		}
		if (eb.MoveNext()) return false;
		return true;
	}

	public override int GetHashCode()
	{
		int h = 0;
		int i = 0; // make sure the element order affects the hash
		foreach (T e in this) {
			h ^= e.GetHashCode() + (i++);
		}
		return h;
	}
}

}
