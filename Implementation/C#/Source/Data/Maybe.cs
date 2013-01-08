namespace Cks.Data {

public abstract class Maybe<T>
{
	public sealed class Just : Maybe<T>
	{
		public readonly T Value;
		public Just(T Value) { this.Value = Value; }

		public override int GetHashCode()
		{
			return 1 + Value.GetHashCode();
		}

		public override bool Equals(object _o)
		{
			Just o = _o as Just;
			if (o == null) return false;
			if (o.Value == null) return this.Value == null;
			if (this.Value == null) return false;
			return Value.Equals(o.Value);
		}
	}

	public sealed class Nothing : Maybe<T>
	{
		public override int GetHashCode()
		{
			return 0;
		}

		public override bool Equals(object o)
		{
			return (o is Nothing);
		}
	}
}

}
