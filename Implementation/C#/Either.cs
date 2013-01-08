namespace Cks.Data {

public abstract class Either<L,R>
{
	public sealed class Left : Either<L,R>
	{
		public readonly L Value;
		public Left(L Value) { this.Value = Value; }

		public override int GetHashCode()
		{
			return Value.GetHashCode();
		}

		public override bool Equals(object _o)
		{
			Left o = _o as Left;
			if (o == null) return false;
			if (o.Value == null) return this.Value == null;
			if (this.Value == null) return false;
			return Value.Equals(o.Value);
		}
	}

	public sealed class Right : Either<L,R>
	{
		public readonly R Value;
		public Right(R Value) { this.Value = Value; }

		public override int GetHashCode()
		{
			return Value.GetHashCode();
		}

		public override bool Equals(object _o)
		{
			Right o = _o as Right;
			if (o == null) return false;
			if (o.Value == null) return this.Value == null;
			if (this.Value == null) return false;
			return Value.Equals(o.Value);
		}
	}
}

}


