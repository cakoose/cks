using BigNatural = Mono.Math.BigInteger;

namespace Cks.Data {

/**
 * The Mono.Math.BigInteger class only handles positive integers.  They
 * shoulda called it BigNatural.
 *
 * This handles negatives by wrapping Mono.Math.BigInteger and adding
 * a sign bit.
 *
 * TODO: This is an inefficient placeholder I created so I could get the
 * rest of CKS working.  Need to replace.
 */
public class BigInteger
{
	public readonly bool IsPositive;
	public readonly BigNatural Magnitude;

	public BigInteger(bool IsPositive, BigNatural Magnitude)
	{
		this.IsPositive = IsPositive;
		this.Magnitude = Magnitude;
	}

	public override string ToString()
	{
		if (IsPositive) {
			return Magnitude.ToString();
		} else if (Magnitude == 0) {
			return "0";
		} else {
			return "-" + Magnitude.ToString();
		}
	}

	public static readonly BigInteger Zero = new BigInteger(true, 0);
	public static readonly BigInteger One = new BigInteger(true, 1);

	public override bool Equals(object o)
	{
		if (o.GetType() != this.GetType()) return false;
		BigInteger i = (BigInteger) o;
		return (this == i);
	}

	public override int GetHashCode()
	{
		if (Magnitude == 0) return 0;
		int h = Magnitude.GetHashCode();
		if (!IsPositive) h = -h;
		return h;
	}

	public static BigInteger mk(int Value)
	{
		if (Value > 0) {
			return new BigInteger(true, new BigNatural((uint) Value));
		}
		else {
			uint Pos = (uint) -Value;
			return new BigInteger(false, new BigNatural(Pos));
		}
	}

	public static BigInteger operator+(BigInteger a, BigInteger b)
	{
		if (a.IsPositive == b.IsPositive) {
			return new BigInteger(a.IsPositive, a.Magnitude + b.Magnitude);
		}
		else if (a.Magnitude > b.Magnitude) {
			return new BigInteger(a.IsPositive, a.Magnitude - b.Magnitude);
		}
		else {
			return new BigInteger(b.IsPositive, b.Magnitude - a.Magnitude);
		}
	}

	public static BigInteger operator+(BigInteger a, int b)
	{
		bool IsPositive;
		uint Magnitude;
		if (b > 0) {
			IsPositive = true;
			Magnitude = (uint) b;
		} else {
			IsPositive = false;
			Magnitude = (uint) -b;
		}

		if (a.IsPositive == IsPositive) {
			return new BigInteger(a.IsPositive, a.Magnitude + Magnitude);
		}
		else if (a.Magnitude > Magnitude) {
			return new BigInteger(a.IsPositive, a.Magnitude - Magnitude);
		}
		else {
			return new BigInteger(IsPositive, Magnitude - a.Magnitude);
		}
	}

	public static BigInteger operator-(BigInteger a, BigInteger b)
	{
		BigInteger nb = new BigInteger(!b.IsPositive, b.Magnitude);
		return a + nb;
	}

	public static BigInteger operator*(BigInteger a, int b)
	{
		bool IsPositive;
		uint Magnitude;
		if (b > 0) {
			IsPositive = true;
			Magnitude = (uint) b;
		} else {
			IsPositive = false;
			Magnitude = (uint) -b;
		}

		return new BigInteger(a.IsPositive == IsPositive, a.Magnitude * Magnitude);
	}

	public static bool operator<(BigInteger a, BigInteger b) { return Compare(a, b) < 0; }
	public static bool operator<=(BigInteger a, BigInteger b) { return Compare(a, b) <= 0; }
	public static bool operator>(BigInteger a, BigInteger b) { return Compare(a, b) > 0; }
	public static bool operator>=(BigInteger a, BigInteger b) { return Compare(a, b) >= 0; }
	public static bool operator==(BigInteger a, BigInteger b) { return Compare(a, b) == 0; }
	public static bool operator!=(BigInteger a, BigInteger b) { return Compare(a, b) != 0; }

	public static int Compare(BigInteger a, BigInteger b)
	{
		if (a.Magnitude == b.Magnitude) {
			if (a.Magnitude == 0) return 0;
			if (a.IsPositive == b.IsPositive) return 0;
		}
		
		if (a.IsPositive) {
			if (b.IsPositive) {
				return (a.Magnitude > b.Magnitude) ? 1 : -1;
			} else {
				return 1;
			}
		} else {
			if (b.IsPositive) {
				return -1;
			} else {
				return (a.Magnitude > b.Magnitude) ? -1 : 1;
			}
		}
	}
}

}
