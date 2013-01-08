namespace Cks.Binary.Reader {

public abstract class BinaryReader<T>
{
	public abstract T ReadImpl(System.IO.BinaryReader In);

	public T Read(System.IO.BinaryReader In)
	{
		T v = ReadImpl(In);
		if (In.PeekChar() != -1) {
			throw new BinaryFormatException("encountered extra data (after the value)");
		}
		return v;
	}

	public T ReadPartial(System.IO.BinaryReader In)
	{
		return ReadImpl(In);
	}
}

} // namespace
