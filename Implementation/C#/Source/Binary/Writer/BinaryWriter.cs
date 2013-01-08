namespace Cks.Binary.Writer {

public abstract class BinaryWriter<T>
{
	public abstract void Write(System.IO.BinaryWriter Out, T Value);
}

} // namespace

