using C = Cks.Data;
using TextParser = Cks.Text.Reader.Parser.TextParser;

namespace Cks.Text.Reader {

public abstract class TextReader<T>
{
	public abstract T Marshal(Cks.Text.Reader.Model.Value Value);
	public virtual C.Maybe<T> Default { get { return new C.Maybe<T>.Nothing(); } }

	// -----------------------------------------------
	// Convenience functions for clients.

	public T Read(System.IO.TextReader In)
	{
		return Marshal(TextParser.Parse(In));
	}

	public T Read(System.IO.Stream In)
	{
		using (System.IO.StreamReader s = new System.IO.StreamReader(In)) {
			return Read(s);
		}
	}

	public T Read(byte[] Data)
	{
		using (System.IO.MemoryStream s = new System.IO.MemoryStream(Data)) {
			return Read(s);
		}
	}
}

} // namespace
