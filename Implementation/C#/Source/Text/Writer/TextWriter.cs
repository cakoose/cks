namespace Cks.Text.Writer {

public abstract class TextWriter<T>
{
	public abstract void Write(System.IO.TextWriter Out, T Value);
	public virtual void WriteIndented(IndentPrinter Out, T Value)
	{
		Write(Out, Value);
		Out.WriteLine();
	}

	public void Write(System.IO.Stream Out, T Value)
	{
		using (System.IO.StreamWriter s = new System.IO.StreamWriter(Out)) {
			Write(s, Value);
		}
	}

	public string WriteToString(T Value)
	{
		using (System.IO.StringWriter s = new System.IO.StringWriter()) {
			Write(s, Value);
			return s.ToString();
		}
	}

	public byte[] WriteToByteArray(T Value)
	{
		using (System.IO.MemoryStream s = new System.IO.MemoryStream()) {
			Write(s, Value);
			return s.ToArray();
		}
	}
}

}
