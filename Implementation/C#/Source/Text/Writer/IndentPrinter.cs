using TW = System.IO.TextWriter;

namespace Cks.Text.Writer {

/**
 * Printer that manages indentation.
 */
public class IndentPrinter : TW
{
	private readonly TW Out;
	private readonly string PrefixString;
	private readonly string IndentString;
	private uint Level = 0;

	private bool nl;

	private static readonly string DefaultIndent = "  ";

	public IndentPrinter(TW Out, string IndentString, string PrefixString, bool FreshLine)
	{
		this.Out = Out;
		this.IndentString = IndentString;
		this.PrefixString = PrefixString;
		this.nl = FreshLine;
	}

	public IndentPrinter(TW Out, string IndentString, string PrefixString)
		: this(Out, IndentString, PrefixString, true) {}

	public IndentPrinter(TW Out, string IndentString)
		: this(Out, IndentString, "") {}

	public IndentPrinter(TW Out)
		: this(Out, DefaultIndent) {}

	public override System.Text.Encoding Encoding { get { return Out.Encoding; } }

	// -----------------------------------------------
	// Indentation Control
	
	public void Indent(uint NumIndents)
	{
		this.Level += NumIndents;
	}

	public void Indent()
	{
		this.Level += 1;
	}

	public void Dedent(uint NumIndents)
	{
		if (NumIndents > this.Level) throw new System.ArgumentException("attempt to decrase indent by " + NumIndents + ", but indent level is currently " + Level);
		this.Level -= NumIndents;
	}

	public void Dedent()
	{
		if (this.Level < 1) throw new System.ArgumentException("attempt to decrase indent, but indent level is currently " + Level);
		this.Level -= 1;
	}

	// -----------------------------------------------
	// Print Functions
	
	public override void Write(bool v) { Pre(); Out.Write(v); }
	public override void Write(char v) { Pre(); Out.Write(v); }
	public override void Write(char[] v) { Pre(); Out.Write(v); }
	public override void Write(decimal v) { Pre(); Out.Write(v); }
	public override void Write(double v) { Pre(); Out.Write(v); }
	public override void Write(int v) { Pre(); Out.Write(v); }
	public override void Write(long v) { Pre(); Out.Write(v); }
	public override void Write(object v) { Pre(); Out.Write(v); }
	public override void Write(float v) { Pre(); Out.Write(v); }
	public override void Write(string v) { Pre(); Out.Write(v); }
	public override void Write(uint v) { Pre(); Out.Write(v); }
	public override void Write(ulong v) { Pre(); Out.Write(v); }
	public override void Write(string v, object o) { Pre(); Out.Write(v, o); }
	public override void Write(string v, params object[] o) { Pre(); Out.Write(v, o); }
	public override void Write(char[] v, int a, int b) { Pre(); Out.Write(v, a, b); }
	public override void Write(string v, object a, object b) { Pre(); Out.Write(v, a, b); }
	public override void Write(string v, object a, object b, object c) { Pre(); Out.Write(v, a, b, c); }

	public override void WriteLine() { Pre(); Out.WriteLine(); nl = true; }
	
	public override void WriteLine(bool v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(char v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(char[] v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(decimal v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(double v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(int v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(long v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(object v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(float v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(string v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(uint v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(ulong v) { Pre(); Out.WriteLine(v); nl = true; }
	public override void WriteLine(string v, object o) { Pre(); Out.WriteLine(v, o); nl = true; }
	public override void WriteLine(string v, params object[] o) { Pre(); Out.WriteLine(v, o); nl = true; }
	public override void WriteLine(char[] v, int a, int b) { Pre(); Out.WriteLine(v, a, b); nl = true; }
	public override void WriteLine(string v, object a, object b) { Pre(); Out.WriteLine(v, a, b); nl = true; }
	public override void WriteLine(string v, object a, object b, object c) { Pre(); Out.WriteLine(v, a, b, c); nl = true; }

	private void Pre()
	{
		if (nl) {
			Out.Write(PrefixString);
			for (uint i = 0; i < this.Level; i++) {
				Out.Write(IndentString);
			}
			nl = false;
		}
	}
}

} // namespace
