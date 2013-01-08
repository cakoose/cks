namespace Cks.Text.Reader.Model {

public sealed class ProblemException : System.Exception
{
	public readonly Problem Problem;
	public ProblemException(Problem Problem) { this.Problem = Problem; }
	public override string ToString() { return Problem.ToString(); }
}

}
