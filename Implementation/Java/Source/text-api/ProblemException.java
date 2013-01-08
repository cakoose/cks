package cks.io;

import java.util.Arrays;
import java.util.List;

public class ProblemException extends Exception
{
	public final List<Problem> problems;
	public ProblemException(Problem problem) { this.problems = Arrays.asList(problem); }
	public String getMessage() { return problems.get(0).toString(); } // Return the first problem.

	public static ProblemException pex(SourcePos loc, String message) { return new ProblemException(new Problem(loc, message)); }
	public static ProblemException pex(SourcePos loc, String message, SourcePos l2, String m2) { return new ProblemException(new Problem(loc, message, l2, m2)); }
}
