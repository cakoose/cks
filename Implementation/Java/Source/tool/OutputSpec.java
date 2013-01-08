package cks.tool;

import cakoose.util.Either;

public final class OutputSpec
{
	public final Format format;
	public final String file;

	public enum Format { Text, TextIndented, Binary, }

	public OutputSpec(Format format, String file)
	{
		this.format = format;
		this.file = file;
	}

	public static Either<OutputSpec,String> parse(String spec)
	{
		int colonPos = spec.indexOf(':');
		if (colonPos < 0) {
			return Either.Right("missing colon");
		}

		String format = spec.substring(0, colonPos);
		String file = spec.substring(colonPos+1);

		OutputSpec.Format f;

		if (format.equals("text")) {
			f = OutputSpec.Format.Text;
		}
		else if (format.equals("texti")) {
			f = OutputSpec.Format.TextIndented;
		}
		else if (format.equals("binary")) {
			f = OutputSpec.Format.Binary;
		}
		else {
			return Either.Right("\"" + format + "\" is not a valid output format");
		}

		return Either.Left(new OutputSpec(f, file));
	}

}
