package cks.tool;

import cakoose.util.Either;

public final class InputSpec
{
	public final Format format;
	public final String file;

	public enum Format { Text, Binary, }

	public InputSpec(Format format, String file)
	{
		this.format = format;
		this.file = file;
	}

	public static Either<InputSpec,String> parse(String spec)
	{
		int colonPos = spec.indexOf(':');
		if (colonPos < 0) {
			return Either.Right("missing colon");
		}

		String format = spec.substring(0, colonPos);
		String file = spec.substring(colonPos+1);

		InputSpec.Format f;

		if (format.equals("text")) {
			f = InputSpec.Format.Text;
		}
		else if (format.equals("binary")) {
			f = InputSpec.Format.Binary;
		}
		else {
			return Either.Right("\"" + format + "\" is not a valid input format");
		}

		return Either.Left(new InputSpec(f, file));
	}

}
