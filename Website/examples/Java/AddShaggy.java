import org.example.students.*; // the generated classes
import cks.value.binary.reader.BinaryReader;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;

public class AddShaggy
{
	public static void main(String[] args)
		throws IOException
	{
		if (args.length != 1) {
			System.err.println("Expecting exactly one argument.");
			return;
		}
		String file = args[0];

		// Read in the list of students.
		List<Student> students = read(file);
		if (students == null) System.exit(1);

		// Append Shaggy to the list.
		Student shaggy =
			new Student("Shaggy Rogers", 42, Sex.Male_());
		students.add(shaggy);

		// Write the updated list out to the same file.
		write(file, students);
	}

	public static List<Student> read(String file)
		throws IOException
	{
		FileInputStream in = new FileInputStream(file);
		try {
			// The main API call to read CKS data.
			return Students._BinaryReader.read(in);
		} catch (BinaryReader.FormatException ex) {
			System.err.println("Format error: " + ex.getMessage());
			return null;
		} finally {
			in.close();
		}
	}

	public static void write(String file, List<Student> students)
		throws IOException
	{
		FileOutputStream out = new FileOutputStream(file);
		try {
			// The main API call to write CKS data.
			Students._BinaryWriter.write(out, students);
		} finally {
			out.close();
		}
	}
}
