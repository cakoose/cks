using Example.Students; // the generated classes

using Cks.Data;
using BinaryFormatException = Cks.Binary.Reader.BinaryFormatException;

using Console = System.Console;
using System.IO;

public class AddShaggy
{
	public static void Main(string[] Args)
	{
		if (Args.Length != 1) {
			Console.Error.WriteLine("Expecting exactly one argument.");
			return;
		}
		string Path = Args[0];

		// Read in the list of students.
		List<Student> Students = Read(Path);
		if (Students == null) System.Environment.Exit(1);

		// Append Shaggy to the list.
		Student Shaggy =
			new Student("Shaggy Rogers", 42, new Sex.Male_());
		Students.Add(Shaggy);

		// Write the updated list out to the same file.
		Write(Path, Students);
	}

	public static List<Student> Read(string Path)
	{
		FileStream fs = File.OpenRead(Path);
		using (BinaryReader In = new BinaryReader(fs)) {
			try {
				// The main API call to read CKS data.
				return Students._BinaryReader.Read(In);
			}
			catch (BinaryFormatException e) {
				Console.Error.WriteLine("Format error: " + e.Message);
				return null;
			}
		}
	}

	public static void Write(string Path, List<Student> Value)
	{
		FileStream fs = File.Open(Path, FileMode.Create, FileAccess.Write);
		using (BinaryWriter Out = new BinaryWriter(fs)) {
			// The main API call to write CKS data.
			Students._BinaryWriter.Write(Out, Value);
		}
	}
}
