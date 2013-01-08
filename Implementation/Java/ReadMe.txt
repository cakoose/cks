===========================================
CKS Bindings Generator and CKS Java Runtime Library

Requirements:
- Java Runtime Environment (JRE) 5 (aka 1.5), or newer

Build Requirements:
- Java Development Kit (JDK) 5 (aka 1.5), or newer
- GNU Make 3.81 or newer

To build the JAR files, run:

  ./repackage

Output will be put in "Output/repackaged"

An IntelliJ 10 project directory is available in "Support/IntelliJ/".

---------------------------
CKS Bindings Generator

A tool that reads CKS type definitions and generates language-specific
code to handle that type.  Currently supports generating bindings for
Java, Haskell, and C#.

There's a shell script at the top level ("cks") that runs the bindings
generator.  Run it without any arguments to get usage instructions:

  ./cks

If you want it to be on your path, create a symlink to it from a
directory that's on your path.  For example:

   cd $HOME/bin
	ln -s ~/Cks/Implementation/Java/cks .

---------------------------
CKS Java Runtime Library.

Generated CKS bindings make use of functionality in the CKS Java
Runtime Library.  The JARs are in "Output/repackaged/"
