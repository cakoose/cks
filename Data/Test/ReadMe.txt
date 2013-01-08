These files are used by the automated test framework.  The layout:
- Each sub-directory is a "test group"
- Each test group should contain a ".tcks" file whose name matches
  the name of the sub-directory.
- Every ".cks" file in each test group will be tested against the
  bindings generated for the ".tcks" file.
