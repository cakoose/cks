package cks.io;

import java.io.IOException;
import java.math.BigInteger;

public abstract class Parser
{
	public abstract SourcePos getLastIdentSourcePos();
    public abstract void endInput() throws ProblemException, IOException;

	// -----------------------------------------------
	// Record

	public abstract SourcePos beginRecord() throws ProblemException, IOException;
	public abstract String getField() throws ProblemException, IOException;

	// -----------------------------------------------
	// Variant

	public abstract String beginVariant() throws ProblemException, IOException;
	public abstract boolean hasVariantValue() throws ProblemException, IOException;
	public abstract void endVariant() throws ProblemException, IOException;

	// -----------------------------------------------
	// List/Set/Map

	public abstract void beginList() throws ProblemException, IOException;
	public abstract SourcePos hasListNext() throws ProblemException, IOException;

	public abstract void beginSet() throws ProblemException, IOException;
	public abstract SourcePos hasSetNext() throws ProblemException, IOException;

	public abstract void beginMap() throws ProblemException, IOException;
	public abstract SourcePos hasMapNext() throws ProblemException, IOException;
	public abstract SourcePos preMapValue() throws ProblemException, IOException;

	// -----------------------------------------------
	// Opaque Values

	public abstract BigInteger parseInt() throws ProblemException, IOException;
	public abstract BigInteger parseNat() throws ProblemException, IOException;

	public abstract byte parseInt8() throws ProblemException, IOException;
	public abstract short parseInt16() throws ProblemException, IOException;
	public abstract int parseInt32() throws ProblemException, IOException;
	public abstract long parseInt64() throws ProblemException, IOException;

	public abstract short parseNat8() throws ProblemException, IOException;
	public abstract int parseNat16() throws ProblemException, IOException;
	public abstract long parseNat32() throws ProblemException, IOException;
	public abstract BigInteger parseNat64() throws ProblemException, IOException;

	public abstract String parseString() throws ProblemException, IOException;
	public abstract boolean parseBool() throws ProblemException, IOException;
	public abstract void parseVoid() throws ProblemException, IOException;

	/**
	 * @return
	 *    null  -> It's a "None" value.
	 *    TRUE  -> It's a "Set" and the value is coming up.
	 *    FALSE -> It's a "Set" but there's no value (i.e. it's implicit)
	 */
	public abstract Boolean beginMaybe() throws ProblemException, IOException;
	public abstract void endMaybe() throws ProblemException, IOException;  // Only if 'beginMaybe' returns true.
}
