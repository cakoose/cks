package cks.io;

import java.io.OutputStream;
import java.io.IOException;

/**
 * Like ByteArrayOutputStream, except the implementation strategy
 * involves less copying.  ByteArrayOutputStream maintains a single
 * buffer, resizing it when it needs more space.  We maintain a
 * linked list of buffers, appending to it when we need more space.
 */
public final class MemoryOutputStream extends OutputStream
{
	public static final int DefaultInitialExtraSpace = 64;
	public static final int DefaultMaxExtraSpace = 1024;

	private final int maxExtraSpace;
	private int currentExtraSpace;

	public MemoryOutputStream()
	{
		this(DefaultInitialExtraSpace, DefaultMaxExtraSpace);
	}

	/**
	 * 'initialExtraSpace' specifies the number of bytes of buffer we
	 * should initially pre-allocate.  Each time we use up the buffer
	 * and need to allocate more, we'll allocate twice as much free
	 * space as the previous time, up to a maximum of 'maxExtraSpace'.
	 */
	public MemoryOutputStream(int initialExtraSpace, int maxExtraSpace)
	{
		if (initialExtraSpace < 1) throw new IllegalArgumentException("'initialExtraSpace' must be greater than zero (initialExtraSpace=" + initialExtraSpace + ")");
		if (maxExtraSpace < initialExtraSpace) throw new IllegalArgumentException("'maxExtraSpace' must be at least as great as 'initialExtraSpace' (initialExtraSpace=" + initialExtraSpace + ", maxExtraSpace=" + maxExtraSpace + ")");

		this.maxExtraSpace = maxExtraSpace;
		this.currentExtraSpace = initialExtraSpace;

		this.currentBuffer = new byte[this.currentExtraSpace];
		this.currentBufferPos = 0;
	}

	// -----------------------------------------------------------------------------

	private int currentBufferPos;
	private byte[] currentBuffer;

	public void write(byte[] data, int offset, int length)
	{
		int remaining = currentBuffer.length - currentBufferPos;
		if (remaining < length) {
			// Put as much of 'data' as we can in the current buffer.
			System.arraycopy(data, offset, currentBuffer, currentBufferPos, remaining);
			offset += remaining;
			length -= remaining;

			// Retire the current buffer and allocate a new one.
			appendToFullList(currentBuffer, length);

			// Put whatever remains in the newly-allocated buffer.
			System.arraycopy(data, offset, currentBuffer, currentBufferPos, length);
			currentBufferPos += length;
		}
		else {
			System.arraycopy(data, offset, currentBuffer, currentBufferPos, length);
			currentBufferPos += length;
		}
	}

	public void write(byte[] data)
	{
		write(data, 0, data.length);
	}

	public void write(int b)
	{
		if (currentBufferPos >= currentBuffer.length) {
			appendToFullList(currentBuffer, 1);
		}
		currentBuffer[currentBufferPos++] = (byte) b;
	}

	// -----------------------------------------------------------------------------

	private Link firstFullBuffer = null;
	private Link lastFullBuffer = null;
	private int fullBufferCount = 0;

	private static final class Link
	{
		public final byte[] buffer;
		public Link next;

		public Link(byte[] buffer)
		{
			this.buffer = buffer;
		}
	}

	private void appendToFullList(byte[] buffer, int aboutToWrite)
	{
		Link link = new Link(buffer);
		if (firstFullBuffer == null) {
			firstFullBuffer = lastFullBuffer = link;
		}
		else {
			lastFullBuffer.next = link;
			lastFullBuffer = link;
		}
		fullBufferCount += buffer.length;

		currentExtraSpace = Math.max(currentExtraSpace * 2, maxExtraSpace);
		int newBufferSize = currentExtraSpace + aboutToWrite;
		currentBuffer = new byte[newBufferSize];
		currentBufferPos = 0;
	}

	// -----------------------------------------------------------------------------

	public int size()
	{
		return fullBufferCount + currentBufferPos;
	}

	public byte[] toByteArray()
	{
		byte[] out = new byte[size()];

		int pos = 0;
		for (Link l = firstFullBuffer; l != null; l = l.next) {
			System.arraycopy(l.buffer, 0, out, pos, l.buffer.length);
			pos += l.buffer.length;
		}

		System.arraycopy(currentBuffer, 0, out, pos, currentBufferPos);
		pos += currentBufferPos;

		assert pos == out.length; // Make sure we wrote as much as we thought we wrote.
		return out;
	}

	public void copyTo(OutputStream out)
		throws IOException
	{
		for (Link l = firstFullBuffer; l != null; l = l.next) {
			out.write(l.buffer);
		}

		out.write(currentBuffer, 0, currentBufferPos);
	}
}
