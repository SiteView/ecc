package com.dragonflow.siteview.infra.ipmi.util;

public abstract class IPMIUtils
{

    public IPMIUtils()
    {
    }

    public static final String bytesToHex(byte data[], int start, int length)
    {
        StringBuilder buf = new StringBuilder();
        for(int i = start; i < length + start; i++)
        {
            buf.append(byteToHex(data[i]));
            buf.append(" ");
        }

        return buf.toString();
    }

    public static final String bytesToHex(byte data[])
    {
        return bytesToHex(data, 0, data.length);
    }

    public static final String byteToHex(byte data)
    {
        StringBuilder buf = new StringBuilder();
        buf.append(toHexChar(data >>> 4 & 15));
        buf.append(toHexChar(data & 15));
        return buf.toString();
    }

    public static final char toHexChar(int i)
    {
        if(0 <= i && i <= 9)
            return (char)(48 + i);
        else
            return (char)(97 + (i - 10));
    }

    public static final void intToBytes(byte bytedest[], int offset, int intsrc)
    {
        bytedest[offset + 0] = (byte)intsrc;
        bytedest[offset + 1] = (byte)(intsrc >> 8);
        bytedest[offset + 2] = (byte)(intsrc >> 16);
        bytedest[offset + 3] = (byte)(intsrc >> 24);
    }

    public static final int bytesToInt(byte bytesrc[], int offset, int length)
    {
        int intdes = 0;
        for(int i = (offset + length) - 1; i >= offset; i--)
            intdes = (intdes << 8) + (bytesrc[i] & 255);

        return intdes;
    }

    public static final void shortToBytes(byte bytedest[], int offset, short shortsrc)
    {
        bytedest[offset + 0] = (byte)shortsrc;
        bytedest[offset + 1] = (byte)(shortsrc >> 8);
    }

    public static final short bytesToShort(byte bytesrc[], int offset)
    {
        short shortdes = 0;
        for(int i = offset + 1; i >= offset; i--)
            shortdes = (short)((shortdes << 8) + (bytesrc[i] & 255));

        return shortdes;
    }

    public static final byte get2MSBitsUnsigned(byte b)
    {
        return (byte)((b & 192) >>> 6);
    }

    public static final byte get2MSBitsSigned(byte b)
    {
        return (byte)((b & -64) >> 6);
    }

    public static final byte get4MSBitsUnsigned(byte b)
    {
        return (byte)((b & 240) >> 4);
    }

    public static final byte get4MSBitsSigned(byte b)
    {
        return (byte)((b & -16) >>> 4);
    }

    public static final byte get4LSBitsSigned(byte b)
    {
        int tmp = b & 15;
        if(tmp > 7)
            return (byte)(tmp | -16);
        else
            return (byte)tmp;
    }

    public static final byte get6LSBitsUnsigned(byte b)
    {
        return (byte)(b & 63);
    }
}