package com.dragonflow.erlangecc.util;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

public class Base64Encoder
{
  private static final int BUFFER_SIZE = 900;
//  private static Log logger = LogFactory.getEasyLog(Base64Encoder.class);

  private static byte[] encoding = { 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 43, 47, 61 };

  InputStream in = null;
  OutputStream out = null;
  boolean stringp = false;

  private final int get1(byte[] buf, int off) {
    return ((buf[off] & 0xFC) >> 2);
  }

  private final int get2(byte[] buf, int off) {
    return ((buf[off] & 0x3) << 4 | (buf[(off + 1)] & 0xF0) >>> 4);
  }

  private final int get3(byte[] buf, int off) {
    return ((buf[(off + 1)] & 0xF) << 2 | (buf[(off + 2)] & 0xC0) >>> 6);
  }

  private static final int get4(byte[] buf, int off) {
    return (buf[(off + 2)] & 0x3F);
  }

  public void process()
    throws IOException
  {
    byte[] buffer = new byte[900];
    int got = -1;
    int off = 0;
    int count = 0;
    while ((got = this.in.read(buffer, off, 900 - off)) > 0) {
      while (off + 3 <= got) {
        int c1 = get1(buffer, off);
        int c2 = get2(buffer, off);
        int c3 = get3(buffer, off);
        int c4 = get4(buffer, off);
        switch (count)
        {
        case 73:
          this.out.write(encoding[c1]);
          this.out.write(encoding[c2]);
          this.out.write(encoding[c3]);
          this.out.write(10);
          this.out.write(encoding[c4]);
          count = 1;
          break;
        case 74:
          this.out.write(encoding[c1]);
          this.out.write(encoding[c2]);
          this.out.write(10);
          this.out.write(encoding[c3]);
          this.out.write(encoding[c4]);
          count = 2;
          break;
        case 75:
          this.out.write(encoding[c1]);
          this.out.write(10);
          this.out.write(encoding[c2]);
          this.out.write(encoding[c3]);
          this.out.write(encoding[c4]);
          count = 3;
          break;
        case 76:
          this.out.write(10);
          this.out.write(encoding[c1]);
          this.out.write(encoding[c2]);
          this.out.write(encoding[c3]);
          this.out.write(encoding[c4]);
          count = 4;
          break;
        default:
          this.out.write(encoding[c1]);
          this.out.write(encoding[c2]);
          this.out.write(encoding[c3]);
          this.out.write(encoding[c4]);
          count += 4;
        }

        off += 3;
      }

      for (int i = 0; i < 3; ++i)
        buffer[i] = ((i < got - off) ? buffer[(off + i)] : 0);
      off = got - off;
    }

    switch (off)
    {
    case 1:
      this.out.write(encoding[get1(buffer, 0)]);
      this.out.write(encoding[get2(buffer, 0)]);
      this.out.write(61);
      this.out.write(61);
      break;
    case 2:
      this.out.write(encoding[get1(buffer, 0)]);
      this.out.write(encoding[get2(buffer, 0)]);
      this.out.write(encoding[get3(buffer, 0)]);
      this.out.write(61);
    }
  }

  public String processString()
  {
    if (!(this.stringp)) {
      throw new RuntimeException(super.getClass().getName() + "[processString]" + "invalid call (not a String)");
    }
    try
    {
      process();
    } catch (IOException e) {
//      logger.error("IOException " + e.getMessage());
//      logger.debug("", e);
    }
    return ((ByteArrayOutputStream)this.out).toString();
  }

  public Base64Encoder(String input)
  {
    Init(input, "");
  }

  public Base64Encoder(String input, String charset)
  {
    Init(input, charset);
  }

  private void Init(String input, String charset)
  {
    byte[] bytes = new byte[0];
    if ((charset == null) || (charset.equals("")))
      bytes = input.getBytes();
    else {
      try
      {
        bytes = input.getBytes(charset);
      }
      catch (UnsupportedEncodingException e) {
        bytes = input.getBytes();
      }
    }
    this.stringp = true;
    this.in = new ByteArrayInputStream(bytes);
    this.out = new ByteArrayOutputStream();
  }

  public Base64Encoder(InputStream in, OutputStream out)
  {
    this.in = in;
    this.out = out;
    this.stringp = false;
  }

  public static void main(String[] args)
  {
    if (args.length != 1) {
      System.out.println("Base64Encoder <string>");
      System.exit(0);
    }
    Base64Encoder b = new Base64Encoder(args[0]);
    System.out.println("[" + b.processString() + "]");
  }
}
