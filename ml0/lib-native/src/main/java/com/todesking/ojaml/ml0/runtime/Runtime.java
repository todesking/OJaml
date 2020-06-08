package com.todesking.ojaml.ml0.runtime;

import java.util.regex.Pattern;

public class Runtime {
  public static boolean stringEq(String l, String r) {
    if(l == null) return r == null;
    else return l.equals(r);
  }

  public static String stringConcat(String l, String r) {
    return l + r;
  }

  public static CharSequence stringAsCharSeq(String s) {
    return s;
  }

  public static void println(String s) {
    System.out.println(s);
  }

  public static int intAdd(int l, int r) { return l + r; }
  public static int intSub(int l, int r) { return l - r; }
  public static int intMul(int l, int r) { return l * r; }
  public static int intDiv(int l, int r) { return l / r; }
  public static int intMod(int l, int r) { return l % r; }
  public static boolean intEq(int l, int r) { return l == r; }
  public static boolean intLt(int l, int r) { return l < r; }
  public static boolean intGt(int l, int r) { return l > r; }

  public static boolean boolOr(boolean l, boolean r) { return l || r; }
  public static boolean boolAnd(boolean l, boolean r) { return l && r; }
  public static boolean boolEq(boolean l, boolean r) { return l == r; }

  public static Unit unitValue() { return Unit.value; }

  public static Pattern regexCompile(String s) {
    return Pattern.compile(s);
  }

  public static Object nullAsObject() {
    return null;
  }

  public static StackTraceElement stacktraceElement(int depth) {
    RuntimeException e = new RuntimeException();
    e.fillInStackTrace();
    return e.getStackTrace()[depth + 1];
  }
}

