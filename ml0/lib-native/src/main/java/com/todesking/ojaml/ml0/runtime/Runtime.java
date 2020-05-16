package com.todesking.ojaml.ml0.runtime;

public class Runtime {
  public static boolean stringEq(java.lang.String l, java.lang.String r) {
    if(l == null) return r == null;
    else return l.equals(r);
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
}

