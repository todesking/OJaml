package com.todesking.ojaml.ml0.runtime;

public abstract class Fun {
  public final Object local;
  public final Fun parent;
  public final int depth;

  public Fun(Object local, Fun parent) {
    // System.out.println("new Fun(" + local + ", " + parent + ", " + depth + ")");
    this.local = local;
    this.parent = parent;
    if(parent == null) this.depth = 0;
    else this.depth = parent.depth + 1;
  }

  public abstract Object app(Object x);

  protected Object getLocal(int index) {
    if(index > depth) {
      throw new AssertionError("[BUG] getLocal(" + index + ") called when depth = " + depth);
    } if(index < 0) {
      throw new AssertionError("[BUG] getLocal(" + index + ") called");
    } else if(index == depth) {
      return this.local;
    } else {
      return parent.getLocal(index);
    }
  }
}
