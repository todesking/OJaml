package com.todesking.ojaml.ml0.runtime;

public abstract class Fun {
  public final Object local;
  public final Fun parent;
  public final int depth;

  public Fun(Object local, Fun parent, int depth) {
    this.local = local;
    this.parent = parent;
    this.depth = depth;
  }

  public abstract Object app(Object x);

  protected Object getLocal(int index) {
    if(depth == 0) {
      throw new AssertionError("[BUG] getLocal(" + index + ") called but depth = 0");
    } if(index < 0) {
      throw new AssertionError("[BUG] getLocal(" + index + ") called");
    } else if(index == depth - 1) {
      return this.local;
    } else if(parent == null) {
      throw new AssertionError("[BUG] getLocal(" + index + ") called but parent is null");
    } else {
      return parent.getLocal(index);
    }
  }
}
