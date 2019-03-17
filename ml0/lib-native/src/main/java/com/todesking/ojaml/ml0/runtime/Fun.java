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

  protected Object getLocal(int depth, int index) {
    if(depth > this.depth) {
      throw new AssertionError("[BUG] getLocal(" + depth + ", " + index + ") called when depth = " + this.depth);
    } if(depth < 0) {
      throw new AssertionError("[BUG] getLocal(" + depth + ", " + index + ") called");
    } else if(depth == this.depth) {
      if(index == 0) {
        return this.local;
      } else {
        return ((Object[])this.local)[index - 1];
      }
    } else {
      return parent.getLocal(depth, index);
    }
  }
}
