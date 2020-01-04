package com.todesking.ojaml.ml0.runtime;

public abstract class Data {
    public final int arity;

    public Data(int arity) {
        this.arity = arity;
    }

    public String toString(boolean group) {
        if(!group || arity == 0) return toString();
        else return "(" + toString() + ")";
    }
    public abstract Object[] values();
    public abstract String name();

    public String toString() {
      StringBuilder sb = new StringBuilder();
      sb.append(name());
      Object[] vs = values();
      for(int i = 0; i < vs.length; i++) {
        sb.append(" ");
        sb.append(format(vs[i], true));
      }
      return sb.toString();
    }

    public static String format(Object x, boolean group) {
        if(x instanceof Data) {
            return ((Data)x).toString(group);
        } else {
            return x.toString();
        }
    }
}
