# OJaml: ML-like language on JVM


## Project structure

* `ml0`: Language with most basic features
  * `compiler-scala`: ML0 compiler written with Scala
  * `compiler-ml0`: Self-hosted ML0 compiler
  * `lib-native`: Standard library written with Java
  * `lib`: Standard library
  * `test`

## ML0 language spec

```
program = package? import* struct*
package = "package" qname
import = "import" qname
struct = "struct" "{" term* "}"
member = datatype | term
term = tlet | expr
tlet = "let" name (":" type) = expr
expr = lit | var | app | infix | japp | japps | jnew | upcast | elet | if | fun
lit = lit_int | lit_bool | lit_string
var = name
app = expr expr
infix = expr infix_op expr
infix_op = "+" | "-" | "*" | "/"
japp = expr "#" name "(" expr* ")"
japps = jclass "#" name "(" expr* ")"
jnew = "new" jclass "(" expr* ")"
upcast = expr ":>" jclass
elet = "let" name (":" type) = expr "in" expr
if = "if" expr expr ("else" expr)?
fun = "fun" "[" type "]" name* = expr

type = tyname | "(" type ")" | type "->" type

qname = (name ".")* name
name = [a-zA-Z][-_a-zA-Z0-9]*
jclass = "#{" qname "}"
tyname = qname | jclass
```

### Translation to JVM class

```
package pname

struct S1 {
  let x1 = e1
  e2
  datatype ioption =
      None
    | Some of int

  let f1: ioption -> int -> int = fn
      None i = i
      Some j i = j + i
}
```

```java
package pname;

class S1 {
  public static final T1 x1;
  public static final Fun<S1$ioption, Fun<Integer, Integer>> f1;

  static {
    x1 = {{ e1 }} ;
    {{ e2 }} ;
    f1 = new Fun<S1$ioption, Fun<Integer, Integer>> {
      @Override
      public Fun<Integer, Integer> apply(S1$ioption x1) {
        return new Fun<Integer, Integer> {
          @Override
          public Integer apply(Integer x2) {
            if(x1 instanceof S1$ioption$None) {
              return x2;
            } else if(x1 instanceof S1$ioption$Some) {
              return (S1$ioption$Some).x1 + x2;
            } else {
              throw new AssertionError
            }
          }
        }
      }
    }
  }
}

class S1$ioption {}

class S1$ioption$None extends S1$ioption {
}

class S1$ioption$Some extends s1$ioption {
  public final int x1;
  public S1$ioption$Some(x1: Int) {
    this.x1 = x1
  }
}
```
