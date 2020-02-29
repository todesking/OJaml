# OJaml ML0: Simple ML-like language on JVM

## Repl

```sh
sbt repl/run
```

```
ojaml> 1 + 1
res3: int = 2

ojaml> data EitherI = L int | R int ;;

ojaml> let l = L 10 ;;
l: EitherI = L 10

ojaml> match l | R x => "r" | L x => "l" ;;
res2: java.lang.String = l

ojaml> let x = 1 + 2 ;;
x: int = 3

ojaml> let f x = x * 2 ;;
f: int -> int = ojaml.repl.Repl_5$1@141d30da

ojaml> f (x + 1)
res6: int = 8
```
