hol-advisor
===========

1. Install the advisor:

```
cd $HOME
git clone https://github.com/JUrban/hol-advisor.git
cat hol-advisor/.emacs >> .emacs
```

Set up your server and project

```
(customize-save-variable 'hol-advisor-server "mizar.cs.ualberta.ca")
(customize-save-variable 'hol-current-project "Emf199")
```

Setting the server back to Austria (for experiments with other projects):

```
(customize-save-variable 'hol-advisor-server "colo12-c703.uibk.ac.at")
```

2. Test the advisor:

```
emacs test1.ml
```

then type:

```
g(`EVEN x \/ EVEN (x + 3)`);;;
```

and you should get:

```
g(`EVEN x \/ EVEN (x + 1)`);; e(MESON_TAC[EVEN;ADD1]);;
```

Sometimes the proof reconstruction fails:

```
g(`EVEN x \/ EVEN (x + 3)`);; (* reconstruction failed:  BIT0 EVEN_conjunct1 EVEN_DOUBLE MULT_2 ARITH_SUC_conjunct2 EVEN_ADD NUMERAL ADD_SUC
<br/>No more advice *)
```

Then you have to write your own tactical proof (MESON is not strong enough), but the names of lemmas should still be useful.

And sometimes there is no proof found (either it is too hard, or there is none):

```
g(`n + n = 1`);; (* No ATP proof found *)
```

3. Install and use DMTCP (optional)
