Using hol-advisor (HOL(y)Hammer) in Emacs
=========================================

# Install the advisor Emacs code:

```
cd $HOME
git clone https://github.com/JUrban/hol-advisor.git
cat hol-advisor/.emacs >> .emacs
```

In Emacs, set up your server by pressing M-: and then pasting into the minibuffer:

```
(customize-save-variable 'hol-advisor-server "mizar.cs.ualberta.ca")
```

Then customize the project (change "Emf199" to your project - it has to be uploaded to the server before), again by pressing M-: and pasting:

```
(customize-save-variable 'hol-current-project "Emf199")
```

Setting the server back to Austria (for experiments with other projects) is done like this:

```
(customize-save-variable 'hol-advisor-server "colo12-c703.uibk.ac.at")
```

# Test the advisor:

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

If everything fails, go to http://mizar.cs.ualberta.ca/hh/ or http://colo12-c703.uibk.ac.at/hh/ and first try with the web interface.

# Install and use DMTCP (optional)

Install DMTCP:

```
http://downloads.sourceforge.net/project/dmtcp/dmtcp-2.x/2.3.1/dmtcp-2.3.1.tar.gz
tar xzf dmtcp-2.3.1.tar.gz
cd dmtcp-2.3.1
./configure
make
make install
```

Compile your project under DMTCP:

```
dmtcp_launch hol-light-workbench/ocaml/bin/ocaml
```

after loading your project, run in the session:

```
#load "unix.cma";;

if Unix.fork () = 0 then Unix.execvp "dmtcp_command" [|""; "--checkpoint"|] else try ignore (Unix.select [] [] [] 0.1) with _ -> ();;
```

and then you can quit the session. A script called `dmtcp_restart_script.sh` should now appear in your working directory.

Restore the session by running:

```
./dmtcp_restart_script.sh
```

In the Emacs mode, this can be invoked from the menu (HOL Light -> Interactive Mode -> Run Caml Toplevel) or by pressing C-c C-s and supplying

```
./dmtcp_restart_script.sh
```

as the "Caml toplevel to run" .
