fof(h,axiom, ![X,Y] : (sp_ap(const(Y),X) = Y)).
fof(i,axiom, ![F,X,Y] : sp_ap(override(F,X,Y),X) = Y).
fof(j,axiom, ![F,X,Y,Z] : (Z != X => (sp_ap(override(F,X,Y),Z) = sp_ap(F,Z)))).
fof( a ,axiom, (![Vx]: (pPresent(Vx) => (Vx = piano | Vx = pyssel))) ).
fof( a ,axiom, (![Vx]: (pChild(Vx) => (Vx = vincent | Vx = irmeli))) ).
fof( a ,axiom, pChild(vincent) ).
fof( a ,axiom, pChild(irmeli) ).
fof( a ,axiom, pPresent(pyssel) ).
fof( a ,axiom, pPresent(piano) ).
fof( a ,axiom, (![Vf]: ((![Vx]: (pChild(Vx) => pget(Vx,sp_ap(Vf,Vx)))) => (?[Vy]: (pChild(Vy) & popens(Vy,sp_ap(Vf,Vy)))))) ).
fof( a ,axiom, (![Vx]: (pChild(Vx) => (?[Vy]: (pPresent(Vy) & pget(Vx,Vy))))) ).
fof( a ,axiom, (![Vx]: (pPresent(Vx) => (![VnotTT]: (popens(vincent,Vx) => $false)))) ).
fof( a ,axiom, (![VnotTT]: (pget(irmeli,piano) => $false)) ).
fof(conj, conjecture, 
  popens(irmeli,pyssel)
).
