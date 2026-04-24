#!/usr/bin/env wolframscript
(* ::Package:: *)

RKODE[G_, x0_, f0_, dx_, Ns_] := Module[{k1, k2, retvals, index, xn, fn},
retvals = Table[{0.0, 0.0}, {i, 1, Ns}];
xn = x0;
fn = f0;
retvals[[1]] = {x0,f0};
For[index = 2, index <= Ns, index = index + 1,
k1 = dx*G[xn, fn];
k2 = dx*G[xn + dx/2, fn + k1/2];
fn = fn + k2;
xn = xn + dx;
retvals[[index]] = {xn, fn};
];
Return[retvals];
]

(*
(* Oscilador armonico *)
(* Sistema: x' = v, v' = -x *)
G[t_, {x_, v_}] := {v, -x}

(* Ejecutar *)
data = RKODE[G, 0, {1, 0}, 0.1, 100];

(* Extraer posici\[OAcute]n x(t) *)
pos = data[[All, {1, 2}]] /. {t_, {x_, v_}} :> {t, x};

(* Graficar *)
ListLinePlot[pos]
*)

(*PROBLEMA REDDIT*)
g= 9.81;
m=0.43;
c2 = 0.0057;
(* ec. de movimietno*)

(* con resistencia*)

Gw[t_, state_] := Module[
  {x, y, vx, vy, v},
  
  {x, y, vx, vy} = state;
  v = Sqrt[vx^2 + vy^2];
  
  {
   vx,
   vy,
   -(c2/m)*v*vx,
   -g - (c2/m)*v*vy
  }
]


(* Sin resistencia *)

Gwo[t_, state_] := Module[
  {x, y, vx, vy},
  
  {x, y, vx, vy} = state;
  
  {
   vx,
   vy,
   0,
   -g
  }
]
(* Cond iniciales*)

x0 = 0;
y0 = 0;

v0 = 50;
theta = Pi/3;

vx0 = v0 Cos[theta];
vy0 = v0 Sin[theta];

estado0 = {x0, y0, vx0, vy0};

(* Simulaciones*)

sow = RKODE[Gw, 0, estado0, 0.01, 2000];
sowo = RKODE[Gwo, 0, estado0, 0.01, 2000];

(* Trayectoreias *)

trw = sow[[All, 2]][[All, {1, 2}]];
trwo = sowo[[All, 2]][[All, {1, 2}]];

(* Ajustes *)

trw = Select[trw, #[[2]] >= 0 &]; 
trwo = Select[trwo, #[[2]] >= 0 &];

(*Grafico*)

ListLinePlot[
 {trw, trwo},
 PlotLegends -> {"Con resistencia", "Sin resistencia"},
 AxesLabel -> {"x", "y"},
 PlotRange -> All
] 
