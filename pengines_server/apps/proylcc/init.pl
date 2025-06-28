:- module(init, [ init/2 ]).

/**
 * init(-Grid, -NumOfColumns).
 * 
 * Predicado especificando la grilla inicial, que será mostrada al comienzo del juego, donde
 * Grid es una lista con los números que conforman la grilla, y NumOfColumns es la cantidad de columnas, 
 * determinando las dimensiones de la misma.
 */

init([
	512,256,-,64,32,
	4,128,-,4,16,
	2,64,-,-,2,
	-,32,-,-,16,
	-,16,-,-,2,
	-,8,-,-,-,
	-,-,-,-,-
], 5).