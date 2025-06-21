%% Módulo Proylcc con corrección de rangos de disparo y sistema de combos
:- module(proylcc, 
    [
        randomBlock/2,
        shoot/5
    ]).

:- use_module(library(random)).

% randomBlock(+Grid, -Block)
randomBlock(Grid, Block) :-
    max_in_grid(Grid, Max),
    rango_disparo(Max, Rango),
    random_member(Block, Rango).

% Encuentra el máximo valor numérico en la grilla (ignorando los "-")
max_in_grid(Grid, Max) :-
    include(number, Grid, Numeros),
    max_list(Numeros, Max).

% Define el rango permitido según el máximo de la grilla - CORREGIDO
rango_disparo(Max, Rango) :-
    ( Max =< 4     -> Rango = [2,4];
      Max =< 8     -> Rango = [2,4,8];
      Max =< 16    -> Rango = [2,4,8,16];
      Max =< 32    -> Rango = [2,4,8,16,32];
      Max =< 64    -> Rango = [2,4,8,16,32,64];
      Max =< 128   -> Rango = [4,8,16,32,64,128];
      Max =< 256   -> Rango = [8,16,32,64,128,256];
      Max =< 512   -> Rango = [16,32,64,128,256,512];
      Max =< 1024  -> Rango = [32,64,128,256,512,1024];
      Max =< 2048  -> Rango = [64,128,256,512,1024,2048];
      Max =< 4096  -> Rango = [128,256,512,1024,2048,4096];
      Max =< 8192  -> Rango = [256,512,1024,2048,4096,8192];
      % Para máximos muy altos, mantener el rango más alto
      Rango = [256,512,1024,2048,4096,8192]  % Cambiar el caso por defecto
    ).
% Obtener el rango anterior para comparar - TAMBIÉN CORREGIR
rango_anterior(Max, RangoAnterior) :-
    ( Max =< 4     -> RangoAnterior = [];
      Max =< 8     -> RangoAnterior = [2,4];
      Max =< 16    -> RangoAnterior = [2,4,8];
      Max =< 32    -> RangoAnterior = [2,4,8,16];
      Max =< 64    -> RangoAnterior = [2,4,8,16,32];
      Max =< 128   -> RangoAnterior = [2,4,8,16,32,64];
      Max =< 256   -> RangoAnterior = [4,8,16,32,64,128];
      Max =< 512   -> RangoAnterior = [8,16,32,64,128,256];
      Max =< 1024  -> RangoAnterior = [16,32,64,128,256,512];
      Max =< 2048  -> RangoAnterior = [32,64,128,256,512,1024];
      Max =< 4096  -> RangoAnterior = [64,128,256,512,1024,2048];
      Max =< 8192  -> RangoAnterior = [128,256,512,1024,2048,4096];
      % Para máximos muy altos, mantener el rango anterior más alto
      RangoAnterior = [128,256,512,1024,2048,4096]
).

% Determinar bloques retirados cuando se alcanza un nuevo máximo
bloques_retirados(MaxAnterior, MaxNuevo, BloquesRetirados) :-
    rango_anterior(MaxAnterior, RangoAnterior),
    rango_anterior(MaxNuevo, RangoNuevo),
    subtract(RangoAnterior, RangoNuevo, BloquesRetirados).

% Determinar bloques agregados cuando se alcanza un nuevo máximo
bloques_agregados(MaxAnterior, MaxNuevo, BloquesAgregados) :-
    rango_disparo(MaxAnterior, RangoAnterior),
    rango_disparo(MaxNuevo, RangoNuevo),
    subtract(RangoNuevo, RangoAnterior, BloquesAgregados).

/**
 * shoot(+Block, +Column, +Grid, +NumOfColumns, -Effects)
 * Función principal con limpieza de bloques retirados y avisos
 */
shoot(Block, Column, Grid, NumOfColumns, Effects) :-
    number(Block),
    find_insertion_index(Grid, Column, NumOfColumns, Index),
    insert_block_at_index(Grid, Index, Block, Grid1),
    Effects1 = [effect(Grid1, [])],
    max_in_grid(Grid, MaxAnterior),
    % Procesar combinaciones con combo corregido
    process_combinations_with_combo(Grid1, NumOfColumns, Index, Effects1, TempEffects),
    % Verificar si hay nuevo máximo y procesar limpieza
    last(TempEffects, effect(FinalGrid, _)),
    max_in_grid(FinalGrid, MaxNuevo),
    process_cleanup_and_notifications(MaxAnterior, MaxNuevo, FinalGrid, NumOfColumns, TempEffects, Effects).

% Ajuste: Column es 1-based, convertimos a 0-based para calcular posiciones
find_insertion_index(Grid, Column, NumOfColumns, Index) :-
    Column0 is Column - 1,
    length(Grid, Len),
    Rows is Len // NumOfColumns,
    findall(Pos,
        ( between(0, Rows, R),
          Pos is Column0 + R * NumOfColumns,
          Pos < Len
        ),
        Positions),
    member(Index, Positions),
    nth0(Index, Grid, Elem),
    Elem == '-', !.

% Inserta el bloque en el índice dado
insert_block_at_index(Grid, Index, Block, NewGrid) :-
    nth0(Index, Grid, _, Rest),
    nth0(Index, NewGrid, Block, Rest).

%% process_combinations_with_combo(+Grid, +NumCols, +Index, +EffectsAcc, -Effects)
% Mantengo la lógica original pero corrijo el sistema de combos
process_combinations_with_combo(Grid, NumCols, Index, EffectsAcc, Effects) :-
    nth0(Index, Grid, Value),
    Value \= '-',
    % Buscar vecinos con el mismo valor (lógica original)
    findall(Pos,
        ( member(Direction, [left, up, right]),
          neighbor(Index, NumCols, Direction, Pos),
          nth0(Pos, Grid, Value)
        ), Neighbors),
    Neighbors \= [], !,
    % Crear el path de bloques a combinar
    Path = [Index|Neighbors],
    length(Path, NumBlocks), % Contar cuántos bloques se combinan
    NewValue is Value * 2,
    % Determinar posición de destino (el primer vecino como en el original)
    [Dest|_] = Neighbors,
    % Crear cambios: nuevo valor en destino, borrar los demás
    findall((P, '-'), (member(P, Path), P \= Dest), Clears),
    Changes = [(Dest, NewValue)|Clears],
    apply_changes(Grid, Changes, Grid2),
    % Sistema de combo corregido: combo solo si se combinan 3+ bloques
    ( NumBlocks >= 3 ->
        ComboInfo = [newBlock(NewValue), combo(NumBlocks)]
    ;   ComboInfo = [newBlock(NewValue)]
    ),
    append(EffectsAcc, [effect(Grid2, ComboInfo)], Acc2),
    % Continuar procesando desde la posición de destino
    process_combinations_with_combo(Grid2, NumCols, Dest, Acc2, Effects).
process_combinations_with_combo(Grid, NumCols, _, EffectsAcc, Effects) :-
    % No hay más combinaciones, continuar sin aplicar gravedad aquí
    Effects = EffectsAcc.

%% FUNCIONES PARA GRAVEDAD (mantengo las originales)

% apply_gravity_cycle(+Grid, +NumCols, +EffectsAcc, -Effects)
apply_gravity_cycle(Grid, NumCols, EffectsAcc, Effects) :-
    apply_gravity(Grid, NumCols, GravityGrid),
    ( Grid = GravityGrid ->
        % No hubo cambios por gravedad, terminamos
        Effects = EffectsAcc
    ;   % Hubo cambios, agregar efecto de gravedad y buscar combinaciones
        append(EffectsAcc, [effect(GravityGrid, [gravity])], TempEffects),
        check_for_new_combinations(GravityGrid, NumCols, TempEffects, Effects)
    ).

% Aplicar gravedad: hacer caer todos los bloques hacia arriba (gravedad invertida)
apply_gravity(Grid, NumCols, NewGrid) :-
    length(Grid, Len),
    Rows is Len // NumCols,
    apply_gravity_to_columns(Grid, NumCols, Rows, 0, NewGrid).

% Aplicar gravedad columna por columna
apply_gravity_to_columns(Grid, NumCols, Rows, Col, NewGrid) :-
    Col >= NumCols, !,
    NewGrid = Grid.
apply_gravity_to_columns(Grid, NumCols, Rows, Col, NewGrid) :-
    Col < NumCols,
    extract_column(Grid, NumCols, Rows, Col, Column),
    apply_gravity_to_column(Column, GravityColumn),
    replace_column(Grid, NumCols, Rows, Col, GravityColumn, TempGrid),
    NextCol is Col + 1,
    apply_gravity_to_columns(TempGrid, NumCols, Rows, NextCol, NewGrid).

% Extraer una columna de la grilla
extract_column(Grid, NumCols, Rows, Col, Column) :-
    findall(Value,
        ( between(0, Rows, Row),
          Row < Rows,
          Index is Row * NumCols + Col,
          nth0(Index, Grid, Value)
        ), Column).

% Aplicar gravedad invertida a una columna individual (bloques caen hacia arriba)
apply_gravity_to_column(Column, GravityColumn) :-
    include(\=('-'), Column, Blocks),
    length(Column, Len),
    length(Blocks, NumBlocks),
    NumEmpty is Len - NumBlocks,
    length(EmptySpaces, NumEmpty),
    maplist(=('-'), EmptySpaces),
    append(Blocks, EmptySpaces, GravityColumn).

% Reemplazar una columna en la grilla
replace_column(Grid, NumCols, Rows, Col, NewColumn, NewGrid) :-
    replace_column_helper(Grid, NumCols, Rows, Col, NewColumn, 0, NewGrid).

replace_column_helper(Grid, NumCols, Rows, Col, NewColumn, Row, NewGrid) :-
    Row >= Rows, !,
    NewGrid = Grid.
replace_column_helper(Grid, NumCols, Rows, Col, NewColumn, Row, NewGrid) :-
    Row < Rows,
    Index is Row * NumCols + Col,
    nth0(Row, NewColumn, Value),
    nth0(Index, Grid, _, Rest),
    nth0(Index, TempGrid, Value, Rest),
    NextRow is Row + 1,
    replace_column_helper(TempGrid, NumCols, Rows, Col, NewColumn, NextRow, NewGrid).

% check_for_new_combinations(+Grid, +NumCols, +EffectsAcc, -Effects)
check_for_new_combinations(Grid, NumCols, EffectsAcc, Effects) :-
    length(Grid, Len),
    check_all_positions(Grid, NumCols, 0, Len, EffectsAcc, Effects).

% check_all_positions(+Grid, +NumCols, +Index, +Len, +EffectsAcc, -Effects)
check_all_positions(Grid, NumCols, Index, Len, EffectsAcc, Effects) :-
    Index >= Len, !,
    Effects = EffectsAcc.
check_all_positions(Grid, NumCols, Index, Len, EffectsAcc, Effects) :-
    Index < Len,
    nth0(Index, Grid, Value),
    ( Value \= '-',
      has_combination_at(Grid, NumCols, Index) ->
        % Encontramos una combinación después de gravedad, procesarla
        process_combinations_with_combo(Grid, NumCols, Index, EffectsAcc, Effects)
    ;   % No hay combinación en esta posición, seguir
        NextIndex is Index + 1,
        check_all_positions(Grid, NumCols, NextIndex, Len, EffectsAcc, Effects)
    ).

% Verificar si hay una combinación en una posición específica
has_combination_at(Grid, NumCols, Index) :-
    nth0(Index, Grid, Value),
    Value \= '-',
    (   neighbor(Index, NumCols, up,    Dest), nth0(Dest, Grid, Value)
    ;   neighbor(Index, NumCols, left,  Dest), nth0(Dest, Grid, Value)
    ;   neighbor(Index, NumCols, right, Dest), nth0(Dest, Grid, Value)
    ), !.

%% neighbor(+Index, +NumCols, +Dir, -NeighborIndex)
neighbor(Index, NumCols, up,    N) :- Index >= NumCols,        N is Index - NumCols.
neighbor(Index, NumCols, left,  N) :- Col is Index mod NumCols, Col > 0, N is Index - 1.
neighbor(Index, NumCols, right, N) :- Col is Index mod NumCols, Col < NumCols - 1, N is Index + 1.

% FUNCIÓN CORREGIDA - process_cleanup_and_notifications
process_cleanup_and_notifications(MaxAnterior, MaxNuevo, Grid, NumCols, EffectsAcc, Effects) :-
    ( MaxNuevo > MaxAnterior ->
        % Hay nuevo máximo, procesar limpieza y notificaciones
        bloques_retirados(MaxAnterior, MaxNuevo, BloquesRetirados),
        bloques_agregados(MaxAnterior, MaxNuevo, BloquesAgregados),
        
        % Construir lista de notificaciones - SOLO >= 512
        ( MaxNuevo >= 512 ->
            Notifications = [newMaxBlock(MaxNuevo)]
        ;   Notifications = []
        ),
        ( BloquesAgregados \= [] -> 
            append(Notifications, [newBlockAdded], TempNotif1)
        ;   TempNotif1 = Notifications
        ),
        ( BloquesRetirados \= [] ->
            append(TempNotif1, [blockEliminated], FinalNotifications)
        ;   FinalNotifications = TempNotif1
        ),
        
        % Limpiar bloques retirados de la grilla si es necesario
        ( BloquesRetirados \= [] ->
            cleanup_retired_blocks(Grid, BloquesRetirados, CleanGrid),
            append(FinalNotifications, [cleanup(BloquesRetirados)], AllNotifications),
            append(EffectsAcc, [effect(CleanGrid, AllNotifications)], TempEffects),
            apply_gravity_cycle(CleanGrid, NumCols, TempEffects, Effects)
        ;   % No hay bloques retirados, agregar notificaciones al último efecto
            last(EffectsAcc, effect(LastGrid, LastNotifications)),
            append(LastNotifications, FinalNotifications, UpdatedNotifications),
            append(InitialEffects, [effect(LastGrid, LastNotifications)], EffectsAcc),
            append(InitialEffects, [effect(LastGrid, UpdatedNotifications)], TempEffects),
            apply_gravity_cycle(Grid, NumCols, TempEffects, Effects)
        )
    ;   % No hay nuevo máximo, solo aplicar gravedad
        apply_gravity_cycle(Grid, NumCols, EffectsAcc, Effects)
    ).

% cleanup_retired_blocks(+Grid, +RetiredBlocks, -CleanGrid)
cleanup_retired_blocks(Grid, RetiredBlocks, CleanGrid) :-
    findall((Index, '-'),
        ( nth0(Index, Grid, Value),
          member(Value, RetiredBlocks)
        ), Changes),
    apply_changes(Grid, Changes, CleanGrid).

%% SOLO LA CORRECCIÓN DE LA FUNCIÓN hint_shot EXISTENTE
%% Reemplaza la función hint_shot en tu código original

% Función para obtener hint de una jugada específica - MEJORADA
% hint_shot(+Block, +Column, +Grid, +NumOfColumns, -HintInfo)
% Predice el resultado de disparar un bloque en una columna específica
hint_shot(Block, Column, Grid, NumOfColumns, HintInfo) :-
    number(Block),
    % Verificar si se puede disparar en esta columna
    ( \+ find_insertion_index(Grid, Column, NumOfColumns, _) ->
        HintInfo = [no_shot]
    ;   % Se puede disparar, calcular preview
        find_insertion_index(Grid, Column, NumOfColumns, Index),
        insert_block_at_index(Grid, Index, Block, Grid1),
        
        % Analizar la combinación inmediata
        analyze_immediate_combination(Grid1, NumOfColumns, Index, ComboInfo),
        
        % Si hay combinación, calcular el próximo bloque
        ( member(newBlock(_), ComboInfo) ->
            % Simular el disparo para obtener la grilla final
            simulate_shot_outcome(Block, Column, Grid, NumOfColumns, FinalGrid),
            % Generar el próximo bloque basado en la grilla final
            randomBlock(FinalGrid, NextBlock),
            append(ComboInfo, [nextBlock(NextBlock)], HintInfo)
        ;   % No hay combinación, próximo bloque basado en la grilla actual
            randomBlock(Grid1, NextBlock),
            append(ComboInfo, [nextBlock(NextBlock)], HintInfo)
        )
    ).

% analyze_immediate_combination(+Grid, +NumCols, +Index, -ComboInfo)
% Analiza qué combinación se formaría inmediatamente al colocar el bloque
analyze_immediate_combination(Grid, NumCols, Index, ComboInfo) :-
    nth0(Index, Grid, Value),
    Value \= '-',
    % Buscar vecinos con el mismo valor
    findall(Pos,
        ( member(Direction, [left, up, right]),
          neighbor(Index, NumCols, Direction, Pos),
          nth0(Pos, Grid, Value)
        ), Neighbors),
    
    % Determinar el tipo de combinación
    ( Neighbors = [] ->
        ComboInfo = [no_combo]
    ;   length(Neighbors, NumNeighbors),
        TotalBlocks is NumNeighbors + 1,
        NewValue is Value * 2,
        ( TotalBlocks >= 3 ->
            ComboInfo = [combo(TotalBlocks), newBlock(NewValue)]
        ;   ComboInfo = [simple_combo, newBlock(NewValue)]
        )
    ).

% simulate_shot_outcome(+Block, +Column, +Grid, +NumOfColumns, -FinalGrid)
% Simula un disparo completo para obtener la grilla final
simulate_shot_outcome(Block, Column, Grid, NumOfColumns, FinalGrid) :-
    shoot(Block, Column, Grid, NumOfColumns, Effects),
    % Obtener la última grilla de los efectos
    last(Effects, effect(FinalGrid, _)).

% Nuevo predicado para generar par de bloques (actual, siguiente)
generate_block_pair(Grid, CurrentBlock, NextBlock) :-
    max_in_grid(Grid, Max),
    rango_disparo(Max, Rango),
    random_member(CurrentBlock, Rango),
    random_member(NextBlock, Rango).

% Inicializar estado del juego con par de bloques
init_game_state(Grid, NumCols, GameState) :-
    generate_block_pair(Grid, CurrentBlock, NextBlock),
    GameState = gameState(Grid, NumCols, CurrentBlock, NextBlock).

% Actualizar estado después de un disparo
update_game_state(gameState(Grid, NumCols, CurrentBlock, _), Column, NewGameState) :-
    shoot(CurrentBlock, Column, Grid, NumCols, Effects),
    % Obtener la grilla final después de todos los efectos
    last(Effects, effect(FinalGrid, _)),
    % Generar nuevo par de bloques basado en la grilla final
    generate_block_pair(FinalGrid, NewCurrentBlock, NewNextBlock),
    NewGameState = gameState(FinalGrid, NumCols, NewCurrentBlock, NewNextBlock).

%% apply_changes(+Grid, +Changes, -NewGrid)
apply_changes(Grid, [], Grid).
apply_changes(Grid, [(I, V)|T], NewGrid) :-
    nth0(I, Grid, _, Rest),
    nth0(I, Temp, V, Rest),
    apply_changes(Temp, T, NewGrid).