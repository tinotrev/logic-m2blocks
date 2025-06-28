%% Módulo Proylcc con correcciones de score, posicionamiento y procesamiento completo
:- module(proylcc, 
    [
        randomBlock/2,
        shoot/5,
        hint_shot/5,
        generate_block_pair/3,
        init_game_state/3,
        update_game_state/3
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

% Define el rango permitido según el máximo de la grilla
rango_disparo(Max, Rango) :-
    ( Max =< 4     -> Rango = [2,4];
      Max =< 8     -> Rango = [2,4,8];
      Max =< 16    -> Rango = [2,4,8,16];
      Max =< 32    -> Rango = [2,4,8,16,32];
      Max =< 64    -> Rango = [2,4,8,16,32,64];
      Max =< 128   -> Rango = [2,4,8,16,32,64];
      Max =< 256   -> Rango = [2,4,8,16,32,64];
      Max =< 512   -> Rango = [2,4,8,16,32,64];
      Max =< 1024  -> Rango = [4,8,16,32,64,128];
      Max =< 2048  -> Rango = [8,16,32,64,128,256];
      Max =< 4096  -> Rango = [16,32,64,128,256,512];
      Max =< 8192  -> Rango = [16,32,64,128,256,512];
      Max =< 16384 -> Rango = [32,64,128,256,512,1024];
      Rango = [32,64,128,256,512,1024]
    ).

% Determinar bloques retirados cuando se alcanza un nuevo máximo
bloques_retirados(MaxAnterior, MaxNuevo, BloquesRetirados) :-
    rango_disparo(MaxAnterior, RangoAnterior),
    rango_disparo(MaxNuevo, RangoNuevo),
    subtract(RangoAnterior, RangoNuevo, BloquesRetirados).

% Determinar bloques agregados cuando se alcanza un nuevo máximo
bloques_agregados(MaxAnterior, MaxNuevo, BloquesAgregados) :-
    rango_disparo(MaxAnterior, RangoAnterior),
    rango_disparo(MaxNuevo, RangoNuevo),
    subtract(RangoNuevo, RangoAnterior, BloquesAgregados).

/**
 * shoot(+Block, +Column, +Grid, +NumOfColumns, -Effects)
 * Función principal con procesamiento completo garantizado
 */
shoot(Block, Column, Grid, NumOfColumns, Effects) :-
    number(Block),
    find_insertion_index(Grid, Column, NumOfColumns, Index),
    insert_block_at_index(Grid, Index, Block, Grid1),
    Effects1 = [effect(Grid1, [])],
    max_in_grid(Grid, MaxAnterior),
    % Procesar todo el ciclo de combinaciones y gravedad
    process_complete_cycle(Grid1, NumOfColumns, Index, Effects1, TempEffects),
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

%% PROCESAMIENTO COMPLETO - GARANTIZA QUE TODO SE PROCESE
% process_complete_cycle(+Grid, +NumCols, +StartIndex, +EffectsAcc, -Effects)
% Procesa combinaciones iniciales, aplica gravedad y busca nuevas combinaciones hasta estabilizar
process_complete_cycle(Grid, NumCols, StartIndex, EffectsAcc, Effects) :-
    % 1. Procesar combinaciones desde la posición inicial
    process_combinations_from_position(Grid, NumCols, StartIndex, EffectsAcc, ComboEffects),
    % 2. Aplicar ciclo completo de gravedad y combinaciones hasta estabilizar
    last(ComboEffects, effect(CurrentGrid, _)),
    apply_complete_gravity_cycle(CurrentGrid, NumCols, ComboEffects, Effects).

process_combinations_from_position(Grid, NumCols, Index, EffectsAcc, Effects) :-
    nth0(Index, Grid, Value),
    Value \= '-',
    find_connected_group(Grid, NumCols, Index, Value, Group),
    length(Group, GroupSize),
    GroupSize >= 2, !,
   determine_destination_position(Index, Group, NumCols, DestPos),
    % Calcular score y nuevo valor
    TotalBlocks is GroupSize,
    calculate_combo_score(Value, TotalBlocks, Score),
    calculate_new_block_value(Value, NewValue),
    % Crear cambios: nuevo valor en DestPos, borrar los demás
    findall((P, '-'), (member(P, Group), P \= DestPos), Clears),
    Changes = [(DestPos, NewValue)|Clears],
    apply_changes(Grid, Changes, Grid2),
    % Información de combo
    ( TotalBlocks >= 3 ->
        ComboInfo = [newBlock(NewValue), combo(TotalBlocks), score(Score)]
    ;   ComboInfo = [newBlock(NewValue), score(Score)]
    ),
    append(EffectsAcc, [effect(Grid2, ComboInfo)], Acc2),
    % Aplicar gravedad inmediatamente
    apply_gravity(Grid2, NumCols, GravityGrid),
    ( Grid2 = GravityGrid ->
        % Sin cambios por gravedad, continuar desde DestPos
        process_combinations_from_position(GravityGrid, NumCols, DestPos, Acc2, Effects)
    ;   % Con cambios, agregar efecto de gravedad y continuar
        append(Acc2, [effect(GravityGrid, [gravity])], Acc3),
        process_combinations_from_position(GravityGrid, NumCols, DestPos, Acc3, Effects)
    ).
process_combinations_from_position(_, _, _, EffectsAcc, EffectsAcc).

% find_all_matching_neighbors(+Grid, +NumCols, +Index, +Value, -Matches)
% Encuentra todos los vecinos directos con el mismo valor
find_all_matching_neighbors(Grid, NumCols, Index, Value, Matches) :-
    findall(Pos,
        ( member(Direction, [left, up, right]),
          neighbor(Index, NumCols, Direction, Pos),
          nth0(Pos, Grid, Value)
        ), Matches).

determine_destination_position(Index, Group, NumCols, DestPos) :-
    length(Group, GroupSize),
    ( GroupSize =:= 2,
      Group = [Pos1, Pos2],
      are_horizontal_neighbors(Pos1, Pos2, NumCols) ->
        DestPos = Index  % Combinación horizontal de 2: posición del bloque disparado
    ; GroupSize =:= 2,
      Group = [Pos1, Pos2],
      are_vertical_neighbors(Pos1, Pos2, NumCols) ->
        % Combinación vertical de 2: elegir la posición superior (menor índice)
        ( Pos1 < Pos2 -> DestPos = Pos1 ; DestPos = Pos2 )
    ; GroupSize >= 2 ->
        % Para tres o más bloques, calcular el centro geométrico
        maplist(pos_to_rc(NumCols), Group, RCPairs),
        unzip(RCPairs, Rows, Cols),
        average(Rows, AvgRow), average(Cols, AvgCol),
        round(AvgRow, RoundRow), round(AvgCol, RoundCol),
        DestPos is RoundRow * NumCols + RoundCol
    ).

% Verifica si dos posiciones son vecinos horizontales
are_horizontal_neighbors(Pos1, Pos2, NumCols) :-
    Row1 is Pos1 // NumCols,
    Row2 is Pos2 // NumCols,
    Row1 =:= Row2,
    abs(Pos1 - Pos2) =:= 1.

% Verifica si dos posiciones son vecinos verticales
are_vertical_neighbors(Pos1, Pos2, NumCols) :-
    Col1 is Pos1 mod NumCols,
    Col2 is Pos2 mod NumCols,
    Col1 =:= Col2,              % Misma columna
    abs(Pos1 - Pos2) =:= NumCols. % Diferencia de una fila

% Helpers para la posición de destino geométrico
pos_to_rc(NumCols, Pos, (R,C)) :- R is Pos // NumCols, C is Pos mod NumCols.
unzip([], [], []).
unzip([(A,B)|T], [A|As], [B|Bs]) :- unzip(T, As, Bs).
average(List, Avg) :- sum_list(List, Sum), length(List, Len), Len > 0, Avg is Sum / Len.
round(X, R) :- R is round(X).
is_horizontal_neighbor(P1, P2, NumCols) :-
    Row1 is P1 // NumCols, Row2 is P2 // NumCols,
    Row1 =:= Row2, abs(P1 - P2) =:= 1.

% find_center_position(Positions, -CenterPos)
% Encuentra la posición central geométricamente
find_center_position(Positions, CenterPos) :-
    sort(Positions, SortedPositions),
    length(SortedPositions, Len),
    ( Len mod 2 =:= 1 ->
        % Cantidad impar: tomar el del medio
        MiddleIndex is Len // 2,
        nth0(MiddleIndex, SortedPositions, CenterPos)
    ;   % Cantidad par: tomar el de la izquierda del centro
        MiddleIndex is (Len // 2) - 1,
        nth0(MiddleIndex, SortedPositions, CenterPos)
    ).

% is_horizontal_neighbor(+Pos1, +Pos2, +NumCols)
% Verifica si dos posiciones son vecinos horizontales
is_horizontal_neighbor(Pos1, Pos2, NumCols) :-
    Row1 is Pos1 // NumCols,
    Row2 is Pos2 // NumCols,
    Row1 =:= Row2,
    abs(Pos1 - Pos2) =:= 1.


% calculate_combo_score(+BaseValue, +NumBlocks, -Score)
% SCORE = BaseValue * NumBlocks (Ej: bloque 2 con 3 bloques = 2*3 = 6 puntos)
calculate_combo_score(BaseValue, NumBlocks, Score) :-
    Score is BaseValue * NumBlocks,
    % DEBUG: Imprimir score en consola
    format('[DEBUG SCORE] BaseValue: ~w, NumBlocks: ~w, Score calculado: ~w~n', [BaseValue, NumBlocks, Score]).

% calculate_new_block_value(+BaseValue, -NewValue)
% NUEVO BLOQUE = BaseValue * 2 (Ej: combo de bloques 2 = nuevo bloque 4)
% Esto es INDEPENDIENTE del tamaño del combo
calculate_new_block_value(BaseValue, NewValue) :-
    NewValue is BaseValue * 2,
    % DEBUG: Imprimir nuevo valor en consola
    format('[DEBUG NUEVO BLOQUE] BaseValue: ~w, NewValue calculado: ~w~n', [BaseValue, NewValue]).

% CICLO SIMPLIFICADO: Ya no necesita ciclo complejo porque aplicamos gravedad inmediatamente
% apply_complete_gravity_cycle(+Grid, +NumCols, +EffectsAcc, -Effects)
% SIMPLIFICADO: Solo aplica gravedad final y busca combinaciones restantes
apply_complete_gravity_cycle(Grid, NumCols, EffectsAcc, Effects) :-
    apply_gravity(Grid, NumCols, GravityGrid),
    ( Grid = GravityGrid ->
        % No hubo cambios por gravedad, buscar combinaciones finales
        check_and_process_remaining_combinations(GravityGrid, NumCols, EffectsAcc, Effects)
    ;   % Hubo cambios, agregar efecto de gravedad
        append(EffectsAcc, [effect(GravityGrid, [gravity])], TempEffects),
        % Buscar combinaciones restantes
        check_and_process_remaining_combinations(GravityGrid, NumCols, TempEffects, Effects)
    ).

% check_and_process_all_combinations(+Grid, +NumCols, +EffectsAcc, -Effects)
% Busca y procesa TODAS las combinaciones posibles en la grilla
check_and_process_all_combinations(Grid, NumCols, EffectsAcc, Effects) :-
    length(Grid, Len),
    process_all_positions_exhaustively(Grid, NumCols, 0, Len, EffectsAcc, Effects).

% process_all_positions_exhaustively(+Grid, +NumCols, +Index, +Len, +EffectsAcc, -Effects)
% Procesa exhaustivamente todas las posiciones hasta que no haya más combinaciones
process_all_positions_exhaustively(Grid, NumCols, Index, Len, EffectsAcc, Effects) :-
    Index >= Len, !,
    % Terminamos de revisar todas las posiciones, verificar si hubo cambios
    last(EffectsAcc, effect(CurrentGrid, _)),
    ( CurrentGrid = Grid ->
        Effects = EffectsAcc  % No hubo cambios, terminar
    ;   % Hubo cambios, volver a revisar desde el principio
        process_all_positions_exhaustively(CurrentGrid, NumCols, 0, Len, EffectsAcc, Effects)
    ).
process_all_positions_exhaustively(Grid, NumCols, Index, Len, EffectsAcc, Effects) :-
    Index < Len,
    nth0(Index, Grid, Value),
    ( Value \= '-',
      has_combination_at(Grid, NumCols, Index) ->
        % Encontramos una combinación, procesarla
        process_combinations_from_position(Grid, NumCols, Index, EffectsAcc, TempEffects),
        % Continuar desde donde nos quedamos con la nueva grilla
        last(TempEffects, effect(NewGrid, _)),
        process_all_positions_exhaustively(NewGrid, NumCols, Index, Len, TempEffects, Effects)
    ;   % No hay combinación en esta posición, seguir
        NextIndex is Index + 1,
        process_all_positions_exhaustively(Grid, NumCols, NextIndex, Len, EffectsAcc, Effects)
    ).

% check_and_process_remaining_combinations(+Grid, +NumCols, +EffectsAcc, -Effects)
% MEJORADO: Busca y procesa combinaciones restantes de forma más eficiente
check_and_process_remaining_combinations(Grid, NumCols, EffectsAcc, Effects) :-
    find_first_combination_position(Grid, NumCols, Position),
    ( Position = none ->
        % No hay más combinaciones
        Effects = EffectsAcc
    ;   % Hay una combinación, procesarla
        process_combinations_from_position(Grid, NumCols, Position, EffectsAcc, TempEffects),
        % Verificar si hay más combinaciones después de esta
        last(TempEffects, effect(NewGrid, _)),
        check_and_process_remaining_combinations(NewGrid, NumCols, TempEffects, Effects)
    ).

% find_first_combination_position(+Grid, +NumCols, -Position)
% Encuentra la primera posición con una combinación disponible
find_first_combination_position(Grid, NumCols, Position) :-
    length(Grid, Len),
    between(0, Len, Index),
    Index < Len,
    has_combination_at(Grid, NumCols, Index),
    !,
    Position = Index.
find_first_combination_position(_, _, none).

% Verificar si hay una combinación en una posición específica
has_combination_at(Grid, NumCols, Index) :-
    nth0(Index, Grid, Value),
    Value \= '-',
    (   neighbor(Index, NumCols, up,    Dest), nth0(Dest, Grid, Value)
    ;   neighbor(Index, NumCols, left,  Dest), nth0(Dest, Grid, Value)
    ;   neighbor(Index, NumCols, right, Dest), nth0(Dest, Grid, Value)
    ), !.

%% FUNCIONES DE GRAVEDAD (sin cambios)
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
            append(Notifications, [newBlockAdded(BloquesAgregados)], TempNotif1)  % ← Incluir la lista
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
            apply_complete_gravity_cycle(CleanGrid, NumCols, TempEffects, Effects)
        ;   % No hay bloques retirados, agregar notificaciones al último efecto
            last(EffectsAcc, effect(LastGrid, LastNotifications)),
            append(LastNotifications, FinalNotifications, UpdatedNotifications),
            append(InitialEffects, [effect(LastGrid, LastNotifications)], EffectsAcc),
            append(InitialEffects, [effect(LastGrid, UpdatedNotifications)], TempEffects),
            apply_complete_gravity_cycle(Grid, NumCols, TempEffects, Effects)
        )
    ;   % No hay nuevo máximo, solo aplicar gravedad
        apply_complete_gravity_cycle(Grid, NumCols, EffectsAcc, Effects)
    ).

% cleanup_retired_blocks(+Grid, +RetiredBlocks, -CleanGrid)
cleanup_retired_blocks(Grid, RetiredBlocks, CleanGrid) :-
    findall((Index, '-'),
        ( nth0(Index, Grid, Value),
          member(Value, RetiredBlocks)
        ), Changes),
    apply_changes(Grid, Changes, CleanGrid).

%% FUNCIONES DE HINT Y GENERACIÓN CORREGIDAS
% hint_shot(+Block, +Column, +Grid, +NumOfColumns, -HintInfo)
% Predice el resultado con validación de bloques eliminados
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
        
        % Si hay combinación, calcular el próximo bloque con validación
        ( member(newBlock(_), ComboInfo) ->
            % Simular el disparo para obtener la grilla final
            simulate_shot_outcome(Block, Column, Grid, NumOfColumns, FinalGrid),
            % Generar el próximo bloque validando que esté disponible
            generate_valid_next_block(FinalGrid, NextBlock),
            append(ComboInfo, [nextBlock(NextBlock)], HintInfo)
        ;   % No hay combinación, próximo bloque basado en la grilla actual
            generate_valid_next_block(Grid1, NextBlock),
            append(ComboInfo, [nextBlock(NextBlock)], HintInfo)
        )
    ).

% ACTUALIZACIÓN EN analyze_immediate_combination para usar las funciones separadas
analyze_immediate_combination(Grid, NumCols, Index, ComboInfo) :-
    nth0(Index, Grid, Value),
    Value \= '-',
    find_all_matching_neighbors(Grid, NumCols, Index, Value, Neighbors),
    
    ( Neighbors = [] ->
        ComboInfo = [no_combo]
    ;   length(Neighbors, NumNeighbors),
        TotalBlocks is NumNeighbors + 1,
        calculate_combo_score(Value, TotalBlocks, Score),
        calculate_new_block_value(Value, NewValue),
        ( TotalBlocks >= 3 ->
            ComboInfo = [combo(TotalBlocks), newBlock(NewValue), score(Score)]
        ;   ComboInfo = [simple_combo, newBlock(NewValue), score(Score)]
        )
    ).

% simulate_shot_outcome(+Block, +Column, +Grid, +NumOfColumns, -FinalGrid)
simulate_shot_outcome(Block, Column, Grid, NumOfColumns, FinalGrid) :-
    shoot(Block, Column, Grid, NumOfColumns, Effects),
    last(Effects, effect(FinalGrid, _)).

% generate_valid_next_block(+Grid, -NextBlock)
% Genera un bloque válido verificando que esté en el rango disponible
generate_valid_next_block(Grid, NextBlock) :-
    max_in_grid(Grid, Max),
    rango_disparo(Max, Rango),
    % Intentar hasta 10 veces generar un bloque válido
    between(1, 10, _),
    random_member(NextBlock, Rango),
    !.
generate_valid_next_block(Grid, NextBlock) :-
    % Si falla, tomar el primer bloque del rango
    max_in_grid(Grid, Max),
    rango_disparo(Max, [NextBlock|_]).

% Generar par de bloques con validación
generate_block_pair(Grid, CurrentBlock, NextBlock) :-
    generate_valid_next_block(Grid, CurrentBlock),
    generate_valid_next_block(Grid, NextBlock).

% Inicializar estado del juego
init_game_state(Grid, NumCols, GameState) :-
    generate_block_pair(Grid, CurrentBlock, NextBlock),
    GameState = gameState(Grid, NumCols, CurrentBlock, NextBlock).

% update_game_state(+GameState, +Column, -NewGameState)
% Actualiza el estado del juego después de disparar un bloque
% CORREGIDO: Valida y limpia bloques eliminados de la cola
update_game_state(gameState(Grid, NumCols, CurrentBlock, NextBlock), Column, NewGameState) :-
    % Disparar el CurrentBlock y obtener los efectos
    shoot(CurrentBlock, Column, Grid, NumCols, Effects),
    % Obtener la grilla final después de procesar los efectos
    last(Effects, effect(FinalGrid, _)),
    % Determinar el máximo anterior y el nuevo máximo
    max_in_grid(Grid, MaxAnterior),
    max_in_grid(FinalGrid, MaxNuevo),
    % Si hay un nuevo máximo, verificar bloques eliminados
    ( MaxNuevo > MaxAnterior ->
        % Obtener la lista de bloques retirados
        bloques_retirados(MaxAnterior, MaxNuevo, RetiredBlocks),
        % Validar ambos bloques en cola
        validate_and_update_queue(FinalGrid, NextBlock, RetiredBlocks, ValidNextBlock, NeedsNewCurrent),
        % Generar bloques según la validación
        ( NeedsNewCurrent = true ->
            % NextBlock fue eliminado, generar nuevo par completo
            generate_block_pair(FinalGrid, NewCurrentBlock, NewNextBlock),
            NewGameState = gameState(FinalGrid, NumCols, NewCurrentBlock, NewNextBlock)
        ;   % NextBlock es válido, solo generar nuevo NextBlock
            generate_valid_next_block(FinalGrid, NewNextBlock),
            NewGameState = gameState(FinalGrid, NumCols, ValidNextBlock, NewNextBlock)
        )
    ;   % No hay nuevo máximo, avanzar la cola normalmente
        generate_valid_next_block(FinalGrid, NewNextBlock),
        NewGameState = gameState(FinalGrid, NumCols, NextBlock, NewNextBlock)
    ).

% validate_and_update_queue(+Grid, +NextBlock, +RetiredBlocks, -ValidNextBlock, -NeedsNewCurrent)
% Valida los bloques en cola y determina si necesitan ser reemplazados
validate_and_update_queue(Grid, NextBlock, RetiredBlocks, ValidNextBlock, NeedsNewCurrent) :-
    ( member(NextBlock, RetiredBlocks) ->
        % NextBlock fue eliminado, necesitamos generar nuevo bloque actual también
        ValidNextBlock = none,
        NeedsNewCurrent = true
    ;   % NextBlock sigue siendo válido
        ValidNextBlock = NextBlock,
        NeedsNewCurrent = false
    ).

% validate_block_in_range(+Block, +Grid)
% Verifica si un bloque está en el rango válido de disparo
validate_block_in_range(Block, Grid) :-
    max_in_grid(Grid, Max),
    rango_disparo(Max, Rango),
    member(Block, Rango).

% clean_invalid_blocks_from_queue(+CurrentBlock, +NextBlock, +Grid, -ValidCurrentBlock, -ValidNextBlock, -BlocksCleaned)
% Limpia bloques inválidos de la cola de disparo
clean_invalid_blocks_from_queue(CurrentBlock, NextBlock, Grid, ValidCurrentBlock, ValidNextBlock, BlocksCleaned) :-
    % Verificar CurrentBlock
    ( validate_block_in_range(CurrentBlock, Grid) ->
        ValidCurrentBlock = CurrentBlock,
        CurrentCleaned = false
    ;   generate_valid_next_block(Grid, ValidCurrentBlock),
        CurrentCleaned = true
    ),
    % Verificar NextBlock
    ( validate_block_in_range(NextBlock, Grid) ->
        ValidNextBlock = NextBlock,
        NextCleaned = false
    ;   generate_valid_next_block(Grid, ValidNextBlock),
        NextCleaned = true
    ),
    % Determinar qué bloques fueron limpiados
    findall(Block, 
        ( ( CurrentCleaned = true, Block = CurrentBlock )
        ; ( NextCleaned = true, Block = NextBlock )
        ), BlocksCleaned).

%% apply_changes(+Grid, +Changes, -NewGrid)
apply_changes(Grid, [], Grid).
apply_changes(Grid, [(I, V)|T], NewGrid) :-
    nth0(I, Grid, _, Rest),
    nth0(I, Temp, V, Rest),
    apply_changes(Temp, T, NewGrid).

% find_connected_group(+Grid, +NumCols, +StartIndex, +Value, -Group)
% Encuentra todos los bloques conectados con valor Value a partir de StartIndex
find_connected_group(Grid, NumCols, StartIndex, Value, Group) :-
    find_connected_group_bfs(Grid, NumCols, [StartIndex], Value, [], Group).

% BFS para encontrar bloques conectados
find_connected_group_bfs(_, _, [], _, Visited, Group) :-
    sort(Visited, Group).
find_connected_group_bfs(Grid, NumCols, [Index|Queue], Value, Visited, Group) :-
    ( member(Index, Visited) ->
        find_connected_group_bfs(Grid, NumCols, Queue, Value, Visited, Group)
    ;   nth0(Index, Grid, V),
        ( V == Value ->
            find_neighbors(Index, NumCols, Neighbors),
            append(Queue, Neighbors, NewQueue),
            find_connected_group_bfs(Grid, NumCols, NewQueue, Value, [Index|Visited], Group)
        ;   find_connected_group_bfs(Grid, NumCols, Queue, Value, Visited, Group)
        )
    ).

% Encuentra vecinos en todas las direcciones
find_neighbors(Index, NumCols, Neighbors) :-
    findall(N,
        ( member(Dir, [up, left, right, down]),
          neighbor(Index, NumCols, Dir, N)
        ), Neighbors).