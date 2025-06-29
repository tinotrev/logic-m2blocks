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

%max_in_grid(+Grid, -Max)
max_in_grid(Grid, Max) :-
    include(number, Grid, Numeros),
    max_list(Numeros, Max).

%rango_disparo(+Max, -Rango)
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

%randomBlock(+Grid, -Block)
randomBlock(Grid, Block) :-
    max_in_grid(Grid, Max),
    rango_disparo(Max, Rango),
    random_member(Block, Rango).

%find_insertion_index(+Grid, +Column, +NumOfColumns, -Index)
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

%insert_block_at_index(+Grid, +Index, +Block, -NewGrid)
insert_block_at_index(Grid, Index, Block, NewGrid) :-
    nth0(Index, Grid, _, Rest),
    nth0(Index, NewGrid, Block, Rest).

%apply_gravity_to_column(+Column, -GravityColumn)
apply_gravity_to_column(Column, GravityColumn) :-
    include(\=('-'), Column, Blocks),
    length(Column, Len),
    length(Blocks, NumBlocks),
    NumEmpty is Len - NumBlocks,
    length(EmptySpaces, NumEmpty),
    maplist(=('-'), EmptySpaces),
    append(Blocks, EmptySpaces, GravityColumn).

%replace_column(+Grid, +NumCols, +Rows, +Col, +NewColumn, -NewGrid)
replace_column(Grid, NumCols, Rows, Col, NewColumn, NewGrid) :-
    replace_column_helper(Grid, NumCols, Rows, Col, NewColumn, 0, NewGrid).

%replace_column_helper(+Grid, +NumCols, +Rows, +Col, +NewColumn, +Row, -NewGrid)
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

%extract_column(+Grid, +NumCols, +Rows, +Col, -Column)
extract_column(Grid, NumCols, Rows, Col, Column) :-
    findall(Value,
        ( between(0, Rows, Row),
          Row < Rows,
          Index is Row * NumCols + Col,
          nth0(Index, Grid, Value)
        ), Column).

%apply_gravity_to_columns(+Grid, +NumCols, +Rows, +Col, -NewGrid)
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

%apply_gravity(+Grid, +NumCols, -NewGrid)
apply_gravity(Grid, NumCols, NewGrid) :-
    length(Grid, Len),
    Rows is Len // NumCols,
    apply_gravity_to_columns(Grid, NumCols, Rows, 0, NewGrid).

%find_neighbors(+Index, +NumCols, -Neighbors)
find_neighbors(Index, NumCols, Neighbors) :-
    findall(N,
        ( member(Dir, [up, left, right, down]),
          neighbor(Index, NumCols, Dir, N)
        ), Neighbors).

%find_connected_group_bfs(+Grid, +NumCols, +Queue, +Value, +Visited, -Group)
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

%find_connected_group(+Grid, +NumCols, +StartIndex, +Value, -Group)
find_connected_group(Grid, NumCols, StartIndex, Value, Group) :-
    find_connected_group_bfs(Grid, NumCols, [StartIndex], Value, [], Group).

%pos_to_rc(+NumCols, +Pos, -RC)
pos_to_rc(NumCols, Pos, (R,C)) :- 
    R is Pos // NumCols, 
    C is Pos mod NumCols.

%unzip(+List, -FirstElements, -SecondElements)
unzip([], [], []).
unzip([(A,B)|T], [A|As], [B|Bs]) :- 
    unzip(T, As, Bs).

%average(+List, -Avg)
average(List, Avg) :- 
    sum_list(List, Sum), 
    length(List, Len), 
    Len > 0, 
    Avg is Sum / Len.

%round(+X, -R)
round(X, R) :- 
    R is round(X).

%are_horizontal_neighbors(+Pos1, +Pos2, +NumCols)
are_horizontal_neighbors(Pos1, Pos2, NumCols) :-
    Row1 is Pos1 // NumCols,
    Row2 is Pos2 // NumCols,
    Row1 =:= Row2,
    abs(Pos1 - Pos2) =:= 1.

%are_vertical_neighbors(+Pos1, +Pos2, +NumCols)
are_vertical_neighbors(Pos1, Pos2, NumCols) :-
    Col1 is Pos1 mod NumCols,
    Col2 is Pos2 mod NumCols,
    Col1 =:= Col2,
    abs(Pos1 - Pos2) =:= NumCols.

%determine_destination_position(+Index, +Group, +NumCols, -DestPos)
determine_destination_position(Index, Group, NumCols, DestPos) :-
    length(Group, GroupSize),
    ( GroupSize =:= 2,
      Group = [Pos1, Pos2],
      are_horizontal_neighbors(Pos1, Pos2, NumCols) ->
        DestPos = Index
    ; GroupSize =:= 2,
      Group = [Pos1, Pos2],
      are_vertical_neighbors(Pos1, Pos2, NumCols) ->
        ( Pos1 < Pos2 -> DestPos = Pos1 ; DestPos = Pos2 )
    ; GroupSize >= 2 ->
        maplist(pos_to_rc(NumCols), Group, RCPairs),
        unzip(RCPairs, Rows, Cols),
        average(Rows, AvgRow), average(Cols, AvgCol),
        round(AvgRow, RoundRow), round(AvgCol, RoundCol),
        DestPos is RoundRow * NumCols + RoundCol
    ).

%calculate_combo_score(+BaseValue, +NumBlocks, -Score)
calculate_combo_score(BaseValue, NumBlocks, Score) :-
    Score is BaseValue * NumBlocks,
    format('[DEBUG SCORE] BaseValue: ~w, NumBlocks: ~w, Score calculado: ~w~n', [BaseValue, NumBlocks, Score]).

%calculate_new_block_value(+BaseValue, -NewValue)
calculate_new_block_value(BaseValue, NewValue) :-
    NewValue is BaseValue * 2,
    format('[DEBUG NUEVO BLOQUE] BaseValue: ~w, NewValue calculado: ~w~n', [BaseValue, NewValue]).

%apply_changes(+Grid, +Changes, -NewGrid)
apply_changes(Grid, [], Grid).
apply_changes(Grid, [(I, V)|T], NewGrid) :-
    nth0(I, Grid, _, Rest),
    nth0(I, Temp, V, Rest),
    apply_changes(Temp, T, NewGrid).

%process_combinations_from_position(+Grid, +NumCols, +Index, +EffectsAcc, -Effects)
process_combinations_from_position(Grid, NumCols, Index, EffectsAcc, Effects) :-
    nth0(Index, Grid, Value),
    Value \= '-',
    find_connected_group(Grid, NumCols, Index, Value, Group),
    length(Group, GroupSize),
    GroupSize >= 2, !,
    determine_destination_position(Index, Group, NumCols, DestPos),
    TotalBlocks is GroupSize,
    calculate_combo_score(Value, TotalBlocks, Score),
    calculate_new_block_value(Value, NewValue),
    findall((P, '-'), (member(P, Group), P \= DestPos), Clears),
    Changes = [(DestPos, NewValue)|Clears],
    apply_changes(Grid, Changes, Grid2),
    ( TotalBlocks >= 3 ->
        ComboInfo = [newBlock(NewValue), combo(TotalBlocks), score(Score)]
    ;   ComboInfo = [newBlock(NewValue), score(Score)]
    ),
    append(EffectsAcc, [effect(Grid2, ComboInfo)], Acc2),
    apply_gravity(Grid2, NumCols, GravityGrid),
    ( Grid2 = GravityGrid ->
        process_combinations_from_position(GravityGrid, NumCols, DestPos, Acc2, Effects)
    ;   append(Acc2, [effect(GravityGrid, [gravity])], Acc3),
        process_combinations_from_position(GravityGrid, NumCols, DestPos, Acc3, Effects)
    ).
process_combinations_from_position(_, _, _, EffectsAcc, EffectsAcc).

%neighbor(+Index, +NumCols, +Dir, -NeighborIndex)
neighbor(Index, NumCols, up,    N) :- Index >= NumCols,        N is Index - NumCols.
neighbor(Index, NumCols, left,  N) :- Col is Index mod NumCols, Col > 0, N is Index - 1.
neighbor(Index, NumCols, right, N) :- Col is Index mod NumCols, Col < NumCols - 1, N is Index + 1.

%has_combination_at(+Grid, +NumCols, +Index)
has_combination_at(Grid, NumCols, Index) :-
    nth0(Index, Grid, Value),
    Value \= '-',
    (   neighbor(Index, NumCols, up,    Dest), nth0(Dest, Grid, Value)
    ;   neighbor(Index, NumCols, left,  Dest), nth0(Dest, Grid, Value)
    ;   neighbor(Index, NumCols, right, Dest), nth0(Dest, Grid, Value)
    ), !.

%find_first_combination_position(+Grid, +NumCols, -Position)
find_first_combination_position(Grid, NumCols, Position) :-
    length(Grid, Len),
    between(0, Len, Index),
    Index < Len,
    has_combination_at(Grid, NumCols, Index),
    !,
    Position = Index.
find_first_combination_position(_, _, none).

%check_and_process_remaining_combinations(+Grid, +NumCols, +EffectsAcc, -Effects)
check_and_process_remaining_combinations(Grid, NumCols, EffectsAcc, Effects) :-
    find_first_combination_position(Grid, NumCols, Position),
    ( Position = none ->
        Effects = EffectsAcc
    ;   process_combinations_from_position(Grid, NumCols, Position, EffectsAcc, TempEffects),
        last(TempEffects, effect(NewGrid, _)),
        check_and_process_remaining_combinations(NewGrid, NumCols, TempEffects, Effects)
    ).

%apply_complete_gravity_cycle(+Grid, +NumCols, +EffectsAcc, -Effects)
apply_complete_gravity_cycle(Grid, NumCols, EffectsAcc, Effects) :-
    apply_gravity(Grid, NumCols, GravityGrid),
    ( Grid = GravityGrid ->
        check_and_process_remaining_combinations(GravityGrid, NumCols, EffectsAcc, Effects)
    ;   append(EffectsAcc, [effect(GravityGrid, [gravity])], TempEffects),
        check_and_process_remaining_combinations(GravityGrid, NumCols, TempEffects, Effects)
    ).

%cleanup_retired_blocks(+Grid, +RetiredBlocks, -CleanGrid)
cleanup_retired_blocks(Grid, RetiredBlocks, CleanGrid) :-
    findall((Index, '-'),
        ( nth0(Index, Grid, Value),
          member(Value, RetiredBlocks)
        ), Changes),
    apply_changes(Grid, Changes, CleanGrid).

%bloques_retirados(+MaxAnterior, +MaxNuevo, -BloquesRetirados)
bloques_retirados(MaxAnterior, MaxNuevo, BloquesRetirados) :-
    rango_disparo(MaxAnterior, RangoAnterior),
    rango_disparo(MaxNuevo, RangoNuevo),
    subtract(RangoAnterior, RangoNuevo, BloquesRetirados).

%bloques_agregados(+MaxAnterior, +MaxNuevo, -BloquesAgregados)
bloques_agregados(MaxAnterior, MaxNuevo, BloquesAgregados) :-
    rango_disparo(MaxAnterior, RangoAnterior),
    rango_disparo(MaxNuevo, RangoNuevo),
    subtract(RangoNuevo, RangoAnterior, BloquesAgregados).

%process_cleanup_and_notifications(+MaxAnterior, +MaxNuevo, +Grid, +NumCols, +EffectsAcc, -Effects)
process_cleanup_and_notifications(MaxAnterior, MaxNuevo, Grid, NumCols, EffectsAcc, Effects) :-
    ( MaxNuevo > MaxAnterior ->
        bloques_retirados(MaxAnterior, MaxNuevo, BloquesRetirados),
        bloques_agregados(MaxAnterior, MaxNuevo, BloquesAgregados),
        
        ( MaxNuevo >= 512 ->
            Notifications = [newMaxBlock(MaxNuevo)]
        ;   Notifications = []
        ),
        ( BloquesAgregados \= [] -> 
            append(Notifications, [newBlockAdded(BloquesAgregados)], TempNotif1)
        ;   TempNotif1 = Notifications
        ),
        ( BloquesRetirados \= [] ->
            append(TempNotif1, [blockEliminated], FinalNotifications)
        ;   FinalNotifications = TempNotif1
        ),
        
        ( BloquesRetirados \= [] ->
            cleanup_retired_blocks(Grid, BloquesRetirados, CleanGrid),
            append(FinalNotifications, [cleanup(BloquesRetirados)], AllNotifications),
            append(EffectsAcc, [effect(CleanGrid, AllNotifications)], TempEffects),
            apply_complete_gravity_cycle(CleanGrid, NumCols, TempEffects, Effects)
        ;   last(EffectsAcc, effect(LastGrid, LastNotifications)),
            append(LastNotifications, FinalNotifications, UpdatedNotifications),
            append(InitialEffects, [effect(LastGrid, LastNotifications)], EffectsAcc),
            append(InitialEffects, [effect(LastGrid, UpdatedNotifications)], TempEffects),
            apply_complete_gravity_cycle(Grid, NumCols, TempEffects, Effects)
        )
    ;   apply_complete_gravity_cycle(Grid, NumCols, EffectsAcc, Effects)
    ).

%process_complete_cycle(+Grid, +NumCols, +StartIndex, +EffectsAcc, -Effects)
process_complete_cycle(Grid, NumCols, StartIndex, EffectsAcc, Effects) :-
    process_combinations_from_position(Grid, NumCols, StartIndex, EffectsAcc, ComboEffects),
    last(ComboEffects, effect(CurrentGrid, _)),
    apply_complete_gravity_cycle(CurrentGrid, NumCols, ComboEffects, Effects).

%shoot(+Block, +Column, +Grid, +NumOfColumns, -Effects)
shoot(Block, Column, Grid, NumOfColumns, Effects) :-
    number(Block),
    find_insertion_index(Grid, Column, NumOfColumns, Index),
    insert_block_at_index(Grid, Index, Block, Grid1),
    Effects1 = [effect(Grid1, [])],
    max_in_grid(Grid, MaxAnterior),
    process_complete_cycle(Grid1, NumOfColumns, Index, Effects1, TempEffects),
    last(TempEffects, effect(FinalGrid, _)),
    max_in_grid(FinalGrid, MaxNuevo),
    process_cleanup_and_notifications(MaxAnterior, MaxNuevo, FinalGrid, NumOfColumns, TempEffects, Effects).

%generate_valid_next_block(+Grid, -NextBlock)
generate_valid_next_block(Grid, NextBlock) :-
    max_in_grid(Grid, Max),
    rango_disparo(Max, Rango),
    between(1, 10, _),
    random_member(NextBlock, Rango),
    !.
generate_valid_next_block(Grid, NextBlock) :-
    max_in_grid(Grid, Max),
    rango_disparo(Max, [NextBlock|_]).

%generate_block_pair(+Grid, -CurrentBlock, -NextBlock)
generate_block_pair(Grid, CurrentBlock, NextBlock) :-
    generate_valid_next_block(Grid, CurrentBlock),
    generate_valid_next_block(Grid, NextBlock).

%init_game_state(+Grid, +NumCols, -GameState)
init_game_state(Grid, NumCols, GameState) :-
    generate_block_pair(Grid, CurrentBlock, NextBlock),
    GameState = gameState(Grid, NumCols, CurrentBlock, NextBlock).

%validate_and_update_queue(+Grid, +NextBlock, +RetiredBlocks, -ValidNextBlock, -NeedsNewCurrent)
validate_and_update_queue(Grid, NextBlock, RetiredBlocks, ValidNextBlock, NeedsNewCurrent) :-
    ( member(NextBlock, RetiredBlocks) ->
        ValidNextBlock = none,
        NeedsNewCurrent = true
    ;   ValidNextBlock = NextBlock,
        NeedsNewCurrent = false
    ).

%update_game_state(+GameState, +Column, -NewGameState)
update_game_state(gameState(Grid, NumCols, CurrentBlock, NextBlock), Column, NewGameState) :-
    shoot(CurrentBlock, Column, Grid, NumCols, Effects),
    last(Effects, effect(FinalGrid, _)),
    max_in_grid(Grid, MaxAnterior),
    max_in_grid(FinalGrid, MaxNuevo),
    ( MaxNuevo > MaxAnterior ->
        bloques_retirados(MaxAnterior, MaxNuevo, RetiredBlocks),
        validate_and_update_queue(FinalGrid, NextBlock, RetiredBlocks, ValidNextBlock, NeedsNewCurrent),
        ( NeedsNewCurrent = true ->
            generate_block_pair(FinalGrid, NewCurrentBlock, NewNextBlock),
            NewGameState = gameState(FinalGrid, NumCols, NewCurrentBlock, NewNextBlock)
        ;   generate_valid_next_block(FinalGrid, NewNextBlock),
            NewGameState = gameState(FinalGrid, NumCols, ValidNextBlock, NewNextBlock)
        )
    ;   generate_valid_next_block(FinalGrid, NewNextBlock),
        NewGameState = gameState(FinalGrid, NumCols, NextBlock, NewNextBlock)
    ).

%find_all_matching_neighbors(+Grid, +NumCols, +Index, +Value, -Matches)
find_all_matching_neighbors(Grid, NumCols, Index, Value, Matches) :-
    findall(Pos,
        ( member(Direction, [left, up, right]),
          neighbor(Index, NumCols, Direction, Pos),
          nth0(Pos, Grid, Value)
        ), Matches).


    ).

%simulate_shot_outcome(+Block, +Column, +Grid, +NumOfColumns, -FinalGrid)
simulate_shot_outcome(Block, Column, Grid, NumOfColumns, FinalGrid) :-
    shoot(Block, Column, Grid, NumOfColumns, Effects),
    last(Effects, effect(FinalGrid, _)).

%analyze_immediate_combination(+Grid, +NumCols, +Index, -ComboInfo)
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

%hint_shot(+Block, +Column, +Grid, +NumOfColumns, -HintInfo)
hint_shot(Block, Column, Grid, NumOfColumns, HintInfo) :-
    number(Block),
    ( \+ find_insertion_index(Grid, Column, NumOfColumns, _) ->
        HintInfo = [no_shot]
    ;   find_insertion_index(Grid, Column, NumOfColumns, Index),
        insert_block_at_index(Grid, Index, Block, Grid1),
        
        analyze_immediate_combination(Grid1, NumOfColumns, Index, ComboInfo),
        
        ( member(newBlock(_), ComboInfo) ->
            simulate_shot_outcome(Block, Column, Grid, NumOfColumns, FinalGrid),
            generate_valid_next_block(FinalGrid, NextBlock),
            append(ComboInfo, [nextBlock(NextBlock)], HintInfo)
        ;   generate_valid_next_block(Grid1, NextBlock),
            append(ComboInfo, [nextBlock(NextBlock)], HintInfo)
        )
    ).