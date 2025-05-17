%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FLP Project 2 - Babylon Tower
% Author: Roman Janota
% Email: xjanot04@fit.vutbr.cz
% Date: 25.04.2025
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpfd)).
:- include('input2.pl').

% Database of visited states for dynamic A*
:- dynamic visited/2.

%% write_state(+State)
write_state(State) :-
    write_state(State, 0).
write_state([], _).
write_state([Row | Rest], RowIndex) :-
    write_row(Row),
    nl,
    NextRowIndex is RowIndex + 1,
    write_state(Rest, NextRowIndex).

%% write_row(+Row)
write_row([]).
write_row([ball(Color, Shade) | Rest]) :-
    write(Color),
    write(Shade),
    write(' '),
    write_row(Rest).
write_row([gap | Rest]) :-
    write('** '),
    write_row(Rest).

%% print_solution(+Solution)
print_solution([]).
print_solution([State | []]) :-
    write_state(State).
print_solution([State | Rest]) :-
    write_state(State),
    nl,
    print_solution(Rest).

% parse(+Lines, -InitialState)
parse(Lines, InitialState) :-
    maplist(parse_line, Lines, InitialState).

%% parse_line(+Line, -State)
parse_line(Line, State) :-
    maplist(parse_ball, Line, State).

%% parse_ball(+Ball, -ParsedBall)
parse_ball(['*', '*'], gap).
parse_ball([C|Digits], ball(C, Shade)) :-
    C \= '*',
    digits_to_number(Digits, Shade).

%% digits_to_number(+Digits, -Number)
digits_to_number(Digits, Number) :-
    % convert the list of digits to a number
    foldl(digit_acc, Digits, 0, Number).

%% digit_acc(+Digit, +Acc, -Result)
digit_acc(Digit, Acc, Result) :-
    % convert the character to its numeric value
    char_code(Digit, Code),
    DigitValue is Code - 48,
    Result is Acc * 10 + DigitValue.

%% goal(+State)
% Check if the state is a goal state.
% A goal state is when all balls are in their correct columns,
% the shades are in increasing order, and the gap is at the bottom left.
goal(State) :-
    transpose(State, Columns),
    % check if all columns are correct
    enumerate(Columns, 0, IndexedColumns),
    maplist(is_column_correct, IndexedColumns),
    % check if the gap is at the bottom left
    last(State, [gap|_]).

%% enumerate(+List, +Index, -EnumeratedList)
enumerate([], _, []).
enumerate([H|T], Index, [[Index, H]|EnumT]) :-
    NextIndex is Index + 1,
    enumerate(T, NextIndex, EnumT).

%% is_column_correct(+ColumnIndexList)
is_column_correct([ColumnIndex, Column]) :-
    exclude(==(gap), Column, Balls),
    % Check if all balls have the correct color for this column
    expected_color(ColumnIndex, ExpectedColor),
    maplist(has_color(ExpectedColor), Balls),
    % Check that the shades are in increasing order
    findall(Shade, member(ball(_, Shade), Balls), Shades),
    is_shade_increasing(Shades).

%% expected_color(+ColumnIndex, -Color)
expected_color(ColumnIndex, Color) :-
    % assume the columns are indexed from 0 to 25 or 'A' to 'Z'
    Code is ColumnIndex + 65,
    char_code(Color, Code).

%% has_color(+Color, +Ball)
has_color(C, ball(C, _)).

%% is_shade_increasing(+Shades)
is_shade_increasing([]).
is_shade_increasing([_]).
is_shade_increasing([H1,H2|T]) :-
    H1 < H2,
    is_shade_increasing([H2|T]).

%% solve_a_star_basic(+InitialState, -Solution)
% Try to solve the Babylon Tower using A*.
solve_a_star_basic(State, Solution) :-
    a_star_basic([node(State, 0, 0, [])], [], Solution).

%% solve_a_star_dynamic(+InitialState, -Solution)
% Try to solve the Babylon Tower using dynamic A*.
solve_a_star_dynamic(State, Solution) :-
    a_star_dynamic([node(State, 0, 0, [])], Solution).

%% solve_ida_star(+InitialState, -Solution)
% Try to solve the Babylon Tower using IDA*.
solve_ida_star(State, Solution) :-
    ida_star([node(State, 0, 0, [])], Solution).

%% a_star_basic(+Open, +Closed, -Solution)
a_star_basic([], _, _) :-
    writeln('No solution found!'),
    fail.

a_star_basic([node(State, G, H, Path)|Open], Closed, Solution) :-
    (
        % check if the current state is a goal state
        goal(State) ->
        reverse([State|Path], Solution), !
    ;
        % otherwise, generate the next states
        findall(Next, move(State, Next), NextStates),
        % calculate the heuristic for each next state
        maplist(heuristic, NextStates, Hs),
        G1 is G + 1,
        % calculate the f-value for each next state
        maplist(f_value(G1), Hs, FValues),
        % pair each state with its f-value
        pairs_keys_values(Pairs, FValues, NextStates),
        % filter out states already in the closed list with a higher f-value
        exclude(worse_than_closed(Closed), Pairs, FilteredPairs1),
        % filter out states already in the open list with a higher f-value
        exclude(worse_than_open(Open), FilteredPairs1, FilteredPairs),
        % create new nodes
        create_nodes(FilteredPairs, G1, [State | Path], NewNodes),
        % add the new nodes to the open list
        append(Open, NewNodes, UnsortedOpen),
        % sort the open list by f-value
        sort_nodes(UnsortedOpen, SortedOpen),
        % recursively call a_star with the new open and closed lists
        a_star_basic(SortedOpen, [node(State, G, H, Path)|Closed], Solution)
    ).

%% a_star_dynamic(+Open, -Solution)
a_star_dynamic([], _) :-
    writeln('No solution found!'),
    fail.

a_star_dynamic([node(State, G, _, Path)|Open], Solution) :-
    (
        % check if the current state is a goal state
        goal(State) ->
        % if so, reverse the path and clear the visited database
        reverse([State|Path], Solution),
        retractall(visited(_, _))
    ;
        % add the current state to the visited database
        assertz(visited(State, G)),
        % generate next states and filter out based on database
        findall(Next, (move(State, Next), \+ (visited(Next, PrevG), PrevG =< G+1)), NextStates),
        % calculate the heuristic for each next state
        maplist(heuristic, NextStates, Hs),
        G1 is G + 1,
        % calculate the f-value for each next state
        maplist(f_value(G1), Hs, FValues),
        % pair each state with its f-value
        pairs_keys_values(Pairs, FValues, NextStates),
        % filter out states already in the open list with a higher f-value
        exclude(worse_than_open(Open), Pairs, FilteredPairs),
        % create new nodes
        create_nodes(FilteredPairs, G1, [State | Path], NewNodes),
        % add the new nodes to the open list
        append(Open, NewNodes, UnsortedOpen),
        % sort the open list by f-value
        sort_nodes(UnsortedOpen, SortedOpen),
        % recursively call a_star with the new open and closed lists
        a_star_dynamic(SortedOpen, Solution)
    ).

%% ida_star(+InitialState, -Solution)
ida_star([], _) :-
    writeln('No solution found!'),
    fail.

ida_star([node(State, _, _, _)|_], Solution) :-
    % calculate the initial heuristic
    heuristic(State, H),
    % run with the initial bound
    ida_star_recursive(State, H, Solution).

%% ida_star_recursive(+State, +Bound, -Solution)
ida_star_recursive(State, Bound, Solution) :-
    % write('Searching with bound: '), write(Bound), nl,  %%% UNCOMMENT TO SEE THE PROGRESS
    % try to find a solution with the current bound
    ida_star_search(State, 0, Bound, [], Solution, NewBound),
    (
        % found it, we can stop
        NewBound = found ->
        true
    ;
        % otherwise continue with the new bound
        (
            % new bound is infinity, we failed
            NewBound = infinity ->
            fail
        ;
            ida_star_recursive(State, NewBound, Solution)
        )
    ).

%% ida_star_search(+State, +G, +Bound, +Path, -Solution, -Result)
ida_star_search(State, G, Bound, Path, Solution, Result) :-
    heuristic(State, H),
    F is G + H,

    (
        % if f-value exceeds bound, it will be the new bound for the next iteration
        F > Bound ->
        Result = F
    ;
        (
            % check if the current state is a goal state
            goal(State) ->
            reverse([State|Path], Solution),
            Result = found
        ;
            % otherwise continue searching
            findall(Next, move(State, Next), NextStates),
            % filter out states already in the path (cant have better f-value)
            exclude(member_of_path(Path), NextStates, FilteredNextStates),
            G1 is G + 1,
            process_successors(FilteredNextStates, State, G1, Bound, Path, Solution, Result)
        )
    ).

%% member_of_path(+Path, +State)
member_of_path(Path, State) :-
    member(State, Path).

%% process_successors(+Successors, +State, +G, +Bound, +Path, -Solution, -Result)
process_successors([], _, _, _, _, _, infinity).
process_successors([Next|Rest], State, G, Bound, Path, Solution, Result) :-
    % try the first successor
    ida_star_search(Next, G, Bound, [State|Path], Solution, TmpResult),

    (
        % found a solution
        TmpResult = found ->
        Result = found
    ;
        % continue searching with the next successor
        process_successors(Rest, State, G, Bound, Path, Solution, RestResult),

        (
            % found a solution
            RestResult = found ->
            Result = found
        ;
            % update the bound
            min_bound(TmpResult, RestResult, Result)
        )
    ).

%% min_bound(+A, +B, -Min)
min_bound(infinity, B, B).
min_bound(A, infinity, A).
min_bound(A, B, A) :- A =< B.
min_bound(A, B, B) :- B < A.

%% sort_nodes(+Open, -Sorted)
sort_nodes(Open, Sorted) :-
    % sort based f-value
    map_list_to_pairs(node_f_value, Open, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

%% node_f_value(+Node, -F)
node_f_value(node(State, G, H, _), F-State) :-
    % F = G + H
    F is G + H.

%% create_nodes(+Pairs, +G, +Path, -Nodes)
create_nodes([], _, _, []).
create_nodes([F-State | Rest], G, Path, [node(State, G, H, Path) | Nodes]) :-
    % extract the heuristic value from the pair
    H is F - G,
    create_nodes(Rest, G, Path, Nodes).

%% f_value(+G, +H, -F)
f_value(G, H, F) :-
    F is G + H.

%% worse_than_closed(+Closed, +F-State)
worse_than_closed(Closed, F-State) :-
    member(node(State, ClosedG, ClosedH, _), Closed),
    ClosedF is ClosedG + ClosedH,
    F >= ClosedF.

%% worse_than_open(+Open, +F-State)
worse_than_open(Open, F-State) :-
    member(node(State, OpenG, OpenH, _), Open),
    OpenF is OpenG + OpenH,
    F >= OpenF.

%% heuristic(+State, -H)
% The heuristic is the sum of the distances of each ball to its correct position.
heuristic(State, H) :-
    length(State, NumRows),
    % calculate distance for each ball
    calculate_all_ball_distances(State, 0, NumRows, BallDistances),
    % sum the distances
    sum_list(BallDistances, H).

%% calculate_all_ball_distances(+State, +RowIdx, +NumRows, -AllDistances)
calculate_all_ball_distances(_, RowIdx, NumRows, []) :-
    RowIdx >= NumRows, !.
calculate_all_ball_distances(State, RowIdx, NumRows, AllDistances) :-
    nth0(RowIdx, State, Row),
    length(Row, NumCols),
    % calculate distances for each ball in the row
    calculate_row_distances(State, Row, RowIdx, 0, NumCols, RowDistances),
    NextRowIdx is RowIdx + 1,
    % go to the next row
    calculate_all_ball_distances(State, NextRowIdx, NumRows, RestDistances),
    append(RowDistances, RestDistances, AllDistances).

%% calculate_row_distances(+State, +Row, +RowIdx, +ColIdx, +NumCols, -Distances)
calculate_row_distances(_, _, _, ColIdx, NumCols, []) :-
    ColIdx >= NumCols, !.
calculate_row_distances(State, Row, RowIdx, ColIdx, NumCols, [Distance|RestDistances]) :-
    nth0(ColIdx, Row, Ball),
    ball_distance(State, Ball, RowIdx, ColIdx, Distance),
    NextColIdx is ColIdx + 1,
    calculate_row_distances(State, Row, RowIdx, NextColIdx, NumCols, RestDistances).

%% ball_distance(+State, +Ball, +RowIdx, +ColIdx, -Distance)
% Calculate the distance of a ball to its correct position.
ball_distance(State, Ball, RowIdx, ColIdx, Distance) :-
    (
        Ball = gap,
        % distance from the gap to the bottom left corner
        length(State, Height),
        Distance is (ColIdx) + (Height - 1 - RowIdx)
    ;
        Ball = ball(Color, Shade),
        % get the correct column for the color of the ball
        ball_color_map(Color, CorrectColIdx),
        % calculate the distance from the correct column
        ColDistance is abs(ColIdx - CorrectColIdx),
        % do the same for the shade
        RowDistance is abs(RowIdx - (Shade - 1)),
        % sum the distances
        Distance is ColDistance + RowDistance
    ).

%% ball_color_map(+Color, -Index)
% Map the color of the ball to its index in the column.
ball_color_map(C, Index) :-
    char_code(C, Code),
    % just a sanity check for 'A' - 'Z' range
    Code >= 65,
    Code =< 90,
    % -1 to convert to 0-based index
    Index is Code - 64 - 1.

%% move(+State, -Next)
move(State, Next) :-
    % find the gap and generate moves - rotate any ring or move the gap one step down or up if possible
    find_gap(State, GapRow, GapCol),
    (   move_gap_down(State, GapRow, GapCol, Next)
    ;   move_gap_up(State, GapRow, GapCol, Next)
    ;   rotate_any_ring(State, Next)
    ).

%% find_gap(+State, -Row, -Col)
find_gap(State, Row, Col) :-
    nth0(Row, State, RowList),
    nth0(Col, RowList, gap).

%% move_gap_down(+State, +Row, +Col, -Next)
move_gap_down(State, Row, Col, Next) :-
    % find the length of the column
    length(State, Height),
    % check if the gap is not at the bottom
    Row < Height - 1,
    % get the ball below the gap
    Row1 is Row + 1,
    nth0(Row1, State, RowList),
    nth0(Col, RowList, Ball),
    % create a new state with the gap moved down
    replace_element(State, Row, Col, Ball, TempState),
    replace_element(TempState, Row1, Col, gap, Next).

%% move_gap_up(+State, +Row, +Col, -Next)
move_gap_up(State, Row, Col, Next) :-
    % check if the gap is not at the top
    Row > 0,
    % get the ball above the gap
    Row1 is Row - 1,
    nth0(Row1, State, RowList),
    nth0(Col, RowList, Ball),
    % create a new state with the gap moved up
    replace_element(State, Row, Col, Ball, TempState),
    replace_element(TempState, Row1, Col, gap, Next).

%% rotate_any_ring(+State, -Next)
rotate_any_ring(State, Next) :-
    length(State, N),
    N1 is N - 1,
    between(0, N1, RowIndex),
    (   rotate_left(State, RowIndex, Next)
    ;
        rotate_right(State, RowIndex, Next)
    ).

%% rotate_left(+State, +RowIndex, -NextState)
rotate_left(State, RowIndex, NextState) :-
    nth0(RowIndex, State, Row),

    % just append the first element to the end of the list
    Row = [First | Rest],
    append(Rest, [First], RotatedRow),
    replace_row(State, RowIndex, RotatedRow, NextState).

%% rotate_right(+State, +RowIndex, -NextState)
rotate_right(State, RowIndex, NextState) :-
    nth0(RowIndex, State, Row),

    % get the last element and prepend it to the front of the list
    reverse(Row, [Last | RevRest]),
    reverse(RevRest, Rest),
    RotatedRow = [Last | Rest],
    replace_row(State, RowIndex, RotatedRow, NextState).

%% replace_element(+State, +RowIndex, +ColIndex, +NewValue, -NewState)
% Replace an element in the State at a specific Row and Column index.
replace_element(State, RowIndex, ColIndex, NewValue, NewState) :-
    nth0(RowIndex, State, OldRow),
    replace_in_row(OldRow, ColIndex, NewValue, NewRow),
    replace_row(State, RowIndex, NewRow, NewState).

%% replace_in_row(+Row, +Index, +NewValue, -NewRow)
% Replace an element in a Row at a specific Index.
replace_in_row([_|Rest], 0, NewValue, [NewValue|Rest]) :- !.
replace_in_row([H|Rest], Index, NewValue, [H|NewRest]) :-
    Index > 0,
    NewIndex is Index - 1,
    replace_in_row(Rest, NewIndex, NewValue, NewRest).

%% replace_row(+State, +RowIndex, +NewRow, -NewState)
% Replace an entire row in the State.
replace_row([_|Rest], 0, NewRow, [NewRow|Rest]) :- !.
replace_row([H|Rest], Index, NewRow, [H|NewRest]) :-
    Index > 0,
    NewIndex is Index - 1,
    replace_row(Rest, NewIndex, NewRow, NewRest).

main :-
    % read and parse the input
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL,S),
    parse(S, Parsed),

    % use A* to try to solve the Babylon Tower
    % solve_a_star_basic(Parsed, Solution),     %%% UNCOMMENT TO USE STANDARD A*
    solve_a_star_dynamic(Parsed, Solution),     %%% UNCOMMENT TO USE DYNAMIC A*
    % solve_ida_star(Parsed, Solution),         %%% UNCOMMENT TO USE IDA*
    print_solution(Solution),
    halt.
