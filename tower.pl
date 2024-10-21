% Supportive predicates for domain constraints, row and column length validations
% Apply domain constraints to each row in the grid
domain_constraints([], _). % Base case: no more rows to apply constraints to
domain_constraints([CurrentRow | RemainingRows], GridSize) :-
    fd_domain(CurrentRow, 1, GridSize), % Set the domain for the current row
    GridSize > 0, % Assuming grid size is always positive
    domain_constraints(RemainingRows, GridSize).

% No-op function for for introducing visual breakpoints(used during debugging)
noop(_).

% Determine the total number of rows in the grid
row_length(Grid, NumberOfRows) :-
    length(Grid, NumberOfRows),
    NumberOfRows >= 0.

% Verify that all columns in the grid have the correct length
col_length([], _). % Base case: no more rows to check
col_length([FirstRow | RemainingRows], DesiredLength) :-
    length(FirstRow, DesiredLength),
    col_length(RemainingRows, DesiredLength).

% Matrix manipulation predicates for transpose and visibility checks
transpose_matrix([], []).
transpose_matrix([F|Fs], Ts) :-
    transpose_matrix(F, [F|Fs], Ts).
transpose_matrix([], _, []).
transpose_matrix([_|Rs], Ms, [Ts|Tss]) :-
    decompose_lists(Ms, Ts, Ms1),
    transpose_matrix(Rs, Ms1, Tss).

decompose_lists([], [], []).
decompose_lists([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    decompose_lists(Rest, Fs, Oss).

% Visibility validation for normal and reverse direction
validate_visibility_normal([], []).
validate_visibility_normal([HeadRow | TailRows], [ExpectedVis | RemainingVis]) :-
    validate_row_visibility(HeadRow, ExpectedVis),
    validate_visibility_normal(TailRows, RemainingVis).

validate_visibility_reverse([], []).
validate_visibility_reverse([HeadRow | TailRows], [ExpectedVis | RemainingVis]) :-
    reverse(HeadRow, ReversedRow),
    validate_row_visibility(ReversedRow, ExpectedVis),
    validate_visibility_reverse(TailRows, RemainingVis).

% Counting visible towers in a row
count_visible_towers([], [], _).
count_visible_towers([Height | RestHeights], [Height | Visible], MaxSeen) :-
    Height #> MaxSeen,
    count_visible_towers(RestHeights, Visible, Height).
count_visible_towers([Height | RestHeights], Visible, MaxSeen) :-
    Height #=< MaxSeen,
    count_visible_towers(RestHeights, Visible, MaxSeen).

% Main ntower solution using finite domain solver from the older version
ntower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),
    length(T, N),
    maplist(length_(N), T),
    maplist(set_domain_for_towers(N), T),
    maplist(fd_all_different, T),
    transpose(T, Transposed),
    maplist(fd_all_different, Transposed),
    maplist(fd_labeling, T),
    verify_visibility(Transposed, Top),

    verify_visibility(T, Left),
    verify_visibility_reversed(T, Right),
    verify_visibility_reversed(Transposed, Bottom).

% Auxiliary predicates to support ntower logic
length_(N, List) :- length(List, N).
set_domain_for_towers(N, Row) :- fd_domain(Row, 1, N).

% Transpose the grid (reused from older version for consistency)
transpose([], []).
transpose([F|Fs], Ts) :- transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

% Verify visibility for ntower (reused from older version)
verify_visibility([], []).
verify_visibility([Row | Rest], [ExpectedCount | Counts]) :-
    visible_towers(Row, ExpectedCount),
    verify_visibility(Rest, Counts).

verify_visibility_reversed([], []).
verify_visibility_reversed([Row | Rest], [ExpectedCount | Counts]) :-
    reverse(Row, ReversedRow),
    visible_towers(ReversedRow, ExpectedCount),
    verify_visibility_reversed(Rest, Counts).

visible_towers(Row, VisibleCount) :-
    calculate_visible_towers(Row, VisibleTowers, 0),
    length(VisibleTowers, VisibleCount).

calculate_visible_towers([], [], _).
calculate_visible_towers([Height | Rest], [Height | Visible], MaxSeen) :-
    Height #> MaxSeen,
    calculate_visible_towers(Rest, Visible, Height).
calculate_visible_towers([Height | Rest], Visible, MaxSeen) :-
    Height #=< MaxSeen,
    calculate_visible_towers(Rest, Visible, MaxSeen).

% Plain ntower solution without finite domain solver (current working version continues)
plain_ntower(N, T, C) :-
    row_length(T, N),
    generate_domain_list(N, DomainList),
    counts(U,D,L,R) = C,
    validate_plain_visibility(N, T, L, R, DomainList),
    transpose_matrix(T, TransposedT),
    validate_plain_visibility(N, TransposedT, U, D, DomainList).

% Continue with the additional predicates for plain ntower solution, testing, performance comparison, and finding ambiguous puzzles from the current version

% Additional predicates for plain ntower solution
generate_domain_list(N, Domain) :- 
    findall(Num, between(1, N, Num), Domain).

ensure_unique_row([], []).
ensure_unique_row([Head | Tail], Domain) :-
    member(Head, Domain),
    exclude_element(Head, Domain, ReducedDomain),
    ensure_unique_row(Tail, ReducedDomain).

exclude_element(Element, [Element | Rest], Rest).
exclude_element(Element, [Other | Rest], [Other | ReducedRest]) :-
    exclude_element(Element, Rest, ReducedRest).

validate_plain_visibility(_, [], [], [], _).
validate_plain_visibility(Size, [Row | Rows], [LeftVis | LRest], [RightVis | RRest], Domain) :-
    length(Row, Size),
    ensure_unique_row(Row, Domain),
    validate_row_visibility_plain(Row, LeftVis),
    reverse(Row, ReversedRow),
    validate_row_visibility_plain(ReversedRow, RightVis),
    validate_plain_visibility(Size, Rows, LRest, RRest, Domain).

validate_row_visibility_plain(Row, VisibleCount) :- 
    count_visible_towers_plain(Row, 0, 0, VisibleCount).

count_visible_towers_plain([], Length, _, Length).
count_visible_towers_plain([Height | Rest], Length, MaxHeight, Result) :-
    Height > MaxHeight,
    UpdatedLength is Length + 1, 
    count_visible_towers_plain(Rest, UpdatedLength, Height, Result).
count_visible_towers_plain([Height | Rest], Length, MaxHeight, Result) :-
    Height =< MaxHeight,
    count_visible_towers_plain(Rest, Length, MaxHeight, Result).

% Testing and performance comparison
ntower_time_new(Time) :- 
    statistics(cpu_time, [Start|_]),
    ntower(4, _, counts([1,3,2,2],[2,2,1,3],[3,1,3,2],[2,3,2,1])),
    statistics(cpu_time, [Finish|_]),
    Time is (Finish - Start).

plain_ntower_time_new(Time) :- 
    statistics(cpu_time, [Start|_]),
    plain_ntower(4, _, counts([1,3,2,2],[2,2,1,3],[3,1,3,2],[2,3,2,1])),
    statistics(cpu_time, [Finish|_]),
    Time is (Finish - Start).

speedup(Result) :-
    ntower_time_new(NTime),
    plain_ntower_time_new(PTime),
    NTime > 0 -> Result is PTime / NTime; Result is 0.

% Finding ambiguous puzzles
ambiguous(N, C, T1, T2) :-
    ntower(N, T1, C),
    ntower(N, T2, C),
    T1 \= T2.
