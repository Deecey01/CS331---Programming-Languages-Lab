% Maze Solver - CS331 Assignment VI
% Advanced Prolog Programming

% Import maze data
:- [mazedata].

% Dynamic predicates for search algorithm
:- dynamic visited/1.
:- dynamic queue/1.
:- dynamic path/2.
:- dynamic faultynode/1.

% BFS algorithm to find shortest path
shortest_path(Start, End, Path) :-
    % Initialize search
    retractall(visited(_)),
    retractall(queue(_)),
    retractall(path(_, _)),
    
    % Start with initial node
    assert(visited(Start)),
    assert(queue([Start])),
    assert(path(Start, [Start])),
    
    % Run BFS until solution found or no path exists
    bfs_loop(End, PathFound),
    
    % Return the path or empty list if no path exists
    (PathFound = true -> 
        path(End, RevPath),
        reverse(RevPath, Path),
        format('Shortest path from ~w to ~w:~n', [Start, End]),
        print_path(Path),
        length(Path, PathLength),
        format('Path length: ~w~n', [PathLength])
    ;
        Path = [],
        format('No path found from ~w to ~w~n', [Start, End])
    ).

% Print the complete path with all nodes
print_path(Path) :-
    format('~w', [Path]),
    nl.

% BFS main loop
bfs_loop(End, Found) :-
    % Check if queue is empty (no solution)
    (queue([]) -> 
        Found = false
    ;
        % Get next node from queue
        queue([Current|Rest]),
        retract(queue([Current|Rest])),
        assert(queue(Rest)),
        
        % Check if destination reached
        (Current = End -> 
            Found = true
        ;
            % Process neighbors
            expand_node(Current),
            bfs_loop(End, Found)
        )
    ).

% Expand a node by adding its valid neighbors to the queue
expand_node(Node) :-
    % Get current path to this node
    path(Node, CurrentPath),
    
    % Find all valid neighbors
    findall(Neighbor, 
           (mazelink(Node, Neighbor), 
            \+ faultynode(Neighbor), 
            \+ visited(Neighbor)), 
           Neighbors),
    
    % Process each neighbor
    process_neighbors(Neighbors, CurrentPath).

% Process all neighbors of a node
process_neighbors([], _).
process_neighbors([Neighbor|Rest], CurrentPath) :-
    % Mark as visited
    assert(visited(Neighbor)),
    
    % Update queue
    queue(Q),
    retract(queue(Q)),
    append(Q, [Neighbor], NewQ),
    assert(queue(NewQ)),
    
    % Record path to this neighbor
    append(CurrentPath, [Neighbor], NewPath),
    assert(path(Neighbor, NewPath)),
    
    % Process remaining neighbors
    process_neighbors(Rest, CurrentPath).

% Dynamically add a faulty node
add_faulty_node(Node) :-
    (faultynode(Node) ->
        format('Node ~w is already marked as faulty~n', [Node])
    ;
        assert(faultynode(Node)),
        format('Added faulty node: ~w~n', [Node])
    ).

% Dynamically remove a faulty node
remove_faulty_node(Node) :-
    (retract(faultynode(Node)) ->
        format('Removed faulty node: ~w~n', [Node])
    ;
        format('Node ~w was not marked as faulty~n', [Node])
    ).

% Display the maze information
display_maze_info :-
    findall(Node, faultynode(Node), FaultyNodes),
    format('Faulty nodes: ~w~n', [FaultyNodes]),
    
    % Count total nodes in the maze
    findall(X, (mazelink(X, _); mazelink(_, X)), AllNodes),
    sort(AllNodes, UniqueNodes),
    length(UniqueNodes, NodeCount),
    format('Total nodes in maze: ~w~n', [NodeCount]),
    
    % Find maze dimensions
    max_list(UniqueNodes, MaxNode),
    format('Highest node ID: ~w~n', [MaxNode]).

% Helper predicate to visualize the path in 2D grid
visualize_path(Path, Width) :-
    format('Path visualization in 2D grid (Width = ~w):~n', [Width]),
    findall(Node, faultynode(Node), FaultyNodes),
    max_list([0|Path], MaxNode),
    Height is (MaxNode // Width) + 1,
    
    % Print grid
    print_grid(0, 0, Width, Height, Path, FaultyNodes).

% Print the grid with path visualization
print_grid(X, Y, Width, Height, _, _) :-
    Y >= Height, !.
print_grid(X, Y, Width, Height, Path, FaultyNodes) :-
    X >= Width, !,
    nl,
    NextY is Y + 1,
    print_grid(0, NextY, Width, Height, Path, FaultyNodes).
print_grid(X, Y, Width, Height, Path, FaultyNodes) :-
    Node is Y * Width + X,
    (member(Node, Path) ->
        % Show node number in path with leading zeros for alignment
        format('~|~`0t~d~3+', [Node])
    ; member(Node, FaultyNodes) ->
        write(' XX')
    ;
        write(' __')
    ),
    write(' '),
    NextX is X + 1,
    print_grid(NextX, Y, Width, Height, Path, FaultyNodes).

% Example usage in console:
% ?- display_maze_info.
% ?- shortest_path(0, 99, Path).
% ?- visualize_path(Path, 10).  % For 10x10 maze
% ?- add_faulty_node(12).
% ?- shortest_path(0, 99, NewPath).
% ?- remove_faulty_node(12).