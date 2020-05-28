%% Exercise 1
%% ----------
%% Write a predicate last(List,X) which is true only when List is a
%% list that contains at least one element and X is the last element
%% of that list.
%%
%% Hints:
%% 1) handle the case where List is empty first
%% 2) then use an accumulator to find the last element of List
%% 3) so last/2 is a wrapper for last/3

% An empty list is always false
last([],_) :-
    false.

% Wrapper which calls the 3-place predicate which adds an accumulator
% that will eventually hold the last element of List (expressed using _)
% to avoid a singleton error)
last(List,X) :-
    last(List,X,_).

% Checks if X is equal to End, which holds the last element of the list,
% once the list has become an empty list confirming End is the actual
% last element.
last([],X,End) :-
    X == End.

% Shortens the list, while setting the accumulator to its head, so list
% eventually becomes empty and the accumulator the last element.
last([H|T],X,_) :-
    last(T,X,H).






%% Exercise 2
%% ----------
%%  Let's call a list doubled if it is made of two consecutive blocks
%%  of elements that are exactly the same. For example, [a,b,c,a,b,c]
%%  is doubled (it's made up of [a,b,c] followed by [a,b,c] ) and so
%%  is [foo,gubble,foo,gubble]. On the other hand, [foo,gubble,foo]
%%  is not doubled. Write a predicate doubled(List) which succeeds when
%%  List is a doubled list.
%%
%%  Hints:
%%  1) you may find the prefix and suffix predicates helpful
%%  2) you may find the append predicate helpful

% Wrapper which calls the 2-place predicate which has two copies of
% List so that one can be shortened without getting rid of a whole
% version of List.
doubled(List) :-
    doubled(List,List).

% Checks if when ToDouble is appended to itself if it is the same as
% list, meaning the first and second half are the same, and therefore
% doubled is true
doubled(List,ToDouble) :-
    append(ToDouble,ToDouble,List).

% Shortens the second list, the ToDouble one.
doubled(List,[_|T]) :-
    doubled(List,T).





%% Exercise 3
%% ----------
%% A palindrome is a word or phrase that spells the same forwards and
%% backwards. For example, "rotator", "eve", and "nurses run" are all
%% palindromes. Write a predicate palindrome(List) , which checks
%% whether List is a palindrome. For example, the following queries
%% should be found to be true:
%%     palindrome([r,o,t,a,t,o,r]).
%%     palindrome([n,u,r,s,e,s,r,u,n]).
%% while the following query should be found to be false:
%%     palindrome([n,o,t,m,a,r,c]).
%%
%% Hints:
%% 1) you may find the reverse/2 function helpful
%% 2) palindrome(List) should be designed as a wrapper predicate

% Checks if the reverse of List is List, which would make it a
% palindrome
palindrome(List) :-
    reverse(List,List).






%% Exercise 4
%% ----------
%% Write a predicate set(InList,OutList) which takes as input an
%% arbitrary list, and returns a list in which each element of the
%% input list appears only once. Order doesn't matter, since OutList
%% is a set. For example, the query
%%     set([2,2,foo,1,foo, [],[]],X).
%% should yield the result
%%     X = [[], 1, foo, 2]
%%
%% Hints:
%% 1) use an accumulator
%% 2) look up the NOT predicate: \+
%% 3) you may find the member predicate helpful

% Wrapper which class 3-place predicate which adds [] to be used as an
% accumulator
set(InList, OutList) :-
    set(InList, [], OutList).

% if InList has become empty, OutList becomes the accumulator, Acc, and
% is returned
set([],Acc,Acc).

% If the head, H, is not a member of the tail, T, then it is added to
% the accumulator, Acc, as it has become a unique member of the InList.
% InList becomes just the tail, taking out the unique element so it
% can't be added again.
set([H|T],Acc, OutList) :-
    \+member(H,T),
    set(T,[H|Acc],OutList).

% If head, H, is a member of tail, T, then InList just becomes its tail,
% without adding H to the accumulator, Acc.
set([H|T],Acc,OutList) :-
    member(H,T),
    set(T,Acc,OutList).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Unit Tests
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(asmt4).

test(last) :-
        \+ last([], 42),      %% need NOT (\+) for false tests!
        last([42], 42),
        last([1,2,3], 3),
        last([a,b,v,d],d),
        last([a,1,b,2], 2),
        \+ last([a,b,c], d),
        \+ last([1,a,2,b], fg),
        \+ last([1,2,3], 42).

test(doubled, [nondet]) :-    %% need [nondet] when >1 alternatives
        doubled([]),
        doubled([a,b,c,a,b,c]),
        doubled([1,2,3,11,1,2,3,11]),
        doubled([1,a,12,gf,1,a,12,gf]),
        \+ doubled([1,2,1]),
        \+ doubled([1,1,1]),
        \+ doubled([a,b,c]).

test(palindrome) :-
        palindrome([]),
        palindrome([42]),
        palindrome([r,o,t,a,t,o,r]),
        palindrome([r,a,c,e,c,a,r]),
        palindrome([s,t,a,c,k,c,a,t,s]),
        palindrome([e,v,a,c,a,n,i,s,t,a,b,b,a,t,s,i,n,a,c,a,v,e]),
        \+ palindrome([icecream,ice,a,icecream]),
        \+ palindrome([v,a,s,s,a,r]).

test(set1, [nondet]) :-
        set([], X), X == [].

test(set2, [nondet]) :-
        set([42], X), X == [42].

test(set3, [nondet]) :-
        set([2,2,foo,1,foo, [],[]], X), X == [[], foo, 1, 2].

test(set4, [nondet]) :-
        set([1,1,2,2,3,3,4,4], X), X == [4,3,2,1].

test(set5, [nondet]) :-
        set([4,4,3,3,2,2,1,1], X), X == [1,2,3,4].

test(set5, [nondet]) :-
        set([1,1,2,3,4,3,2,11,1], X), X == [1,11,2,3,4].

test(set5, [nondet]) :-
        set([a,b,c,v,v,a,d,f,f,c,c], X), X == [c,f,d,a,v,b].

:- end_tests(asmt4).

%% To run your tests, type the following command at the swipl prompt:
%%     ?- run_tests.
