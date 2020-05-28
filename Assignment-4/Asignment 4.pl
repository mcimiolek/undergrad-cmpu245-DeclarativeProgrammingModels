%% =================================
%% CMPU-245, Fall 2018
%% Asmt. 4
%% =================================
%% Electronic Submission Due: Wed, Dec. 12 at 11:59 p.m.
%% Printouts can be left in the bin outside my door.

%% For this assignment, you should provide Prolog facts and
%% rules for the following predicates. To facilitate your
%% work, some facts and rules have been provided for you.

%% NOTE: Your code should be nicely formatted, with adequate
%%       comments. Long lines should NOT wrap around!!



%% 1) Prerequisites for CMPU courses at Vassar
%% -----------------------------------------------
%% prereqs(+C,Pre)
%% ------------------------------------------
%% holds if Pre is the list of pre-requisite courses for the course C
%
%% Note: The desired info is available from AskBanner.
%%       For this asmt, we only care about the pre-reqs that are CS
%%       courses. Don't worry about "recommended" courses, Math courses,
%%       etc.

%% Sample facts/rules:
%%    prereqs(cs101,[]).
%%    prereqs(cs145,[cs101]).

%% The prereqs for all of the courses listed in the Course catalog
prereqs(cs101,[]).
prereqs(cs102,[cs101]).
prereqs(cs145,[cs101]).
prereqs(cs203,[cs102]).
prereqs(cs224,[cs102,cs145]).
prereqs(cs235,[cs102,cs145]).
prereqs(cs240,[cs102,cs145]).
prereqs(cs241,[cs102,cs145]).
prereqs(cs245,[cs102,cs145]).
prereqs(cs250,[cs102]).
prereqs(cs290,[]).
prereqs(cs295,[]).
prereqs(cs298,[]).
prereqs(cs300,[]).
prereqs(cs301,[cs300]).
prereqs(cs324,[cs224]).
prereqs(cs325,[cs224]).
prereqs(cs331,[cs224,cs240]).
prereqs(cs334,[cs203,cs224]).
prereqs(cs353,[cs203]).
prereqs(cs365,[cs145,cs203,cs245]).
prereqs(cs366,[]).
prereqs(cs375,[cs203]).
prereqs(cs376,[]).
prereqs(cs377,[cs203,cs224]).
prereqs(cs378,[cs203]).
prereqs(cs379,[]).
prereqs(cs395,[cs241]).
prereqs(cs399,[]).






%% offered(+C,+Sem) <=== facts and rules provided for you below
%% -----------------------------------------------------------------
%% holds if the course C is offered in the semester Sem
%% -----------------------------------------------------------------
%% Define in terms of the following helper predicates:
%% offeredList(C,Sems) -- holds if C is offered during the listed semseters
%% Example: offeredList(cs245,[2,3,7]).

%% Courses that are offered every semester.

offered(cs101,_S).
offered(cs102,_S).
offered(cs145,_S).
offered(cs203,_S).
offered(cs224,_S).
offered(cs240,_S).
offered(cs241,_S).

%% Courses that are offered *sometimes*

%% Note: Facts of the form, offeredList(C,Sems) are used to represent
%% that the course C is offered during the semesters listed in
%% Sems. See examples below.

offered(C,Sem) :-
    offeredList(C,Sems),
    member(Sem,Sems).

%% Note: The following course-offering info is not intended to accurately
%% reflect actual course offerings at Vassar, but it is close enough
%% for our purposes.

%% 200 electives
offeredList(cs235,[1,4]).
offeredList(cs245,[2,3,7]).
offeredList(cs250,[5,6,8]).

%% 300 required
offeredList(cs331,[2,4,6,8]).
offeredList(cs334,[1,3,5,7]).

%% 300 electives
offeredList(cs324,[4,8]).
offeredList(cs353,[2,6]).
offeredList(cs366,[2,6]).
offeredList(cs365,[4,8]).
offeredList(cs375,[1,5]).
offeredList(cs376,[1,5]).
offeredList(cs377,[3,7]).
offeredList(cs378,[1,5]).
offeredList(cs379,[3,7]).






%% 2) canTake
%% -----------------------------------------------
%% canTake(+C,+PrevCourses,+Sem)
%% -----------------------------------------------
%% holds if the course C is offered during the semester Sem, and
%% PrevCourses contains all the prerequisites for C.

%% Note: If C is a 300-level course, it can only be taken if
%%       PrevCourses includes at least two 200-level courses.

%% To help with this latter requirement, define facts/rules for the
%% following helper predicates:
%%     2a. lowerLevel(C) -- holds if C is a 100-level or 200-level
%%         course
%%     2b. midLevel(C) -- holds if C is a 200-level course
%%     2c. atLeastNTwos(N,PrevCourses) -- holds if PrevCourses contains
%%         at least N 200-level courses.

%% Then the additional requirement for taking a 300-level course can be
%% expressed as:
%%     "if C is a 300-level course, then PrevCourses must contain at
%%      least two 200-level courses"
%% which is equivalent to:
%%     "either C is a lower-level course (100-level or 200-level) *OR*
%%      PrevCourses must contain at least 2 200-level courses"

%%(Hint: How is "disjunction" (i.e., OR) represented in Prolog syntax?)

%% Facts stating every 100 and 200 level class is lowerlevel
lowerLevel(cs101).
lowerLevel(cs102).
lowerLevel(cs145).
lowerLevel(cs203).
lowerLevel(cs224).
lowerLevel(cs235).
lowerLevel(cs240).
lowerLevel(cs241).
lowerLevel(cs245).
lowerLevel(cs250).
lowerLevel(cs290).
lowerLevel(cs295).
lowerLevel(cs298).

%% Facts stating every 200 level class is midlevel
midLevel(cs203).
midLevel(cs224).
midLevel(cs235).
midLevel(cs240).
midLevel(cs241).
midLevel(cs245).
midLevel(cs250).
midLevel(cs290).
midLevel(cs295).
midLevel(cs298).

%% Checks that a list of previous courses, PrevCourses has at least N 200 
%% level classes in it


%% Creates a wrapper class which adds a accumulator to the original arguments
atLeastNTwos(N, PrevCourses) :-
    atLeastNTwos(N, PrevCourses, 0).

%% Checks if N is less than or equal to Accumulator, and outputs the boolean
%% result
atLeastNTwos(N, _, Acc) :-
    N =< Acc.

%% When head is a 200 level class, incremment the accumulator and then check
%% atLeastNTwos with the tail of PrevCourses
atLeastNTwos(N, [H|T], Acc) :-
    midLevel(H),
    atLeastNTwos(N, T, (Acc + 1)).

%% When head isn't a 200 level class, check atLeastNTwos with the tail
%% of PrevCourses
atLeastNTwos(N, [_|T], Acc) :-
    atLeastNTwos(N, T, Acc).


%% Checks if a class C can be taken during a given semester Sem. In order to do so
%% it checks if C is offered in Sem, the prereqs of C are a subset of PrevCourses, the
%% list of previous courses taken, which indicates that the prereqs for C have
%% been met, and if it is a lowerLevel class or not, indicating it is a 300 level class,
%% in which case all of the previous are checked, but then it is checked if there
%% are at least two 200 classes taken as the other requirement for a 300 level class
%% using atLeastNTwos.
canTake(C, PrevCourses, Sem) :-
    offered(C,Sem),
    prereqs(C,X),
    subset(X,PrevCourses),
    lowerLevel(C);
    offered(C,Sem),
    prereqs(C,Y),
    subset(Y,PrevCourses),
    atLeastNTwos(2,PrevCourses).






%% 3) selectCourse
%
%% ---------------------------------------------------------------
%% selectCourse(-C, +PrevCourses, +CoursesLeft, -RemCourses, +Sem)
%% --------------------------------------------------------------
%% holds if C is a course that can be taken during semester Sem,
%% given that the courses listed in PrevCourses have already been
%% taken. C must be drawn from CoursesLeft. RemCourses is what
%% remains after removing C from CoursesLeft. (See "select" from the
%% map-coloring code.)


%% Wrapper that adds a version of courses left that can be destroyed
selectCourse(C, PrevCourses, CoursesLeft, RemCourses, Sem) :-
    selectCourse(C, PrevCourses, CoursesLeft, RemCourses, Sem, CoursesLeft).



%% Returns false if the version of CoursesLeft to be destroyed has
%% become empty
selectCourse(_C,_PreCourses,_CoursesLeft,_RemCourses,_Sem,[]) :-
    false.

%% Checks if the head of the CoursesLeft being destroyed can be
%% taken, sets C to that value, and then sets RemCourses to
%% CoursesLeft without it.
selectCourse(C, PrevCourses, CoursesLeft, RemCourses, Sem, [H|T]) :-
    canTake(H, PrevCourses, Sem),
    member(C,[H|T]),
    delete(CoursesLeft, C, RemCourses).

%% If the head cannot be used, looks in the tail for an course
%% that can be selected
selectCourse(C, PrevCourses, CoursesLeft, RemCourses, Sem, [_|T]) :-
    selectCourse(C, PrevCourses, CoursesLeft, RemCourses, Sem, T).






%% 4) incrementally fleshing out a full schedule
%% ---------------------------------------------------------------
%% fillSched(+PrevSched, +PrevCourses, +CurrCourses, +MaxPer, +CoursesLeft,
%%           +Sem, -FullSched)
%% --------------------------------------------------------------------
%% holds if the current partial schedule can be fleshed out into a
%% full schedule that satisfies the C.S. major.
%% --------------------------------------------------------------------
%% PrevSched is a list containing info about the courses taken in
%% each of the prior semesters. Each element of PrevSched has the
%% form N/Cs, where N is the number of the semester (1 <= N <= 8),
%% and Cs is the list of courses taken in that semester
%% (e.g., [cs102,cs145]));
%% PrevCourses is a *flat* list of *all* the C.S. courses from *all*
%% of the semesters included in PrevSched (e.g., [cs101,cs102,cs145]);
%% CurrCourses are the C.S. courses that are being scheduled (so far)
%% for the current semester (Sem);
%% MaxPer is an upper bound on the number of C.S. courses that can be
%% taken during any one semester (e.g., MaxPer might be 3);
%% CoursesLeft are the C.S. courses that the student has not yet
%% scheduled that are needed to complete the major;
%% Sem is the semester number (1-8); and
%% FullSched is the resulting full schedule (typically not known until
%% until the very end).

%% Note: A partial schedule can be incrementally fleshed out
%% in the following ways:
%% (1) If possible, select a course to add to CurrCourses for this
%%     semester. (Recursive goal will stay in this semester.)
%% (2) Move to the next semester. (Recursive goal will involve
%%     appending Sem/CurrCourses to the PrevSched.)
%% (3) The current semester is 9, indicating that semesters 1-8 are
%%     included in PrevSched, which means that you're done.
%% ---------------------------------------------------------------------
%% (1) is not possible if you've already scheduled MaxPer courses for
%%     the current semester, or there's no course that you're able
%%      to add to the current semester because of pre-reqs.
%% (2) is not possible if the number of courses left is too big to
%%     fit into the remaining semesters. (For example, if there
%%     are 8 courses left, but only 2 semesters left, and MaxPer = 3,
%%     then there's not enough room left to schedule the remaining
%%     courses. Including this constraint will make the search for
%%     a solution faster.)
%% (3) is not allowed if CoursesLeft is not empty, among other things.
%%      Here's case (3), done for you!


%% Works with case 1. It checks that the current semester, Sem, is less than
%% 9 indicating it is a valid semester for a class, makes sure that the length
%% of of CurrCourses, the current courses to be taken that semester, does not
%% exceed the amount of classes that can be taken per semester, uses selectCourse
%% to check if a course can be used in the schedule with the given information,
%% and updates CurrCourses to include the selected course. It then continues
%% attempting to fill the schedule using fillSched
fillSched(PrevSched, PrevCourses, CurrCourses, MaxPer, CoursesLeft, Sem, FullSched) :-
    Sem < 9,
    length(CurrCourses,Temp),
    MaxPer > Temp,
    selectCourse(X, PrevCourses, CoursesLeft, Y, Sem),
    UpdatedCurCourses = [X|CurrCourses],
    fillSched(PrevSched, PrevCourses, UpdatedCurCourses, MaxPer, Y, Sem, FullSched).

%% Works with case 2. It checks that the current semester, Sem, is less than 9
%% indicating the semester should still be incremented to see if classes can fit in
%% the next semester, checks that the classes to be taken do not exceed the amount
%% possible remaining by calculating the remaing class spots available and seeing
%% if the length of Courses is greater than that, and finally increments the semester.
%% It then continues attempting to full the schedule using fillSched. It also updates
%% FullSched
fillSched(PrevSched, PrevCourses, CurrCourses, MaxPer, CoursesLeft, Sem, FullSched) :-
    Sem < 9,
    length(CoursesLeft,X),
    X @=< (MaxPer * (8-Sem)),
    append(CurrCourses,PrevCourses,L),
    NewSem is Sem + 1,
    N = [Sem/CurrCourses],
    append(PrevSched,N,UpdatedPrevSched),
    fillSched(UpdatedPrevSched, L, [], MaxPer, CoursesLeft, NewSem, FullSched).

%% Works with case 3, and was provided.
fillSched(PrevSched, _PrevCourses, [], _MaxPer, [], 9, FullSched) :-
%% Since accumulating schedule in a list, it comes out backwards;
%% so, need to reverse it
    FullSched = PrevSched,
%% Hey, let's print it out nicely! (printSemester defined below)
    maplist(printSemester,FullSched).

%% ---------------------------------------------------------------
%% Some additional facts/rules to help you out.
%% ---------------------------------------------------------------

%% printSemester(+Sched)
%% ---------------------------------------------------------------
%% always succeeds. causes the Sched for *one* semester to be
%% printed out nicely. assumes Sched has the form: N/Courses.
%% For example, Sched might be: 1/[cs101]. Another example:
%% 2/[cs102,cs145].

printSemester(N/Courses) :-
format('Semester ~w: ~w \n', [N,Courses]).

%% "Wrapper predicate" to facilitate testing:

%% makeSched(+MaxPer,+TwoElec,+ThrElecA,+ThrElecB,-FullSched)
%% ---------------------------------------------------------------
%% holds if there is a FullSched of courses (over 8 semesters)
%% that satisfies the C.S. major requirements, as follows:
%% MaxPer = max number of CS courses in any single semester
%% TwoElec = one of cs235, cs245, cs250
%% ThrElecA = one of the 300-level electives
%% ThrElecB = another of the 300-level electives

makeSched(MaxPer,TwoElec,ThrElecA,ThrElecB,FullSched) :-
    member(TwoElec,[cs235,cs245,cs250]),
    subset([ThrElecA,ThrElecB],[cs324,cs325,cs353,cs365,cs366,cs375,cs376,
            cs377,cs378,cs379]),
%% We assemble the list of courses as follows to make the search
%% go faster. Note that the courses need not be scheduled in this
%% order, but the actual order will probably not deviate too much
%% from this
    ListyOne = [cs101,cs102,cs145,cs203,cs224,cs240,cs241],
    ListyTwo = [TwoElec,cs331,cs334,ThrElecA,ThrElecB],
    append(ListyOne,ListyTwo,Listy),
%% Here's the call to "fillSched" with suitably initialized arguments
%% Notice that PrevSched = PrevCourses = CurrCourses = []
%% Listy = the full list of courses needed to satisfy the major
%% FullSched won't be instantiated until the entire goal succeeds.
    fillSched([],[],[],MaxPer,Listy,1,FullSched).

%% Here's an example:

%% -?- makeSched(3,cs245,cs365,cs377,FullSched).
%% Semester 1: [cs101] <--- These 8 lines are side-effect printing
%% Semester 2: [cs145,cs102]
%% Semester 3: [cs240,cs224,cs203]
%% Semester 4: [cs331,cs241]
%% Semester 5: [cs334]
%% Semester 6: []
%% Semester 7: [cs377,cs245]
%% Semester 8: [cs365]
%% FullSched = [1/[cs101], 2/[cs145, cs102], 3/[cs240, cs224, cs203],
%%              4/[cs331, cs241], 5/[cs334], 6/[], 7/[cs377|...],
%%              8/[...]] .

%% The last line "FullSched = ..." is "the answer". See why we used
%% "maplist" and "printSemester"!






%% =======================================================
%%  TESTING WILL BE A SIGNIFICANT PART OF YOUR GRADE!
%% =======================================================
%%  You should include MANY separate tests for EACH of the following predicates
%%  to demonstrate that they work properly when tested in isolation:
%%     prereqs, canTake, selectCourse, fillSched.
%%  For "fillSched", create some tests that start with a full (or nearly
%%  full) schedule.  For example, you might try defining a TESTER predicate
%%  like this:

tester(0,F) :-
  PrevSched = [1/[cs101],2/[cs145,cs102],3/[cs224,cs203],4/[cs235,cs240],
               5/[cs376,cs241],6/[cs366,cs331],7/[cs334],8/[]],
  PrevCourses = [cs101,cs102,cs145,cs203,cs224,cs240,cs241,cs235,cs331,cs334,cs366,cs376],
  fillSched(PrevSched, PrevCourses, [], 2, [], 9, F).

tester(1,F) :-
  CoursesLeft = [cs101,cs145,cs102,cs224,cs203,cs235,cs240,
               cs376,cs241,cs366,cs331,cs334],
  fillSched([], [], [], 2, CoursesLeft, 1, F).

tester(2,F) :-
  PrevSched = [1/[cs101],2/[cs145,cs102],3/[cs224,cs203]],
  PrevCourses = [cs101,cs102,cs145,cs203,cs224],
  CoursesLeft = [cs235,cs240,cs376,cs241,cs366,cs331,cs334],
  fillSched(PrevSched, PrevCourses, [], 3, CoursesLeft, 4, F).

tester(3,F) :-
  PrevSched = [1/[cs101],2/[cs102],3/[cs203]],
  PrevCourses = [cs101,cs102,cs203],
  CoursesLeft = [cs145,cs224,cs240,cs241,cs235,cs331,cs334,cs366,cs376],
  fillSched(PrevSched, PrevCourses, [], 2, CoursesLeft, 4, F).

tester(4,F) :-
  PrevSched = [1/[cs101],2/[cs145,cs102],3/[cs224,cs203],4/[cs235,cs240],
               5/[cs376,cs241],6/[cs366,cs331],7/[],8/[]],
  PrevCourses = [cs101,cs102,cs145,cs203,cs224,cs240,cs241,cs235,cs331,cs366,cs376],
  CoursesLeft = [cs334],
  fillSched(PrevSched, PrevCourses, [], 2, CoursesLeft, 9, F).

tester(5,F) :-
  PrevSched = [1/[],2/[cs145,cs102],3/[cs224,cs203],4/[cs235,cs240],
               5/[cs376,cs241],6/[cs366,cs331],7/[],8/[]],
  PrevCourses = [cs101,cs102,cs145,cs203,cs224,cs240,cs241,cs235,cs331,cs366,cs376],
  CoursesLeft = [cs334,cs101,cs102],
  fillSched(PrevSched, PrevCourses, [], 2, CoursesLeft, 8, F).

tester(6,F) :-
  PrevSched = [1/[],2/[cs145,cs102],3/[cs224,cs203],4/[cs235,cs240],
               5/[cs376,cs241],6/[cs366,cs331],7/[cs334],8/[]],
  PrevCourses = [cs101,cs102,cs145,cs203,cs224,cs240,cs241,cs235,cs331,cs366,cs376,cs334],
  CoursesLeft = [cs101,cs102,sc145],
  fillSched(PrevSched, PrevCourses, [], 2, CoursesLeft, 8, F).

tester(7,F) :-
  CoursesLeft = [cs241,cs376,cs145,cs102,cs101,cs334,cs224,cs235,cs240,
               cs366,cs331,cs203],
  fillSched([], [], [], 2, CoursesLeft, 1, F).

%%  Other test cases can be constructed similarly (using a first argument that
%%  is a different number).  That way, at the command line, you can just type:
%%  tester(0,F) instead of the whole big mess above.

tester(_N,F) :-
  F = "False",
  write('This test returns not fillSched \n').

%%  You should define MANY such test expressions for fillSched.

%%  When everything appears to be working, *then* you should define SEVERAL
%%  MORE test expressions for makeSched.

%%  As described in asmt8-template.txt, you should define an "output"
%%  predicate that generates all of your test results in a nicely formatted
%%  manner.  And you should generate an output file that you will include
%%  in your electronic submission, and that you will print out.  See
%%  "asmt8-template.txt" for more details.

%%  HINTS:  If testPrereqs takes two arguments (e.g., cs101 and Cs)
%%          you can use:  maplist(testPrereqs,[cs101,cs353,cs366],_Listy)
%%    Listy represents the list of answers (each element being a list of
%%    prereqs for one of the given courses).

%%    Run "output." at the prolog query to test your "output" predicate.
%%    You don't have to create an output file every time you test; only
%%    at the end, when you're done.







%% ==============================
%%  TEST PREDICATES  --  provided by your helpful professor!
%% ==============================

%%  testPrereqs(+C,Cs)
%% --------------------------

testPrereqs(C,Cs) :-
  prereqs(C,Cs),
  format('  prereqs(~w,~w)\n',[C,Cs]).

testPrereqs(C,Cs) :-
  \+ prereqs(C,Cs),
  format('  not prereqs(~w,~w)\n',[C,Cs]).


%%  testCanTake(+C,+Cs,+Sem)
%% ----------------------------

testCanTake(C,Cs,Sem) :-
  canTake(C,Cs,Sem),
  format('  canTake(~w,~w,~w)\n',[C,Cs,Sem]).

testCanTake(C,Cs,Sem) :-
  \+ canTake(C,Cs,Sem),
  format('  NOT canTake(~w,~w,~w)\n',[C,Cs,Sem]).

%%  testSelectCourse(-C, +PrevCourses, +CoursesLeft, -RemCourses, +Sem)
%% ----------------------------------------------------------------------

testSelectCourse(C,PrevCourses,CoursesLeft,RemCourses,Sem) :-
  selectCourse(C,PrevCourses,CoursesLeft,RemCourses,Sem),
  format('  selectCourse(~w,~w,~w,~w,~w)\n',[C,PrevCourses,CoursesLeft,RemCourses,Sem]).

testSelectCourse(C,PrevCourses,CoursesLeft,RemCourses,Sem) :-
  \+ selectCourse(C,PrevCourses,CoursesLeft,RemCourses,Sem),
  format('  NOT selectCourse(N/A,~w,~w,~w,N/A)\n',[C,PrevCourses,CoursesLeft,RemCourses,Sem]).

%%  testFillSched
%% ------------------------------

testFillSched(N) :-
  format('Test ~w: \n', N),
  tester(N,F),
  write(F),
  write('\n'),
  write('\n').

%%  testMakeSched(+MaxPer,+TwoElec,+ThrElecA,+ThrElecB,-FullSched)
%% ----------------------------

testMakeSched(MaxPer,TwoElec,ThrElecA,ThrElecB,FullSched) :-
  makeSched(MaxPer,TwoElec,ThrElecA,ThrElecB,FullSched),
  format('  makeSched(~w,~w,~w,~w,~w)\n',[MaxPer,TwoElec,ThrElecA,ThrElecB,FullSched]),
  write('\n').

testMakeSched(MaxPer,TwoElec,ThrElecA,ThrElecB,FullSched) :-
  \+ makeSched(MaxPer,TwoElec,ThrElecA,ThrElecB,FullSched),
  format('  NOT makeSched(~w,~w,~w,~w,N/A)\n',[MaxPer,TwoElec,ThrElecA,ThrElecB,FullSched]),
  write('\n').

%%  output
%% -----------------------------------------------------
%%  a proposition that always succeeds.  used to perform tests
%%  and print out results.

output :-
  write('\n-----------------------\n'),
  write(' CMPU-245, Fall 2018\n'),
  write(' Asmt. 4 Matthew Imiolek\n'),
  write('-----------------------\n\n'),

  write('PROBLEM ONE:  Testing prereqs:\n'),   %% This example uses maplist
  maplist(testPrereqs,[cs101,cs102,cs145,cs203,cs224,cs240,cs245,cs365,cs375,cs377],_List),
  %% False tests
  maplist(testPrereqs,[cs102,cs245,cs375],[[],[cs101,cs102],[cs240,cs145]]),

  write('\nPROBLEM TWO:  Testing canTake:\n'),
  maplist(testCanTake,[cs101,cs102,cs203,cs245,cs365,cs245,cs377,cs145,cs250,cs377],
                      [[],[cs101],[cs102],[cs102,cs145],[cs145,cs203,cs245],[cs101,cs102],[cs240,cs245],[cs240],[cs102],[cs203,cs224]],
                      [3,2,1,7,8,4,5,6,2,1]),                  %% <--- Add MORE TESTS!

  write('\nPROBLEM THREE:  Testing selectCourse:\n'), %% This example doesn't use maplist
    maplist(testSelectCourse, _C, [[],[cs101],[cs203,cs102,cs224],[cs203,cs102],[cs203,cs102,cs224]],
                              [[cs101,cs102,cs145],[cs102],
                              [cs240,cs377,cs102,cs203,cs145,cs224],[cs377,cs102,cs145,cs224],[cs377]],
                              _RemCOurses,[1,3,7,5,2]),
    %testSelectCourse(C,[],[cs101,cs102,cs145],RemCourses,1),
    %testSelectCourse(C,[cs101],[cs102],RemCourses,3),
    %testSelectCourse(C,[cs203,cs102,cs224],[cs240,cs377,cs102,cs203,cs145,cs224],RemCourses,7),

  %% Tests 3 and 7 may seem like they will not work, but after a few seconds they will
  write('\nPROBLEM FOUR:  Testing fillSched:\n'),  %% This example uses maplist
    maplist(testFillSched,[0,1,2,3,4,5,6,7]),   %% <--- Add MORE TESTS!!

  write('\nPROBLEM FIVE:  Testing MakeSched:\n'), %% ==> Include tests for makeSched too!!
  maplist(testMakeSched, [3,4,2,3,3,2,2],[cs245,cs235,cs245,cs250,cs235,cs250,cs331],
                         [cs365,cs353,cs375,cs234,cs365,cs224,cs378],[cs377,cs366,cs376,
                         cs235,cs203,cs353,cs379],_FullSched),

  %% The following ensures no attempts will be made to find more
  %% than one way of satisfying any of the above predications:
  !.
