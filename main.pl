
:- dynamic person/9.    % person(Name, Birth, Death, Father, Mother, Children, Level, Gender, Spouse)
:- dynamic marriage/2.

% Main function
main :-
    menu,
    nl,
    write('Please choose an operation!'), nl,
    flush_output, % Send consol output
    read(Choice),
    choice(Choice),
    main. % Return back to menu

% Menu choices
menu :-
    write('1-) Ask relation'), nl,
    write('2-) Add/Update person'), nl,
    write('3-) Get information of any person'), nl,
    write('4-) Print the family tree'), nl,
    write('5-) Add marriage'), nl,
    write('6-) Terminate the program'), nl.

% Choices
choice(1) :- % Ask relation
    write('Please type first person name and surname: '),
    read(First),
    write('Please type second person name and surname: '),
    read(Second),
    ask_relation(First, Second).

choice(2) :- % Add/Update person
    write('1-) Add Person'), nl,
    write('2-) Update Person'), nl,
    write('Please choose an operation! '),
    read(Choice1),
    choice_2(Choice1).

% Add/Update person, subchoices of choice 2
choice_2(1) :- % Add person
    write('Please type the child name and surname: '), nl,
    read(Child),
   ask_valid_birth_date(Birthdate),
    ask_valid_death_date(Death),
    write('Please type the gender of child (m/f): '), nl,
    read(Gender),
    write('Please type the father name and surname: '), nl,
    read(Father),
    write('Please type the mother name and surname: '), nl,
    read(Mother),
    add_person(Child, Birthdate, Death, Father, Mother, Gender).

% Check for a valid death date
ask_valid_death_date(Death) :-
    write('Please type the death date of the person (or type "none" if person is alive): '), nl,
    read(TempDeath),
    (   TempDeath = 'none'
    ->  Death = 'none'
    ;   TempDeath > 2024
    ->  write('Error: Death date cannot be after 2024. Please enter a valid death date.'), nl,
        ask_valid_death_date(Death)
    ;   Death = TempDeath
    ).

% Check for a valid birth date
ask_valid_birth_date(Birthdate) :-
    write('Please type the birth date of the person: '), nl,
    read(TempBirth),
    (   TempBirth < 1700
    ;   TempBirth > 2024
    ->  write('Error: Birth date cannot be before 1700 and after 2024. Please enter a valid birth date.'), nl,
        ask_valid_birth_date(Birthdate)
    ;   Birthdate = TempBirth
    ).
choice_2(2) :- % Update Person
    write('1. Update the birth year of someone.'), nl,
    write('2. Update the death year of someone.'), nl,
    write('0. Cancel.'), nl,
    write('Enter your choice: '), read(Choice2),
    choice_2_2(Choice2).

% Update person
choice_2_2(0) :- % Cancel update
    write('Update cancelled.'), nl.


choice_2_2(1) :- % Birth date update
    write('Enter the name of the person you want to update: '), nl,
    read(UpdateName),
  ask_valid_birth_date(Birth),
    (retract(person(UpdateName, _, Death, Father, Mother, Children, Level, Gender, Spouse)) ->    %??
        assert(person(UpdateName, Birth, Death, Father, Mother, Children, Level, Gender, Spouse)),
        write('Birth year updated successfully.'), nl
    ; write('Person not found.'), nl).

choice_2_2(2) :- % Death date update
    write('Enter the name of the person you want to update: '), nl,
    read(UpdateName1),
    ask_valid_death_date(Death),
    
    (retract(person(UpdateName1, Birth, _, Father, Mother, Children, Level, Gender, Spouse)) ->      %??
        assert(person(UpdateName1, Birth,Death, Father, Mother, Children, Level, Gender, Spouse)),
        write('Death year updated successfully.'), nl
    ; write('Person not found.'), nl).

%-------------------------------------------------------------------------------------------------------------------------------------------------

choice(3) :- % Get person information
    write('Please type the person name and surname: '), nl,
    read(Name),
    get_information(Name).

choice(4) :- % Print family tree
    print_family_tree.

choice(5) :- % Add mariage
    write('Name of first person: '),
    read(FirstPerson),
    write('Name of second person: '),
    read(SecondPerson),
    add_marriage(FirstPerson, SecondPerson).

choice(6) :- % Terminate the program
    halt.

% For invalid choice
choice(_) :-
    %write('Invalid choice, please try again.'), nl,
    main.

% person(Name, Birth, Death, Father, Mother, Children, Level, Gender, Spouse)

% to add children
add_person(Person, Birthdate, Death, Father, Mother, Gender) :-
    ( person(Father,FatherBirthdate, DeathFather, _, _, _, LevelFather, _, _),
      person(Mother, MotherBirthdate, DeathMother, _, _, _, LevelMother, _, _)
    ->  % Check if the child's birthdate is after the death of the parents
        ( DeathFather \= 'none', Birthdate > DeathFather
        ->  write('Error: Child\'s birthdate is after the father\'s death date.'), nl, !, fail
        ;  true
        ),
        ( DeathMother \= 'none', Birthdate > DeathMother
        ->  write('Error: Child\'s birthdate is after the mother\'s death date.'), nl, !, fail
        ;  true
        ),
        (   Birthdate < FatherBirthdate
        ->  write('Error: Child\'s birthdate is before the father\'s birth date.'), nl, !, fail
        ;   true
        ),
        (   Birthdate < MotherBirthdate
        ->  write('Error: Child\'s birthdate is before the mother\'s birth date.'), nl, !, fail
        ;   true
        ),
         (   Death \= 'none',Death < Birthdate
        ->  write('Error: death date of person is before their birth date.'), nl, !, fail
        ;   true
        ),
        Level is max(LevelFather, LevelMother) + 1
    ;   Level is 0),

    assert(person(Person, Birthdate, Death, Father, Mother, [], Level, Gender, 'none')), % Children initially empty list
    assert_child(Father, Person), % add the new person to his/her father's children list
    assert_child(Mother, Person), % add the new person to his/her mother's children list

    write('Person added successfully.'), nl, nl.

  %To find the level of the newly added person in the add_person method
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.


% Add new person with marriage
add_new_person(Name) :-
    write('New person detected: '), write(Name), nl,
    write('Please provide the following information for '), write(Name), nl,
   ask_valid_birth_date(Birthdate), nl,
    ask_valid_death_date(Death), nl,
    
    (Death \= 'none' -> true; Death = 'none'), % Be sure Death is 'none' if not provided
    write('Gender (m/f): '), read(Gender), nl,

    Father = 'none', % Set Father to 'none'
    Mother = 'none', % Set Mother to 'none'

    assert(person(Name, Birthdate, Death, Father, Mother, [], 0, Gender, 'none')),
    (Father \= 'none' -> assert_child(Father, Name); true),
    (Mother \= 'none' -> assert_child(Mother, Name); true).


%Function for adding the child to the parent's child list
assert_child(Parent, Child) :-
    person(Parent, B, D, F, M, Children, L, G, S),
    retract(person(Parent, B, D, F, M, Children, L, G, S)),
    assert(person(Parent, B, D, F, M, [Child|Children], L, G, S)).

%First 2 person of the family is added here.
person('M', 1940, 2010, 'none3', 'none2', [], 0, m,'S').
person('S', 1942, 'none', 'none1', 'none4', [], 0, f,'M').
add_marriage('M','S').

% Utility to assert a child to a parent's children list
assert_child(Parent, Child) :-
    nonvar(Parent), % Check if Parent is instantiated
    retract(person(Parent, Birth, Death, Father, Mother, Children, Level, Gender, Spouse)),
    append(Children, [Child], NewChildren), % Add child to the end of the list
    assert(person(Parent, Birth, Death, Father, Mother, NewChildren, Level, Gender, Spouse)). % Update parent

% Add marriage function
add_marriage(FirstPerson, SecondPerson) :-
    % Check the persons are already in tree or not, if not add the persons
    (person(FirstPerson, Birth1, _, F1, M1, _, Level1, G1, Spouse1) ->
        true
    ;   add_new_person(FirstPerson),
        person(FirstPerson, Birth1, _, F1, M1, _, Level1, G1, Spouse1)
    ),
    (person(SecondPerson, Birth2, _, F2, M2, _, Level2, G2, Spouse2) ->
        true
    ;   add_new_person(SecondPerson),
        person(SecondPerson, Birth2, _, F2, M2, _, Level2, G2, Spouse2)
    ),

    % Check gender
    ((G1 = G2 ,F1='none',M1='none' ) -> (write('Same gender, marriage is not allowed!'),delete_person(FirstPerson), nl, !, fail);true ),
    ((G1 = G2 ,F2='none',M2='none' ) ->(write('Same gender, marriage is not allowed!'),delete_person(SecondPerson), nl, !, fail);true ) ,
     ((G1 \= G2  ) -> true; (write('Same gender, marriage is not allowed!'), nl, !, fail)),

    % Check marriage
    ((Spouse1 \= 'none',F2='none',M2='none') -> (write('Invalid operation. First Person is already married.'),delete_person(SecondPerson), nl, !, fail); true),
    ((Spouse2 \= 'none',F1='none',M1='none') -> (write('Invalid operation. Second Person is already married.'),delete_person(FirstPerson), nl, !, fail); true),
    ((Spouse1 \= none) -> (write('Invalid operation. Person is already married.'), nl, !, fail); true),
    ((Spouse2 \= none) -> (write('Invalid operation. Person is already married.'), nl, !, fail); true),
    
    Age1 is 2024 - Birth1, %Calculation of age
    Age2 is 2024 - Birth2,

    % Check age, if person is under 18 age, they can not marry
    ((Age1 < 18,F1='none',M1='none') -> (write('Under 18 age marriage!'),delete_person(FirstPerson), nl, !, fail); true),
    ((Age2 < 18,F1='none',M1='none') -> (write('Under 18 age marriage!'),delete_person(FirstPerson), nl, !, fail); true),
    ((Age1 < 18,F2='none',M2='none') -> (write('Under 18 age marriage!'),delete_person(SecondPerson), nl, !, fail); true),
    ((Age2 < 18,F2='none',M2='none') -> (write('Under 18 age marriage!'),delete_person(SecondPerson), nl, !, fail); true),
    ((Age1 < 18) -> (write('Under 18 age marriage!'), nl, !, fail); true),
    ((Age2 < 18) -> (write('Under 18 age marriage!'), nl, !, fail); true),

         % check relative marriage
        ((FirstPerson \= 'none', SecondPerson \= 'none',
      person(FirstPerson, _, _, Father1, Mother1, _, _, _, _),
      person(SecondPerson, _, _, Father2, Mother2, _, _, _, _),
      Father1 \= 'none', Mother1 \= 'none', Father2 \= 'none', Mother2 \= 'none') ->
        (is_invalid_marriage(FirstPerson, SecondPerson) ->
            (write('Invalid marriage: '), write(FirstPerson), write(' - '), write(SecondPerson), nl, ask_relation(FirstPerson, SecondPerson), nl, !, fail)
        ; true)
    ; true),

     % The newly added person's level must be the same as their spouse.
    (Level1 = 0 -> NewLevel1 = Level2; NewLevel1 = Level1),
    (Level2 = 0 -> NewLevel2 = Level1; NewLevel2 = Level2),

    % Add marriage relation
    assert(marriage(FirstPerson, SecondPerson)),
    retract(person(FirstPerson, Birth1, Death1, Father1, Mother1, Children1, Level1, G1, Spouse1)),
    retract(person(SecondPerson, Birth2, Death2, Father2, Mother2, Children2, Level2, G2, Spouse2)),
    assert(person(FirstPerson, Birth1, Death1, Father1, Mother1, Children1, NewLevel1, G1, SecondPerson)),
    assert(person(SecondPerson, Birth2, Death2, Father2, Mother2, Children2, NewLevel2, G2, FirstPerson)),
    write('Marriage added successfully.'), nl.
    
% Check invalid marriages
is_invalid_marriage(FirstPerson, SecondPerson) :-
    is_kizi(FirstPerson, SecondPerson);
    is_oglu(FirstPerson, SecondPerson);
    is_mother(FirstPerson, SecondPerson);
    is_father(FirstPerson, SecondPerson);
    is_amca(FirstPerson, SecondPerson);
    is_dayi(FirstPerson, SecondPerson);
    is_hala(FirstPerson, SecondPerson);
    is_teyze(FirstPerson, SecondPerson);
    is_nephew(FirstPerson, SecondPerson);
    is_niece(FirstPerson, SecondPerson);
    is_brother(FirstPerson, SecondPerson);
    is_sister(FirstPerson, SecondPerson);
    is_abi(FirstPerson, SecondPerson);
    is_abla(FirstPerson, SecondPerson);
    is_grandfather(FirstPerson, SecondPerson);
    is_grandmother(FirstPerson, SecondPerson);
    is_grandchild(FirstPerson, SecondPerson).

% Utility to find the children of a person
find_children(Parent, Children) :-
    person(Parent, _, _, _, _, Children, _, _, _).
    
    
%Print family tree
print_family_tree :-
    findall(Level-Person, person(Person, _, _, _, _, _, Level, _, _), Pairs),
    keysort(Pairs, SortedPairs),
    print_levels(SortedPairs, []).

% Print each level and its members
print_levels([], _).
print_levels([Level-Person | Rest], Visited) :-
    write_level(Level),
    print_members(Level, [Level-Person | Rest], Remaining, Visited, NewVisited),
    print_levels(Remaining, NewVisited).

% Write the level header
write_level(Level) :-
    format('--- Level ~w ---~n', [Level]).

% Print members of the same level
print_members(_, [], [], Visited, Visited).
print_members(Level, [Level-Person | Rest], Remaining, Visited, NewVisited) :-
    (member(Person, Visited) ->
        print_members(Level, Rest, Remaining, Visited, NewVisited)
    ;
        print_person_with_spouse(Person, Level, Visited, UpdatedVisited),
        nl,
        print_members(Level, Rest, Remaining, UpdatedVisited, NewVisited)
    ).
print_members(Level, [OtherLevel-Person | Rest], [OtherLevel-Person | Rest], Visited, Visited) :-
    OtherLevel \= Level.

% Print the person and their spouse
print_person_with_spouse(Person, Level, Visited, NewVisited) :-
    person(Person, _, _, _, _, _, Level, _, Spouse),
    (Spouse \= none ->
        (person(Spouse, _, _, _, _, _, SpouseLevel, _, _) ->
            (SpouseLevel \= Level ->
                % Different levels, print both separately with references
                format('~w (~w)', [Person, Spouse]),
                (member(Spouse, Visited) ->
                    UpdatedVisited = [Person | Visited]
                ;
                    UpdatedVisited = [Person | Visited],
                    append([Spouse], Visited, TempVisited)
                )
            ;
                % Same level, print together
                format('~w - ~w', [Person, Spouse]),
                append([Person, Spouse], Visited, UpdatedVisited)
            )
        ;
            % Spouse data not found, print both together
            format('~w - ~w', [Person, Spouse]),
            append([Person, Spouse], Visited, UpdatedVisited)
        )
    ;
        % No spouse, just print person
        format('~w', [Person]),
        UpdatedVisited = [Person | Visited]
    ),
    NewVisited = UpdatedVisited.
    
% Function for checking relation
ask_relation(First, Second) :-
    (is_husband(First, Second) ->
        write(First),write(' '), write(Second), write('(n)in kocasi '), nl
    ; is_wife(First, Second) ->
        write(First),write(' '), write(Second),write('(n)in karisi'), nl
    ; is_father(First, Second) ->
        write(First),write(' '),write(Second), write('(n)in babasi'), nl
    ; is_mother(First, Second) ->
        write(First),write(' '),write(Second), write('(n)in annesi'), nl
   ; is_kizi(First, Second) ->
        write(First), write(' '),write(Second),write('(n)in kizi'), nl
    ; is_oglu(First, Second) ->
        write(First),write(' '), write(Second), write('(n)in oglu'), nl
    ; is_abi(First, Second) ->
        write(First), write(' '), write(Second),write('(n)in abisi'), nl
    ; is_abla(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in ablasi'), nl
    ; is_brother(First, Second) ->
        write(First),write(' '),write(Second), write('(n)in erkek kardesi'),  nl
    ; is_sister(First, Second) ->
        write(First), write(' '),write(Second), write('(n)in kiz kardesi'),  nl
    ; is_teyze(First, Second) ->
        write(First),write(' '), write(Second), write('(n)in teyzesi'),  nl
    ; is_hala(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in halasi'), nl
    ; is_dayi(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in dayisi'), nl
    ; is_amca(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in amcasi'), nl
    ; is_father_in_law(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in kayinbabasi'), nl
    ; is_mother_in_law(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in kayinvalidesi'), nl
    ; is_nephew(First, Second) ->
        write(First), write(' '), write(Second),write('(n)in erkek yegeni'), nl
    ; is_niece(First, Second) ->
        write(First), write(' '), write(Second),write('(n)in kiz yegeni'), nl
    ; is_bacanak(First, Second) ->
        write(First), write(' '), write(Second),write('(n)in bacanagi'), nl
    ; is_grandfather(First, Second) ->
        write(First), write(' '),write(Second), write('(n)in dedesi'), nl
    ; is_grandmother(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in ninesi'), nl
    ; is_grandchild(First, Second) ->
        write(First), write(' '),write(Second), write('(n)in torunu'), nl
    ; is_gelin(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in gelini'), nl
    ; is_damat(First, Second) ->
        write(First), write(' '),write(Second), write('(n)in damadi'), nl
    ; is_kayinbirader(First, Second) ->
        write(First), write(' '),  write(Second), write('(n)in kayinbiraderi'), nl
    ; is_eniste(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in enistesi'), nl
    ; is_baldiz(First, Second) ->
        write(First), write(' '),write(Second), write('(n)in baldizi'), nl
    ; is_yenge(First, Second) ->
        write(First), write(' '), write(Second),write('(n)in yengesi'), nl
    ; is_elti(First, Second) ->
        write(First), write(' '), write(Second), write('(n)in eltisi'), nl
    ; is_cousin(First, Second) ->
        write(First), write(' '),write(Second), write('(n)in kuzeni'), nl
    ; write('No direct relationship found between '), write(First), write(' and '), write(Second), nl).
    
% Check husband
is_husband(Husband, Wife) :-
    person(Husband, _, _, _, _, _, _, m, Wife).

% Check wife
is_wife(Wife, Husband) :-
    person(Wife, _, _, _, _, _, _, f, Husband).

% Check father
is_father(Father, Child) :-
    person(Father, _, _, _, _, _, _, m, _),
    person(Child, _, _, Father, _, _, _, _, _).

% Check mother
is_mother(Mother, Child) :-
    person(Mother, _, _, _, _, _, _, f, _),
    person(Child, _, _, _, Mother, _, _, _, _).

% Check kiz
is_kizi(Child, Parent) :-
    person(Child, _, _, Father, Mother, _, _, f, _),
    (Parent = Father; Parent = Mother).

% Check ogul
is_oglu(Child, Parent) :-
    person(Child, _, _, Father, Mother, _, _, m, _),
    (Parent = Father; Parent = Mother).

% Check child
is_child(Child, Parent) :-
    person(Child, _, _, Father, Mother, _, _, _, _),
    (Parent = Father; Parent = Mother).

% Check brother
is_brother(Person1, Person2) :-
    person(Person1, Birth1, _, Father1, Mother1, _, _, m, _),
    person(Person2, Birth2, _, Father2, Mother2, _, _, _, _),
    Father1\='none',Father2\='none',Mother1\='none', Mother2\='none',
    Birth1>=Birth2,
    Father1 = Father2, Mother1 = Mother2, Person1 \= Person2.

% Check sister
is_sister(Person1, Person2) :-
    person(Person1, Birth1, _, Father1, Mother1, _, _, f, _),
    person(Person2, Birth2, _, Father2, Mother2, _, _, _, _),
    Father1\='none',Father2\='none',Mother1\='none', Mother2\='none',
    Birth1>=Birth2,
    Father1 = Father2, Mother1 = Mother2, Person1 \= Person2.

% Check amca
is_amca(Brother, Child) :-  % Yeni
    person(Brother, _, _, _, _, _, _, m, _),
    is_father(Father, Child),
    (is_brother(Brother, Father);is_abi(Brother, Father)).

% Check teyze
is_teyze(Sister,Child) :-  % yeni
    person(Sister, _, _, _, _, _, _, f, _),
    is_mother(Mother, Child),
    (is_sister(Sister, Mother);is_abla(Sister, Mother)).

% Check dayi
is_dayi(Brother, Child) :- % yeni
    person(Brother, _, _, _, _, _, _, m, _),
    is_mother(Mother, Child),
    (is_brother(Brother, Mother); is_abi(Brother, Mother)).

% Check hala
is_hala(Sister, Child) :- % yeni
    person(Sister, _, _, _, _, _, _, f, _),
    is_father(Father, Child),
    (is_sister(Sister, Father);is_abla(Sister, Father)).

% Check father in law
is_father_in_law(FatherInLaw, Person) :-
    (is_husband(Person, Spouse); is_wife(Person, Spouse)),
    is_father(FatherInLaw, Spouse).

% Check mother in law
is_mother_in_law(MotherInLaw, Person) :-
    (is_husband(Person, Spouse); is_wife(Person, Spouse)),
    is_mother(MotherInLaw, Spouse).

% Check nephew
is_nephew(Nephew, Person) :-
    person(Nephew, _, _, _, _, _, _, m, _),
    (is_brother(Sibling, Person); is_sister(Sibling, Person);is_abi(Sibling, Person);is_abla(Sibling, Person)),
    is_child(Nephew, Sibling).

% Check niece
is_niece(Niece, Person) :-
    person(Niece, _, _, _, _, _, _, f, _),
    (is_brother(Sibling, Person); is_sister(Sibling, Person);is_abi(Sibling, Person);is_abla(Sibling, Person)),
    is_child(Niece, Sibling).

% Check bacanak
is_bacanak(Husband1, Husband2) :- %yeni
    person(Husband1, _, _, _, _, _, _, m, _),
    person(Husband2, _, _, _, _, _, _, m, _),
    is_husband(Husband1, Wife1),
    is_husband(Husband2, Wife2),
    (is_sister(Wife1, Wife2); is_abla(Wife1, Wife2)).

% Check grandfather
is_grandfather(Grandfather, Grandchild) :-
    (is_father(Father, Grandchild),
        is_father(Grandfather, Father);
        is_mother(Mother, Grandchild),
        is_father(Grandfather, Mother) ).

% Check grandmother
is_grandmother(Grandmother, Grandchild) :-
    (is_mother(Mother, Grandchild),
        is_mother(Grandmother, Mother);
        is_father(Father, Grandchild),
        is_mother(Grandmother, Father)).

% Check grandchild
is_grandchild(Grandchild, Grandparent) :-
    (is_grandfather(Grandparent, Grandchild); is_grandmother(Grandparent, Grandchild)).

% Check abla
is_abla(ElderSister, Sibling) :-
     person(ElderSister, Birth1, _, Father1, Mother1, _, _, f, _),
    person(Sibling, Birth2, _, Father2, Mother2, _, _, _, _),
    Father1\='none',Father2\='none',Mother1\='none', Mother2\='none',
    Birth1<Birth2,
    ElderSister \= Sibling,
    Father1 = Father2, Mother1 = Mother2.

% Check abi
is_abi(ElderBrother, Sibling) :-
      person(ElderBrother, Birth1, _, Father1, Mother1, _, _, m, _),
    person(Sibling, Birth2, _, Father2, Mother2, _, _, _, _),
    Father1\='none',Father2\='none',Mother1\='none', Mother2\='none',
    Birth1<Birth2,
    ElderBrother \= Sibling,
    Father1 = Father2, Mother1 = Mother2.

% Check eniste
is_eniste(Eniste, Person) :-
    is_sister(Sister, Person),
    is_husband(Eniste, Sister)
    ; is_abla(Sister, Person),
    is_husband(Eniste, Sister)
    ;is_hala(Sister, Person),
    is_husband(Eniste, Sister)
    ;is_teyze(Sister, Person),
    is_husband(Eniste, Sister) .

 % Check gelin
is_gelin(Gelin, Parent) :-
    (is_husband(Person, Gelin),
        (is_mother(Parent, Person); is_father(Parent, Person))).

% Check damat
is_damat(Damat,Parent) :-
    (is_wife(Person, Damat),
        (is_mother(Parent, Person); is_father(Parent, Person))).

% Check yenge
is_yenge(Yenge, Person) :-
    is_brother(Brother, Person),
    is_wife(Yenge,Brother)
    ; is_abi(Brother, Person) ,
    is_wife(Yenge,Brother)
    ;is_dayi(Brother, Person),
    is_wife(Yenge,Brother)
    ;is_amca(Brother, Person),
    is_wife(Yenge,Brother) .

% Check elti
is_elti(Person1, Person2) :-
    is_husband(Husband1 , Person1),
    is_husband(Husband2 ,Person2 ),
    (is_abi(Husband1,Husband2); is_brother(Husband1, Husband2)),
     Husband1 \= Husband2.

% Check baldiz
is_baldiz(Baldiz, Person) :-
  person(Baldiz, _, _, _, _, _, _, f, _),
   person(Person, _, _, _, _, _, _, m, _),
  is_husband(Person,Wife),
 (is_sister(Baldiz, Wife); is_abla(Baldiz, Wife)).

% Check cousin
is_cousin(Person1, Person2) :-
    is_child(Person1, Parent1),
    is_child(Person2, Parent2),
    (is_brother(Parent1, Parent2); is_sister(Parent1, Parent2);is_abi(Parent1, Parent2);is_abla(Parent1, Parent2)),
     Parent1 \= Parent2.

% Check kayinbirader
is_kayinbirader(Person1, Person2) :-
     person(Person1, _, _, _, _, _, _, m, _),
    (is_wife(Wife,Person2),
    (is_sister(Wife ,Person1); is_abla(Wife, Person1)));
    (is_husband(Husband ,Person2),
    (is_brother(Husband ,Person1); is_abi(Husband, Person1))) .

% Get person's information
get_information(Name) :-
    (person(Name, Birth, Death, Father, Mother, Children, Level, Gender, Spouse) ->
        write('Name: '), write(Name), nl,
        write('Birth year: '), write(Birth), nl,
        (   Death == 'none' ->
            CurrentYear = 2024,
            Age is CurrentYear - Birth,
            write('Status: Alive'), nl,
            write('Age: '), write(Age), nl
        ;   write('Death year: '), write(Death), nl,
            (   Death == 'none' ->
                Age is 2024 - Birth
            ;   Age is Death - Birth
            ),
            write('Age: '), write(Age), nl,
            write('Status: Dead'), nl
        ),
        (   Children = [] ->
            ChildCount = 0
        ;   length(Children, ChildCount)
        ),
        write('Children: '), write(ChildCount), nl,
        write('Level: '), write(Level), nl,
        write('Gender: '), write(Gender), nl
    ; write('Person not found.'), nl).

 delete_person(PersonName) :-   %delete person for invalid marriage
    retractall(person(PersonName, _, _, _, _, _, _, _, _)),
    retractall(parent(PersonName, _)),
    retractall(parent(_, PersonName)),
    retractall(married(PersonName, _)),
    retractall(married(_, PersonName)).

