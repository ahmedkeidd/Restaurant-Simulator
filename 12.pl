:-consult('public_kb.pl').
check_staff(Day,Time,Reservations):-
	staff(Day,StaffCount), 
	res_count(Day,Time,Reservations,NumReservations),
	(NumReservations)=<(StaffCount).
	
res_count(_,_,[],0).

res_count(Day,Time,[res(Day,Time,_,_)|T],Count):-
	!,
    res_count(Day,Time,T,RestCount),
    Count is (RestCount + 1).

res_count(Day,Time,[res(Wrongday,Wrongtime,_,_)|T],Count):-
    (Wrongday\=Day ; Wrongtime\=Time),
    res_count(Day,Time,T,Count).

	
	
	
	
	
schedule_all_reservations(Days, Schedule) :-
    findall(Name,group(Name,_,_),AllGroups),
    process_all(AllGroups, Days,[],Schedule),
    verify_staff_limits(Days,Schedule).

get_room(Id,Cap) :- 
    tables(Ts), 
    member(t(Id,Cap), Ts).

sufficient_space(Needed, Available) :- 
    (Available) >= (Needed).

is_busy(D,T,R,[res(D,T,_, R)|_]):- !.

is_busy(D, T, R, [_|Tail]) :- is_busy(D,T,R,Tail).

process_all([], _, S, S).

process_all([G|Gs], Dates, Acc, Out) :-
    group(G, S, Time),
    member(D, Dates),
    get_room(Tbl, C),
    sufficient_space(S, C),
    \+ is_busy(D, Time, Tbl, Acc),
    process_all(Gs, Dates, [res(D, Time, G, Tbl)|Acc], Out).

verify_staff_limits([], _).

verify_staff_limits([Day|Rest], Sched) :-
    check_staff(Day, morning, Sched),
    check_staff(Day, evening, Sched),
    verify_staff_limits(Rest, Sched).	


	
	

	
	
group_ingredients(GroupName,Ingredients):-
    order(GroupName,Dishes),
    ingredient_getter(Dishes,Ingredients).
	
ingredient_getter([],[]).

ingredient_getter([Dish|Rest],Ingredients):-
    recipe(Dish,Ing),
    ingredient_getter(Rest,RestIng),
    add_list(Ing,RestIng,Ingredients).

add_list([],L,L).

add_list([H|T],L,[H|R]):-
    add_list(T,L,R).

	
	
	
	
	
	
needed_ingredients(Reservations, AllIngredients) :-
    build_pairs(Reservations, Pairs),
    merge_days(Pairs, AllIngredients).

build_pairs([],[]).

build_pairs([res(Day,_,Group,_)| T], [(Day,Ing)| R]) :-
    group_ingredients(Group, Ing),
    build_pairs(T, R).

merge_days([],[]).

merge_days([(Day,IngList)| Rest], FinalList) :-
    merge_days(Rest,ProcessedRest),
    combine_day_ings(Day,IngList,ProcessedRest,FinalList).

combine_day_ings(Day,Ings,[],[(Day, Ings)]).

combine_day_ings(Day,Ings,[(Day,ExistingIngs)| T], [(Day,AllIngs) | T]) :-
    !, 
    add_list(Ings,ExistingIngs,AllIngs).
	%add_list_we_defined_it_in_the_previous_predicate
	
combine_day_ings(Day,Ings,[(OtherDay,OtherIngs)| T], [(OtherDay,OtherIngs)| R]) :-
    combine_day_ings(Day,Ings,T,R).

	
	
	
	
	
	
write_reservations_to_csv(Filename, Schedule) :-
    open(Filename,write,Bridge),
    write(Bridge,'Day,Month,Time,Group,Table'),
    nl(Bridge),
    write_reservations(Bridge, Schedule),
    close(Bridge).

write_reservations(_, []).

write_reservations(Bridge, [res(day(D,M), Time, Group, Table)| T]) :-
    write(Bridge,D),
    write(Bridge,','),
    write(Bridge,M),
    write(Bridge,','),
    write(Bridge,Time),
    write(Bridge,','),
    write(Bridge,Group),
    write(Bridge,','),
    write(Bridge,Table),
    nl(Bridge),
    write_reservations(Bridge, T).

	
	
	
	
	
	
write_ingredients_to_csv(Filename, AllIngredients) :-
    open(Filename,write,Bridge),
    write(Bridge,'Day,Month,Ingredients'),
    nl(Bridge),
    write_ingredient_rows(Bridge, AllIngredients),
    close(Bridge).

write_ingredient_rows(_,[]).

write_ingredient_rows(Bridge,[(day(D,M),Ingredients) |T]) :-
    write(Bridge,D),
    write(Bridge,','),
    write(Bridge,M),
    write(Bridge,','),
    write_ingredients_list(Bridge,Ingredients),
    nl(Bridge),
    write_ingredient_rows(Bridge, T).

write_ingredients_list(Bridge,[H]) :-
    write(Bridge, H).
	
write_ingredients_list(Bridge,[H|T]) :-
    write(Bridge, H),
    write(Bridge, ';'),
    write_ingredients_list(Bridge, T).