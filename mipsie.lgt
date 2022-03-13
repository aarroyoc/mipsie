:- object(mipsie).

    :- public(run/1).
    run(Filename) :-
        parser::parse(Filename, Code, State),
	machine::execute(Code, State, _).

:- end_object.