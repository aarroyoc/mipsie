:- object(test_registers, extends(lgtunit)).

test(register_value_zero, true(X == 0)) :-
    initial_registers(R),
    registers::value(R, '$zero', X).

initial_registers(R) :-
    R = r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

:- end_object.
