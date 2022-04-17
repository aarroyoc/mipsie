:- object(test_parser, extends(lgtunit)).

test(register_value_zero, true(X == 0)) :-
    initial_registers(R),
    registers::value(R, '$zero', X).

test(register_set_and_read, true(X == 15)) :-
    initial_registers(R),
    registers::set(R, R1, '$t0', 15),
    registers::value(R1, '$t0', X).

test(register_set_set_and_read, true(X == 30)) :-
    initial_registers(R),
    registers::set(R, R1, '$s0', 15),
    registers::set(R1, R2, '$t0', 16),
    registers::set(R2, R3, '$s0', 30),
    registers::value(R3, '$s0', X).

initial_registers(R) :-
    R = r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

:- end_object.
