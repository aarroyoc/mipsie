:- use_module(library(assoc)).

:- object(test_machine, extends(lgtunit)).

test(simple_addi, true(X == 50)) :-
    Code = [addi('$t0', '$zero', 50)],
    initial_state(State),
    machine::execute(Code, State, EndState),
    EndState = mips_state(R, _, _, _),
    registers::value(R, '$t0', X).

test(multi_add, true(X == 50)) :-
    Code = [addi('$t0', '$zero', 25), add('$t0', '$t0', '$t0')],
    initial_state(State),
    machine::execute(Code, State, EndState),
    EndState = mips_state(R, _, _, _),
    registers::value(R, '$t0', X).

% TODO: Add tests for all instructions

initial_state(mips_state(R, 0, M, LS)) :-
    initial_registers(R),
    assoc:empty_assoc(M),
    assoc:empty_assoc(LS).

initial_registers(R) :-
    R = r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

:- end_object.