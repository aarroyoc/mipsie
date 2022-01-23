:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).


run_mips(Filename, EndMipsState) :-
    parse(Filename, MipsCode, MipsState),
    run_all(MipsCode, MipsState, EndMipsState).

parse(Filename, MipsCode, MipsState) :-
    phrase_from_file(mips_asm(MipsCode, MipsState), Filename).

mips_asm([Instruction|MipsCode], MipsState) -->
    mips_instruction(Instruction),
    mips_asm(MipsCode, MipsState).

mips_asm([], mips_state(R, 0)) -->
    {
        register_value(R, '$zero', _)
    },
    [].

mips_instruction(add(Rd, Rs, Rt)) -->
    "\tadd",
    whites,
    register(Rd),
    comma,
    register(Rs),
    comma,
    register(Rt),
    end_line.

mips_instruction(addi(Rt, Rs, I)) -->
    "\taddi",
    whites,
    register(Rt),
    comma,
    register(Rs),
    comma,
    number(I),
    end_line.

number(D) -->
    number_(Ds),
    {
        number_chars(D, Ds)
    }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D]) --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

comma -->
    ( whites | []),
    ",",
    ( whites | []).

white_char -->
    [X],
    {
        member(X, " \t")
    }.
whites --> white_char.
whites -->
       white_char,
       whites.

end_line --> "\n".
end_line --> whites, end_line.

register('$zero') --> "$zero".
register('$at') --> "$at".
register('$v0') --> "$v0".
register('$v1') --> "$v1".
register('$a0') --> "$a0".
register('$a1') --> "$a1".
register('$a2') --> "$a2".
register('$a3') --> "$a3".
register('$t0') --> "$t0".
register('$t1') --> "$t1".
register('$t2') --> "$t2".
register('$t3') --> "$t3".
register('$t4') --> "$t4".
register('$t5') --> "$t5".
register('$t6') --> "$t6".
register('$t7') --> "$t7".
register('$s0') --> "$s0".
register('$s1') --> "$s1".
register('$s2') --> "$s2".
register('$s3') --> "$s3".
register('$s4') --> "$s4".
register('$s5') --> "$s5".
register('$s6') --> "$s6".
register('$s7') --> "$s7".
register('$t8') --> "$t8".
register('$t9') --> "$t9".
register('$k0') --> "$k0".
register('$k1') --> "$k1".
register('$gp') --> "$gp".
register('$sp') --> "$sp".
register('$fp') --> "$fp".
register('$ra') --> "$ra".

run_all(Code, mips_state(R, PC), EndState) :-
    nth0(PC, Code, Instruction),
    execute(Instruction, mips_state(R, PC), NewState),
    run_all(Code, NewState, EndState).

run_all(Code, mips_state(R, PC), mips_state(R, PC)) :-
    length(Code, N),
    PC >= N.

run(Code, State) :-
    register_value(R0, '$zero', _),
    Code = [Instruction|_],
    execute(Instruction, mips_state(R0, 0), State).

execute(add(Rd, Rs, Rt), mips_state(R0, PC0), mips_state(R, PC)) :-
    register_value(R0, Rs, ValRs),
    register_value(R0, Rt, ValRt),
    ValRd is ValRs + ValRt,
    PC is PC0 + 1,
    registers_set(R0, R, Rd, ValRd).

execute(addi(Rt, Rs, I), mips_state(R0, PC0), mips_state(R, PC)) :-
    register_value(R0, Rs, ValRs),
    ValRt is ValRs + I,
    PC is PC0 + 1,
    registers_set(R0, R, Rt, ValRt).

registers_set(R0, R, Rd, ValRd) :-
    findall(Reg, register(Reg), Rs0),
    select(Rd, Rs0, Rs),
    maplist(register_unify(R0, R), Rs),
    register_value(R, Rd, ValRd).

register_unify(R0, R, Reg) :-
    register_value(R0, Reg, X),
    register_value(R, Reg, X).

%?- test_add.
test_add :-
    R0 = r(_,_,1,_,_,_,3,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_),
    execute(add('$t9', '$v1', '$a3'), mips_state(R0, 0), mips_state(R, 1)),
    R = r(_,_,1,_,_,_,3,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,4,_,_,_,_,_,_).

%?- test_sum.
test_sum :-
    run_mips("sum.s", mips_state(R, 3)),
    register_value(R, '$t0', 350),
    register_value(R, '$t1', 250).

% registers
register(X) :- register_value(_, X, _).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$zero', 0).
register_value(r(X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$at', X).
register_value(r(_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$v0', X).
register_value(r(_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$v1', X).
register_value(r(_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$a0', X).
register_value(r(_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$a1', X).
register_value(r(_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$a2', X).
register_value(r(_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$a3', X).
register_value(r(_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t0', X).
register_value(r(_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t1', X).
register_value(r(_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t2', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t3', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t4', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t5', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t6', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$t7', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$s0', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_,_), '$s1', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_,_), '$s2', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_,_), '$s3', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_,_), '$s4', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_,_), '$s5', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_,_), '$s6', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_,_), '$s7', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_,_), '$t8', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_,_), '$t9', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_,_), '$k0', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_,_), '$k1', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_,_), '$gp', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_,_), '$sp', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X,_), '$fp', X).
register_value(r(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,X), '$ra', X).
