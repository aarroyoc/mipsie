:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(reif)).
:- use_module(library(assoc)).

run_mips(Filename, EndMipsState) :-
    parse(Filename, MipsCode, MipsState),
    run_all(MipsCode, MipsState, EndMipsState).

parse(Filename, MipsCode, MipsState) :-
    phrase_from_file(lines(MipsCodeLines), Filename),
    maplist(no_comment_line, MipsCodeLines, MipsCodeLinesClean),
    filter_empty_lines(MipsCodeLinesClean, MipsLines),
    mips_asm(MipsLines, MipsCode, MipsState).

lines([]) --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([]) --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

eos([], []).


no_comment_line(L0, L) :-
    member(#, L0),
    phrase((seq(L), "#"), L0, _).

no_comment_line(L, L) :-
    \+ member(#, L).

filter_empty_lines([], []).
filter_empty_lines([L|Ls0], Ls) :-
    L = "",
    filter_empty_lines(Ls0, Ls).
filter_empty_lines([L|Ls0], [L|Ls]) :-
    L \= "",
    filter_empty_lines(Ls0, Ls).

mips_asm(Lines, Code, mips_state(R, PC, M, LS)) :-
    extract_data_lines(Lines, DataLines),
    mips_data(DataLines, mips_state(R, PC, M, LS0)),
    extract_text_lines(Lines, TextLines),
    mips_label_text(TextLines, Labels),
    merge_assoc(LS0, Labels, LS),
    mips_text(TextLines, Code).

merge_assoc(LS0, LS1, LS) :-
    assoc_to_list(LS0, LS0s),
    assoc_to_list(LS1, LS1s),
    append(LS0s, LS1s, LSs),
    list_to_assoc(LSs, LS).

extract_data_lines([], []).
extract_data_lines([L|Lines], DataLines) :-
    (
	phrase((... , ".data", ... ), L) ->
	extract_data_lines_end(Lines, DataLines)
    ;	extract_data_lines(Lines, DataLines)
    ).

extract_data_lines_end([], []).
extract_data_lines_end([L|Lines], DataLines) :-
    (
	phrase((... , ".text", ... ), L) ->
	DataLines = []
    ;	(DataLines = [L|Ds], extract_data_lines_end(Lines, Ds))
    ).


extract_text_lines([], []).
extract_text_lines([L|Lines], TextLines) :-
    (
	phrase((... , ".text", ... ), L) ->
	Lines = TextLines
    ;	extract_text_lines(Lines, TextLines)
    ).

mips_data([], State) :- default_mips_state(State).
mips_data([L|Ls], State) :-
    phrase(mips_data_declaration(Declaration), L),
    mips_data(Ls, State0),
    add_declaration_state(Declaration, State0, State).

add_declaration_state(asciiz(Label, String), mips_state(R, PC, M0, L0), mips_state(R, PC, M, L)) :-
    append(M0, String, M1),
    append(M1, "\x0\", M),
    length(M0, Addr),
    put_assoc(Label, L0, Addr, L).

% mips_state is defined as
%    Registers
%    PC
%    Memory
%    Label-Memory
default_mips_state(mips_state(R, 0, [], LabelStore)) :-
    register_value(R, '$zero', _),
    empty_assoc(LabelStore).

mips_data_declaration(asciiz(Label, String)) -->
    seq(Label),
    ":",
    whites,
    ".asciiz",
    whites,
    "\"",
    escaped_string(String),
    "\"",
    end_line.

escaped_string([]) --> [].
escaped_string([X|Str]) -->
    [X],
    {
        X \= (\)
    },
    escaped_string(Str).
escaped_string(['\n'|Str]) -->
    "\\n",
    escaped_string(Str).

mips_label_text(TextLines, LS) :-
    mips_label_text(TextLines, 0, LS).
mips_label_text([], _, LS) :-
    empty_assoc(LS).
mips_label_text([Line|TextLines], N, LS) :-
    N1 is N + 1,
    mips_label_text(TextLines, N1, LS0),
    (
	phrase((seq(Label), ":", end_line), Line) ->
	PC is N + 1
    ;	phrase((seq(Label), ":", ... ), Line) -> PC = N ; true
    ),
    (\+ var(PC) -> put_assoc(Label, LS0, PC, LS) ; true).

mips_text([], []).
mips_text([Line|TextLines], [Instruction|Code]) :-
    phrase(mips_instruction(Instruction), Line),
    mips_text(TextLines, Code).

mips_instruction(add(Rd, Rs, Rt)) -->
    optional_label,
    "add",
    whites,
    register(Rd),
    comma,
    register(Rs),
    comma,
    register(Rt),
    end_line.

mips_instruction(addi(Rt, Rs, I)) -->
    optional_label,
    "addi",
    whites,
    register(Rt),
    comma,
    register(Rs),
    comma,
    number(I),
    end_line.

mips_instruction(and(Rd, Rs, Rt)) -->
    optional_label,
    "and",
    whites,
    register(Rd),
    comma,
    register(Rs),
    comma,
    register(Rt),
    end_line.

mips_instruction(andi(Rt, Rs, I)) -->
    optional_label,
    "andi",
    whites,
    register(Rt),
    comma,
    register(Rs),
    comma,
    number(I),
    end_line.

mips_instruction(beq(Rs, Rt, Label)) -->
    optional_label,
    "beq",
    whites,
    register(Rs),
    comma,
    register(Rt),
    comma,
    seq(Label),
    end_line.

mips_instruction(bne(Rs, Rt, Label)) -->
    optional_label,
    "bne",
    whites,
    register(Rs),
    comma,
    register(Rt),
    comma,
    seq(Label),
    end_line.

mips_instruction(j(Label)) -->
    optional_label,
    "j",
    whites,
    seq(Label),
    end_line.

mips_instruction(li(Rd, I)) -->
    optional_label,
    "li",
    whites,
    register(Rd),
    comma,
    number(I),
    end_line.

mips_instruction(la(Rd, Label)) -->
    optional_label,
    "la",
    whites,
    register(Rd),
    comma,
    seq(Label),
    end_line.

mips_instruction(syscall) -->
    optional_label,
    "syscall",
    end_line.

number(D) -->
    number_(Ds),
    {
        number_chars(D, Ds)
    }.

number_([D|Ds]) --> digit(D), number_(Ds).
number_([D]) --> digit(D).

digit(D) --> [D], { char_type(D, decimal_digit) }.

optional_label --> "\t".
optional_label --> ..., ":", whites.

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

end_line --> ( whites | []).

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

run_all(Code, mips_state(R, PC, Memory, LabelStore), EndState) :-
    nth0(PC, Code, Instruction),
    execute(Instruction, mips_state(R, PC, Memory, LabelStore), NewState),
    run_all(Code, NewState, EndState).

run_all(Code, mips_state(R, PC, M, LS), mips_state(R, PC, M, LS)) :-
    length(Code, N),
    PC >= N.

run(Code, State) :-
    register_value(R0, '$zero', _),
    Code = [Instruction|_],
    execute(Instruction, mips_state(R0, 0, _, _), State).

execute(add(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    register_value(R0, Rt, ValRt),
    ValRd is ValRs + ValRt,
    PC is PC0 + 1,
    registers_set(R0, R, Rd, ValRd).

execute(addi(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    ValRt is ValRs + I,
    PC is PC0 + 1,
    registers_set(R0, R, Rt, ValRt).

execute(and(Rd, Rs, Rt), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    register_value(R0, Rt, ValRt),
    ValRd is ValRs /\ ValRt,
    PC is PC0 + 1,
    registers_set(R0, R, Rd, ValRd).

execute(andi(Rt, Rs, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R0, Rs, ValRs),
    ValRt is I /\ ValRs,
    PC is PC0 + 1,
    registers_set(R0, R, Rt, ValRt).

% in real MIPS branch instructions use relative addresses, but most assemblers work with labels too,
% so the end result is the same as with jumps (which use absolute addresses)
execute(beq(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    register_value(R, Rt, ValRt),
    (
	ValRs = ValRt ->
	get_assoc(Label, LS, PC)
    ;   PC is PC0 +1
    ).

execute(bne(Rs, Rt, Label), mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, Rs, ValRs),
    register_value(R, Rt, ValRt),
    (
	ValRs \= ValRt ->
	get_assoc(Label, LS, PC)
    ;   PC is PC0 +1
    ).

execute(j(Label), mips_state(R, _, M, LS), mips_state(R, PC, M, LS)) :-
    get_assoc(Label, LS, PC).

execute(li(Rd, I), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    registers_set(R0, R, Rd, I),
    PC is PC0 + 1.

execute(la(Rd, Label), mips_state(R0, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    get_assoc(Label, LS, Addr),
    registers_set(R0, R, Rd, Addr),
    PC is PC0 + 1.

% syscall - print asciiz
execute(syscall, mips_state(R, PC0, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, '$v0', 4),
    register_value(R, '$a0', Addr),
    print_asciiz(Addr, M),
    PC is PC0 + 1.

% syscall - halt
execute(syscall, mips_state(R, PC, M, LS), mips_state(R, PC, M, LS)) :-
    register_value(R, '$v0', 10),
    halt.

print_asciiz(Addr, M) :-
    nth0(Addr, M, '\x0\').
print_asciiz(Addr, M) :-
    nth0(Addr, M, C),
    format("~a", [C]),
    AddrNext is Addr + 1,
    print_asciiz(AddrNext, M).

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
    run_mips("sum.s", mips_state(R, 3, _, _)),
    register_value(R, '$t0', 350),
    register_value(R, '$t1', 250).

test_hello :-
    run_mips("hello.s", _).

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
